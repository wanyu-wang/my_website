---
categories:
- ""
- ""
date: "2017-10-31T22:26:09-05:00"
description: My work in data analysis
draft: false
image: pic09.jpg
keywords: R
slug: magna
title: Winnie's Work
---
Hi this is my work.


# GDP components over time and among countries

At the risk of oversimplifying things, the main components of gross domestic product, GDP are personal consumption (C), business investment (I), government spending (G) and net exports (exports - imports). You can read more about GDP and the different approaches in calculating at the [Wikipedia GDP page](https://en.wikipedia.org/wiki/Gross_domestic_product).

```{r read_GDP_data}

UN_GDP_data  <-  read_excel(here::here("data", "Download-GDPconstant-USD-countries.xls"), # Excel filename
                sheet="Download-GDPconstant-USD-countr", # Sheet name
                skip=2) # Number of rows to skip

```

The first thing you need to do is to tidy the data, as it is in wide format and you must make it into long, tidy format. Please express all figures in billions (divide values by `1e9`, or $10^9$), and you want to rename the indicators into something shorter.

```{r reshape_GDP_data}
library(reshape2)

tidy_GDP_data  <-  UN_GDP_data %>% 
  pivot_longer(cols=c("1970":"2017"), names_to="Year", values_to="value") %>% 
  mutate(value_b = value/10**9)
  
glimpse(tidy_GDP_data)

# Let us compare GDP components for these 3 countries
country_list <- c("United States", "India", "Germany")

tidy_GDP_data_1  <-  tidy_GDP_data %>% 
  filter(Country %in% country_list) %>% 
  filter(IndicatorName %in% c("Gross capital formation", "Exports of goods and services", "General government final consumption expenditure", "Household consumption expenditure (including Non-profit institutions serving households)", "Imports of goods and services")) %>% 
  mutate(IndicatorName_s = ifelse(IndicatorName == "Gross capital formation", "Gross capital formation",
                                  ifelse(IndicatorName == "Exports of goods and services", "Exports", 
                                         ifelse(IndicatorName == "Imports of goods and services", "Imports",
                                                ifelse(IndicatorName == "General government final consumption expenditure", "Govenment expenditure",
                                                       ifelse(IndicatorName == "Household consumption expenditure (including Non-profit institutions serving households)", "Household expenditure", "NA"))))))
```

First, can you produce this plot?

```{r gdp1, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "gdp1.png"), error = FALSE)
```

```{r}
ggplot(tidy_GDP_data_1, aes(x = Year, y = value_b, color = IndicatorName_s, group = IndicatorName_s))+
  geom_line()+
  facet_wrap(~Country)+
  theme_bw()+
  labs(title = "GDP components over time",
       subtitle="In constant 2010 USD",
       x= "",
       y="Billion US$", 
       colour = "Components of GDP")+
  scale_x_discrete(breaks=seq(1900, 2010, 10))
  
```


Secondly, recall that GDP is the sum of Household Expenditure (Consumption *C*), Gross Capital Formation (business investment *I*), Government Expenditure (G) and Net Exports (exports - imports). Even though there is an indicator `Gross Domestic Product (GDP)` in your dataframe, I would like you to calculate it given its components discussed above.

```{r, cal_GDP_manually}
tidy_GDP_data_2  <-  tidy_GDP_data %>% 
  filter(Country %in% country_list) %>% 
  filter(IndicatorName %in% c("Gross capital formation", "Exports of goods and services", "General government final consumption expenditure", "Household consumption expenditure (including Non-profit institutions serving households)", "Imports of goods and services", "Gross Domestic Product (GDP)")) %>% 
  mutate(IndicatorName_s = ifelse(IndicatorName == "Gross capital formation", "Gross capital formation",
                                  ifelse(IndicatorName == "Exports of goods and services", "Exports", 
                                         ifelse(IndicatorName == "Imports of goods and services", "Imports",
                                                ifelse(IndicatorName == "General government final consumption expenditure", "Govenment expenditure",
                                                       ifelse(IndicatorName == "Household consumption expenditure (including Non-profit institutions serving households)", "Household expenditure", 
                                                              ifelse(IndicatorName == "Gross Domestic Product (GDP)", "GDP", "NA")))))))


GDP_compare <- tidy_GDP_data_2 %>% 
  select(!c(value, IndicatorName)) %>% 
  pivot_wider(id_cols = c(CountryID, Country, Year), names_from = IndicatorName_s, values_from = value_b) %>% 
  mutate(net_export = Exports-Imports,
         cal_GDP = `Household expenditure`+`Govenment expenditure`+`Gross capital formation`+net_export,
         diff = cal_GDP - GDP,
         percent_diff = (cal_GDP - GDP)/GDP,
         up = ifelse(diff > 0, diff, 0),
         down = ifelse(diff < 0, -diff, 0))
```

```{r}
# absolute difference between cal_GDP and GDP
ggplot(GDP_compare) +
  geom_line(aes(x = Year, y = GDP, group = Country), color = "red")+
  geom_line(aes(x = Year, y = cal_GDP, group = Country), color = "blue")+
  geom_ribbon(aes(x=Year,ymin=GDP,ymax=cal_GDP,group=Country),fill="#CB454A",alpha=0.4)+
  facet_wrap(~Country)+
  theme_bw()+
  labs(title = "Different in GDP and calculated GDP",
       subtitle="In constant 2010 USD",
       x= "",
       y="Billion US$")+
  scale_x_discrete(breaks=seq(1900, 2010, 10))

# percentage difference between cal_GDP and GDP
ggplot(GDP_compare)+
  geom_line(aes(x = Year, y = percent_diff, group = Country))+
  geom_hline(yintercept=0,linetype="solid",color="orange", size = 1)+
  facet_wrap(~Country)+
  theme_bw()+
  labs(title = "Percentage different in GDP and calculated GDP",
       subtitle="In constant 2010 USD",
       x= "",
       y="%")+
  scale_x_discrete(breaks=seq(1900, 2010, 10))
```

> What is the % difference between what you calculated as GDP and the GDP figure included in the dataframe?

```{r gdp2, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "gdp2.png"), error = FALSE)
```
```{r}
proportion <- GDP_compare %>% 
  mutate(`Govenment expenditure` = `Govenment expenditure`/GDP,
         `Gross capital formation` = `Gross capital formation`/GDP,
         `Net Exports` = net_export/GDP,
         `Household expenditure` = `Household expenditure`/GDP) %>% 
  select(!c(Imports, Exports, diff, percent_diff, up, down, GDP, cal_GDP, net_export)) %>% 
  pivot_longer(cols=c("Govenment expenditure","Gross capital formation","Net Exports","Household expenditure"), names_to="Components", values_to="Proportion")

ggplot(proportion)+
  geom_line(aes(x = Year, y = Proportion, group = Components, color = Components), size=1.3)+
  facet_wrap(~Country)+
  theme_bw()+
  labs(title = "Percentage different in GDP and calculated GDP",
       subtitle="In constant 2010 USD",
       x= "",
       y="Proportion")+
  scale_x_discrete(breaks=seq(1900, 2010, 10))

```

> If you want to, please change `country_list <- c("United States","India", "Germany")` to include your own country and compare it with any two other countries you like

```{r}
country_list <- c("Thailand", "China", "United Kingdom")
```

```{r}
tidy_GDP_data_1  <-  tidy_GDP_data %>% 
  filter(Country %in% country_list) %>% 
  filter(IndicatorName %in% c("Gross capital formation", "Exports of goods and services", "General government final consumption expenditure", "Household consumption expenditure (including Non-profit institutions serving households)", "Imports of goods and services")) %>% 
  mutate(IndicatorName_s = ifelse(IndicatorName == "Gross capital formation", "Gross capital formation",
                                  ifelse(IndicatorName == "Exports of goods and services", "Exports", 
                                         ifelse(IndicatorName == "Imports of goods and services", "Imports",
                                                ifelse(IndicatorName == "General government final consumption expenditure", "Govenment expenditure",
                                                       ifelse(IndicatorName == "Household consumption expenditure (including Non-profit institutions serving households)", "Household expenditure", "NA"))))))

ggplot(tidy_GDP_data_1, aes(x = Year, y = value_b, color = IndicatorName_s, group = IndicatorName_s))+
  geom_line()+
  facet_wrap(~Country)+
  theme_bw()+
  labs(title = "GDP components over time",
       subtitle="In constant 2010 USD",
       x= "",
       y="Billion US$", 
       colour = "Components of GDP")+
  scale_x_discrete(breaks=seq(1900, 2010, 10))
```

```{r}
tidy_GDP_data_2  <-  tidy_GDP_data %>% 
  filter(Country %in% country_list) %>% 
  filter(IndicatorName %in% c("Gross capital formation", "Exports of goods and services", "General government final consumption expenditure", "Household consumption expenditure (including Non-profit institutions serving households)", "Imports of goods and services", "Gross Domestic Product (GDP)")) %>% 
  mutate(IndicatorName_s = ifelse(IndicatorName == "Gross capital formation", "Gross capital formation",
                                  ifelse(IndicatorName == "Exports of goods and services", "Exports", 
                                         ifelse(IndicatorName == "Imports of goods and services", "Imports",
                                                ifelse(IndicatorName == "General government final consumption expenditure", "Govenment expenditure",
                                                       ifelse(IndicatorName == "Household consumption expenditure (including Non-profit institutions serving households)", "Household expenditure", 
                                                              ifelse(IndicatorName == "Gross Domestic Product (GDP)", "GDP", "NA")))))))


GDP_compare <- tidy_GDP_data_2 %>% 
  select(!c(value, IndicatorName)) %>% 
  pivot_wider(id_cols = c(CountryID, Country, Year), names_from = IndicatorName_s, values_from = value_b) %>% 
  mutate(net_export = Exports-Imports,
         cal_GDP = `Household expenditure`+`Govenment expenditure`+`Gross capital formation`+net_export,
         diff = cal_GDP - GDP,
         percent_diff = (cal_GDP - GDP)/GDP,
         up = ifelse(diff > 0, diff, 0),
         down = ifelse(diff < 0, -diff, 0))
```

```{r}
# absolute difference between cal_GDP and GDP
ggplot(GDP_compare) +
  geom_line(aes(x = Year, y = GDP, group = Country), color = "red")+
  geom_line(aes(x = Year, y = cal_GDP, group = Country), color = "blue")+
  geom_ribbon(aes(x=Year,ymin=GDP,ymax=cal_GDP,group=Country),fill="#CB454A",alpha=0.4)+
  facet_wrap(~Country)+
  theme_bw()+
  labs(title = "Different in GDP and calculated GDP",
       subtitle="In constant 2010 USD",
       x= "",
       y="Billion US$")+
  scale_x_discrete(breaks=seq(1900, 2010, 10))

# percentage difference between cal_GDP and GDP
ggplot(GDP_compare)+
  geom_line(aes(x = Year, y = percent_diff, group = Country))+
  geom_hline(yintercept=0,linetype="solid",color="orange", size = 1)+
  facet_wrap(~Country)+
  theme_bw()+
  labs(title = "Percentage different in GDP and calculated GDP",
       subtitle="In constant 2010 USD",
       x= "",
       y="%")+
  scale_x_discrete(breaks=seq(1900, 2010, 10))
```
```{r}
proportion <- GDP_compare %>% 
  mutate(`Govenment expenditure` = `Govenment expenditure`/GDP,
         `Gross capital formation` = `Gross capital formation`/GDP,
         `Net Exports` = net_export/GDP,
         `Household expenditure` = `Household expenditure`/GDP) %>% 
  select(!c(Imports, Exports, diff, percent_diff, up, down, GDP, cal_GDP, net_export)) %>% 
  pivot_longer(cols=c("Govenment expenditure","Gross capital formation","Net Exports","Household expenditure"), names_to="Components", values_to="Proportion")

ggplot(proportion)+
  geom_line(aes(x = Year, y = Proportion, group = Components, color = Components), size=1.3)+
  facet_wrap(~Country)+
  theme_bw()+
  labs(title = "Percentage different in GDP and calculated GDP",
       subtitle="In constant 2010 USD",
       x= "",
       y="Proportion")+
  scale_x_discrete(breaks=seq(1900, 2010, 10))
```
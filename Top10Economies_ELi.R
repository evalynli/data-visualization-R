#### Evalyn Li Data Visualization Portfolio
## Topic: Comparison of Top 10 Economics based on growth and GDP in 1976 and 2016


# Initial Settings --------------------------------------------------------
library(tidyverse)
library(WDI)
library(rlist)
options(stringsAsFactors = FALSE)

growthRateYrs <- NULL

for (x in c(1971:1976, 2011:2016)){
  growthRateYrs <- append(growthRateYrs, as.character(x))
}

df_GDP <- WDI(country = "all", indicator = "NY.GDP.MKTP.CD", start = 1971, end = 2016, extra = TRUE) %>%
  rename( Country = 'country',
          Capital = 'capital',
          GDP = 'NY.GDP.MKTP.CD',
          Year = 'year')  %>%
  select(Country, Capital, GDP, Year) %>% 
  spread(Year, GDP) %>% 
  select(Country, Capital, growthRateYrs) %>% 
  mutate(Capital = ifelse((Capital == ""), NA, Capital )) %>%
  filter(!is.na(Capital)) %>% 
  arrange(desc(`2016`)) %>% 
  head(n = 10)

top10_2016 <- df_GDP %>%
  select(Country) %>%
  pull()

df_GrowthRate <- df_GDP %>% 
  mutate(`GrowthRate72` = (`1972` - `1971`) / `1971`,
         `GrowthRate73` = (`1973` - `1972`) / `1972`,
         `GrowthRate74` = (`1972` - `1973`) / `1973`,
         `GrowthRate75` = (`1972` - `1974`) / `1974`,
         `GrowthRate76` = (`1972` - `1975`) / `1975`,
         `GrowthRate12` = (`2012` - `2011`) / `2011`,
         `GrowthRate13` = (`2013` - `2012`) / `2012`,
         `GrowthRate14` = (`2014` - `2013`) / `2013`,
         `GrowthRate15` = (`2015` - `2014`) / `2014`,
         `GrowthRate16` = (`2016` - `2015`) / `2015`,
         `1976` =  (`GrowthRate76`+ `GrowthRate75` + `GrowthRate74` + `GrowthRate73` +`GrowthRate72`) / 5,
         `2016` =  (`GrowthRate16`+ `GrowthRate15` + `GrowthRate14` + `GrowthRate13` +`GrowthRate12`) / 5) %>% 
  select(Country, `1976`, `2016`) %>% 
  gather("Year", "GrowthRate", -Country)

df_GDP <- df_GDP %>% 
  select(Country, `1976`, `2016`) %>% 
  gather("Year", "GDP", -Country)

df_perCapita <- WDI(country = "all", indicator = "NY.GDP.PCAP.CD", start = 1976, end = 2016) %>%
  rename( Country = 'country',
          perCapita = 'NY.GDP.PCAP.CD',
          Year = 'year') %>%
  filter(Country %in% top10_2016) %>%
  select(Country, perCapita, Year) %>%
  spread(Year, perCapita) %>%
  select(Country, `1976`, `2016`) %>%
  gather("Year", "GDPpCapita", -Country)

df_final <- df_GDP %>%
  left_join(df_GrowthRate, by = c("Country", "Year")) %>% 
  left_join(df_perCapita, by = c("Country", "Year"))


# Visualization -----------------------------------------------------------

library(ggrepel)
library(ggthemes)

ggplot(df_final) +
  scale_y_log10() +
  geom_point(aes(x = GrowthRate, y = GDPpCapita, size = GDP, color = ifelse((Country %in% c("United States", "China")), I("red"), I("grey90")))) +
  scale_size(range = c(2, 6)) +
  geom_label_repel(aes(x = GrowthRate, y = GDPpCapita, label = Country, color = ifelse((Country %in% c("United States", "China")), I("red"), I("grey90"))),
                   box.padding = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  guides(size = "none", color = "none") +
  facet_grid(.~ Year) +
  labs(title = "Top 10 Economies by 2016 GDP (current US$)",
       x = "Growth Rate (Average of annualized rate in last 5 years)",
       y = "GDP per Capita")

ggsave(filename = "figures/Top10Economies.png", width = 10, height = 5.5)
ggsave(filename = "figures/Top10Economies.pdf", width = 10, height = 5.5)


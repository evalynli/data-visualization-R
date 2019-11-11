#### Evalyn Li Data Visualization Portfolio
## Topic: State w/ the greatest number of 4-year (Dartmouth-like) institutions
## and their earnings.


# Initial Settings --------------------------------------------------------

library(tidyverse)
library(maps)
library(gridExtra)
options(stringsAsFactors = FALSE)

# Read Data ---------------------------------------------------------------
data1 <- "data/MERGED2015_16_PP.csv"
data2 <- "data/Most-Recent-Cohorts-Treasury-Elements.csv"

# Data Wrangling ----------------------------------------------------------



# This function takes a state name in the form of an abbreviation,
# returns a data frame of the longitudes and latitudes of the outline of the state

state_mapping <- function(state_abb) {
  abb_ToName <- data.frame( Abbreviation = state.abb, Full = tolower(state.name)) %>% 
    filter(Abbreviation == state_abb) %>% 
    select(Full) %>% 
    pull()
  State <- map_data("state") %>% 
    filter(region == abb_ToName)
  State
}

CollegeLocation <- read.csv(data1, na.strings = "NULL") %>% 
  select(INSTNM, STABBR, LATITUDE, LONGITUDE, ADM_RATE, CCBASIC, CCUGPROF) 

PostEarnings <- read.csv(data2, na.strings = c("NULL", "PrivacySuppressed")) %>%
  select(INSTNM, MD_EARN_WNE_P10)

CCBASIC_Dartmouth <- CollegeLocation %>% 
  filter(INSTNM == "Dartmouth College") %>% 
  select(CCBASIC) %>%
  pull() %>% 
  as.numeric()

CCUGPROF_Dartmouth <- CollegeLocation %>% 
  filter(INSTNM == "Dartmouth College") %>% 
  select(CCUGPROF) %>% 
  pull() %>% 
  as.numeric()

CollegeLocation <- CollegeLocation %>%
  filter(CCBASIC == CCBASIC_Dartmouth & CCUGPROF == CCUGPROF_Dartmouth)

df <- CollegeLocation %>%
  left_join(PostEarnings, by = "INSTNM") %>%
  group_by(STABBR)

states_amount <- df %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(n = 2) %>%
  select(STABBR)

df <- df %>%
  right_join(states_amount, by = "STABBR") %>%
  group_by(STABBR)

state_1 <- states_amount %>% 
   head(n = 1) %>% 
   pull()

state_2 <- states_amount %>% 
  tail(n = 1) %>% 
  pull()

state_1_outline <- state_mapping(state_1)
state_2_outline <- state_mapping(state_2)

state_1_colleges <- df %>% 
   filter(STABBR == state_1)

state_2_colleges <- df %>% 
  filter(STABBR == state_2)


plotstate1 <- ggplot() +
  geom_point(data = state_2_colleges, 
             aes(x = ADM_RATE, y = MD_EARN_WNE_P10, color = MD_EARN_WNE_P10))  +
  geom_smooth(data = state_2_colleges, aes(x = ADM_RATE, y = MD_EARN_WNE_P10), method = "lm") +
  guides(color=FALSE) +
  geom_text(data = state_2_colleges,
            aes(x = ADM_RATE, y = MD_EARN_WNE_P10 + 0.5, label = INSTNM, color = MD_EARN_WNE_P10),
            size = 2.5) +
  labs(title = "Relationship between Alumni Median Earnings and Admission Rate",
       x = "Admission Rate", 
       y = "Median Earnings After 10 years")


plotstate2 <- ggplot() +
  geom_polygon(data = state_2_outline,
               aes(x = long, y = lat, group = group),
               fill = "white",
               color = "blue") +
  geom_point(data = state_2_colleges, 
             aes(x = LONGITUDE, y = LATITUDE, color = MD_EARN_WNE_P10)) +
  geom_text(data = state_2_colleges,
            aes(x = LONGITUDE, y = LATITUDE + 0.35, label = INSTNM, color = MD_EARN_WNE_P10),
            size = 2.5) +
  coord_map() +
  labs(title = "State (New York) with largest number of schools \n similar to Dartmouth in Size and Research Opportunies",
       x = "Longitude",
       y = "Latitude") +
  guides(color = guide_legend("Median Earnings After 10 years"))

## To display maps
grid.arrange(plotstate1, plotstate2)

## To save image in PDF, not displayed
g <- arrangeGrob(plotstate1, plotstate2)
ggsave(filename = "figures/EarningsSSize.png", plot = g, width = 10, height = 7.5)
ggsave(filename = "figures/EarningsSSize.pdf", plot = g, width = 10, height = 7.5)


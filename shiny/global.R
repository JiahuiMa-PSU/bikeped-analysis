

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(leaflet)



# monthly mean daily volume by detectors
monthly_mean <- read.csv('www/mean_daily_vol_per_month.csv')

monthly_mean <- monthly_mean %>%
  mutate(month = factor(format(as.Date(date), "%b"), levels = month.abb),
         year = as.integer(format(as.Date(date), "%Y")))
# average daily volume by functional classification
monthly_mean_function <- monthly_mean %>%
  group_by(state, functional_classification, month, year) %>%
  summarize(avg_daily_vol = mean(mean_volume)) %>% 
  mutate_if(is.numeric, round, 2)
# average daily volume by facility type
monthly_mean_facility <- monthly_mean %>%
  group_by(state, facility_type, month, year) %>%
  summarize(avg_daily_vol = mean(mean_volume)) %>% 
  mutate_if(is.numeric, round, 2)


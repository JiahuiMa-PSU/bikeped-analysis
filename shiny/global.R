# Load necessary libraries
library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(plotly)

# Load the data

madt <- read_csv('www/madt.csv') 
segment_area <- read_csv('www/segment_info.csv')


seasonal_ratios <- madt %>%
  filter(flow_type == "bike") %>% 
  group_by(segment_area_id, segment_name, state, city, functional_classification, year) %>%
  summarize(
    highest_volume = max(volume, na.rm = TRUE),
    high_vol_month = month[which.max(volume)],
    lowest_volume = min(volume, na.rm = TRUE),
    low_vol_month = month[which.min(volume)],
    count = n(), .groups = 'drop'
  ) %>%
  mutate(seasonal_ratio = if_else(count == 12, highest_volume / lowest_volume, NA_real_)) %>%
  select(-count) %>% 
  ungroup() 

num_distinct_segment_state <- madt %>%
  group_by(state, functional_classification) %>%
  summarize(num_distinct_segment_areas = n_distinct(segment_area_id)) %>%
  ungroup()%>%
  bind_rows(
    madt %>%
      group_by(state) %>%
      summarize(num_distinct_segment_areas = n_distinct(segment_area_id)) %>%
      mutate(functional_classification = "Total")
  ) %>%
  pivot_wider(
    names_from = state,
    values_from = num_distinct_segment_areas,
    values_fill = list(num_distinct_segment_areas = 0)  # Fill missing values with 0
  ) %>% 
  mutate(Total = DC+MD+VA)
 

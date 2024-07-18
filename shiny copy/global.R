

library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)
library(leaflet)
library(lubridate)



#madt by segment area
madt <- read.csv('www/madt_seg.csv') 
segment_info <- read.csv("www/segment_area.csv")

#combine all facility descriptions in a segment area. keep the first lat long.
segments_combined <- segment_info %>%
  group_by(segment_area_id) %>%
  summarise(
    descriptions = paste(unique(facility_description), collapse = "; "),
    facility_types = paste(unique(facility_type), collapse = "; "),
    lat = first(na.omit(lat)),
    long = first(na.omit(long))
  )

madt <- madt %>%
  left_join(segments_combined, by = "segment_area_id") %>%
  mutate(start_time = as.Date(start_time)) %>% 
  mutate(year = year(start_time),
         month = month(start_time, label = T, abbr = T)) %>% 
  #Minnesota has too many 0s in the monthly volume, so excluded here to avoid bias.
  filter(state != "MN")  


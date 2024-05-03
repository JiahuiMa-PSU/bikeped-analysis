library (dplyr)
library(ggplot2)
monthly <- read.csv('data/month_state_fc_avg.csv')
monthly <- monthly %>%
  mutate(month = factor(format(as.Date(start_time), "%b"), levels = month.abb))

check <- read.csv('checklist.csv')
check <- check %>% 
  arrange(-count,-yyyy, functional_classification)

#for example (Figure1) in 2023, for Major Collector and Minor Arterial, 
#VA, MN, and DC all had 12 months' data.

fig1 <- monthly %>%
  filter(functional_classification == 'Major Collector' &
         state %in% c('VA', 'MN', 'DC')) %>%
  filter(grepl("^2023", start_time))

ggplot(fig1, aes(x = month, y = t_volume,group = state, color = state)) +
  geom_line() +
  labs(title = "Bike volume Change on Major Collectros of Each State (2023)",
       x = "Start Time",
       y = "Volume") +
  theme(plot.title=element_text(size=10),
        axis.text=element_text(size=5))

fig2 <- monthly %>%
  filter(functional_classification == 'Minor Arterial' &
           state %in% c('VA', 'MN', 'DC')) %>%
  filter(grepl("^2023", start_time)) %>%
  arrange(start_time)

ggplot(fig2, aes(x = month, y = t_volume,group = state, color = state)) +
  geom_line() +
  labs(title = "Bike volume Change on Minor Arterial of Each State (2023)",
       x = "Start Time",
       y = "Volume") +
  theme(plot.title=element_text(size=10),
        axis.text=element_text(size=5))

fig3 <- monthly %>%
  filter(functional_classification == 'Trail or Shared Use Path' &
           state %in% c('VA', 'MN', 'DC')) %>%
  filter(grepl("^2023", start_time)) %>%
  arrange(start_time)

ggplot(fig3, aes(x = month, y = t_volume,group = state, color = state)) +
  geom_line() +
  labs(title = "Bike volume Change on Trail or Shared Use Path of Each State (2023)",
       x = "Start Time",
       y = "Volume") +
  theme(plot.title=element_text(size=10),
        axis.text=element_text(size=5))



# monthly mean daily volume by detectors
monthly_mean <- read.csv('data/mean_daily_vol_per_month.csv')

monthly_mean <- monthly_mean %>%
  mutate(month = factor(format(as.Date(date), "%b"), levels = month.abb),
         year = as.integer(format(as.Date(date), "%Y")))

# average daily volume by functional classification
monthly_mean_function <- monthly_mean %>%
  group_by( state, functional_classification, month, year) %>%
  summarise(avg_daily_vol = mean(mean_volume, na.rm = TRUE))

# average daily volume by facility type
monthly_mean_facility <- monthly_mean %>%
  group_by(state, facility_type, month, year) %>%
  summarize(avg_daily_vol = mean(mean_volume)) %>% 
  mutate_if(is.numeric, round, 2)


  

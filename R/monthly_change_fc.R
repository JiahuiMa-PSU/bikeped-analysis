library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)




#----------data preparation
madt <- read.csv('shiny/www/madt.csv') 
segment_info<- read.csv("data/segment_id.csv")

segment_madt<- madt %>% 
  select(segment_area_id) %>% 
  unique()
#combine all facility descriptions in a segment area. keep the first lat long.
segment_info<-segment_info %>%
  filter(lat != "NULL") %>% 
  group_by(segment_area_id,segment_name,state,city,functional_classification) %>%
  summarise(
    descriptions = paste(unique(description), collapse = "; "),
    facility_types = paste(unique(facility_type), collapse = "; "),
    lat = first(na.omit(lat)),
    long = first(na.omit(long))
  ) %>%
  ungroup() %>% 
  semi_join(segment_madt, by = "segment_area_id")
  
  #mutate(lat = as.numeric(lat),
  #       long = as.numeric(long))

#----------use segment_info.csv directly for shinyapp.
write.csv(segment_info, "data/segment_info.csv", row.names = TRUE)


madt <- madt %>%
  left_join(segment_info, by = "segment_area_id") %>% 
  mutate(start_time = as.Date(start_time)) %>% 
  mutate(year = year(start_time),
         month = month(start_time, label = T, abbr = T)) 

write.csv(madt, "data/madt_w_info.csv", row.names = TRUE)
#use madt.csv in shinyapp.

madt<- read_csv("data/madt_w_info.csv")
#Numbers
#Number of segment areas in total
num_distinct_segment_areas <- madt %>%
  summarize(num_distinct = n_distinct(segment_area_id))

#Number of segment areas in state-functional_classification
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



#Seasonality Part 0: calculate each segment area's seasonal ratio. 


seasonal_ratios <- madt %>%
  filter(flow_type == "bike") %>% 
  group_by(segment_area_id, segment_name, state, city, functional_classification, year) %>%
  summarize(
    highest_volume = max(volume, na.rm = TRUE),
    high_vol_month = month[which.max(volume)],
    lowest_volume = min(volume, na.rm = TRUE),
    low_vol_month = month[which.min(volume)],
    count = n()
  ) %>%
  mutate(seasonal_ratio = if_else(count == 12, highest_volume / lowest_volume, NA_real_)) %>%
  select(-count) %>% 
  ungroup() 
test<-seasonal_ratios %>% 
  filter(year == 2023)

#Seasonality Part 1: calculate a state seasonal ratio. 

#state seasonal ratio: Method 1: Mean or Median of All Segments' Seasonal Ratios
state_ratios_mean <- seasonal_ratios %>%
  group_by(state, year) %>%
  summarize(mean_seasonal_ratio = mean(seasonal_ratio, na.rm = TRUE),
            median_seasonal_ratio = median(seasonal_ratio, na.rm = TRUE)) %>%
  ungroup()

#state seasonal ratio: Method 2: Monthly Total for the State
state_monthly_totals <- madt %>%
  group_by(segment_area_id, segment_name, state, city, functional_classification, flow_type, year) %>%
  #filter(n_distinct(month) == 12) %>% 
  group_by(state, year, month) %>%
  summarize(total_volume = sum(volume, na.rm = TRUE)) %>%
  ungroup()

state_ratios_total <- state_monthly_totals %>%
  group_by(state, year) %>%
  summarize(
    highest_monthly_volume = max(total_volume, na.rm = TRUE),
    lowest_monthly_volume = min(total_volume, na.rm = TRUE),
    state_seasonal_ratio = highest_monthly_volume / lowest_monthly_volume
  ) %>%
  ungroup() %>% 
#join the results of two methods 
  left_join(state_ratios_mean, by = c("state", "year"))

#one seasonal ratio for the whole state throughout 2019-2023
state_ratios_total_one <- state_monthly_totals %>%
  group_by(state) %>%
  summarize(
    highest_monthly_volume = max(total_volume, na.rm = TRUE),
    lowest_monthly_volume = min(total_volume, na.rm = TRUE),
    state_seasonal_ratio = highest_monthly_volume / lowest_monthly_volume
  ) %>%
  ungroup() %>% 
  #join the results of two methods 
  left_join(state_ratios_mean, by = c("state", "year"))




# Functional Classification Seasonal Ratio: Method 1: Median of All Segments' Seasonal Ratios
functional_classification_ratios_mean <- seasonal_ratios %>%
  group_by(functional_classification, year) %>%
  summarize(
            median_seasonal_ratio = median(seasonal_ratio, na.rm = TRUE)) %>%
  ungroup()

# Functional Classification Seasonal Ratio: Method 2: Monthly Total for Functional Classification
functional_classification_monthly_totals <- madt %>%
  group_by(segment_area_id, segment_name, state, city, functional_classification, flow_type, year) %>%
  filter(n_distinct(month) == 12) %>% 
  group_by(functional_classification, year, month) %>%
  summarize(total_volume = sum(volume, na.rm = TRUE)) %>%
  ungroup()

functional_classification_ratios_total <- functional_classification_monthly_totals %>%
  group_by(functional_classification, year) %>%
  summarize(
    highest_monthly_volume = max(total_volume, na.rm = TRUE),
    lowest_monthly_volume = min(total_volume, na.rm = TRUE),
    functional_classification_seasonal_ratio = highest_monthly_volume / lowest_monthly_volume
  ) %>%
  ungroup() %>% 
  # Join the results of two methods 
  left_join(functional_classification_ratios_mean, by = c("functional_classification", "year"))

# One seasonal ratio for the whole functional classification throughout 2019-2023
functional_classification_ratios_total_one <- functional_classification_monthly_totals %>%
  group_by(functional_classification) %>%
  summarize(
    highest_monthly_volume = max(total_volume, na.rm = TRUE),
    lowest_monthly_volume = min(total_volume, na.rm = TRUE),
    functional_classification_seasonal_ratio = highest_monthly_volume / lowest_monthly_volume
  ) %>%
  ungroup() %>% 
  # Join the results of two methods 
  left_join(functional_classification_ratios_mean %>%
              group_by(functional_classification) %>%
              summarize(mean_seasonal_ratio = mean(mean_seasonal_ratio, na.rm = TRUE),
                        median_seasonal_ratio = median(median_seasonal_ratio, na.rm = TRUE)), 
            by = "functional_classification")

# Print the results
print(functional_classification_ratios_total)
print(functional_classification_ratios_total_one)


# Load necessary libraries
library(ggplot2)
library(dplyr)

# Assuming your dataframe is named 'functional_classification_ratios_total'

# Filter out Interstate and Local
filtered_data <- functional_classification_ratios_total %>%
  filter(!functional_classification %in% c("Interstate", "Local"))

# Create the plot
ggplot(filtered_data, aes(x = year)) +
  geom_line(aes(y = functional_classification_seasonal_ratio, color = "Functional Classification Seasonal Ratio"), size = 1) +
  geom_line(aes(y = median_seasonal_ratio, color = "Median Seasonal Ratio"), size = 1, linetype = "dashed") +
  facet_wrap(~ functional_classification, scales = "fixed") +
  scale_y_continuous(trans = 'log10') +  # Apply logarithmic scale to the y-axis
  scale_color_manual(values = c("Functional Classification Seasonal Ratio" = "blue", 
                                "Median Seasonal Ratio" = "red")) +  # Custom colors
  labs(
    title = "Seasonal Ratios by Functional Classification",
    x = "Year",
    y = "Log-Scaled Seasonal Ratio",
    color = "Ratio Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14),
    strip.text = element_text(size = 12),  # Improve facet label readability
    legend.box.margin = margin(t = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )









library(car)  # For leveneTest
library(rstatix)  # For Shapiro-Wilk test and post-hoc tests

# Assuming your dataframe is named 'functional_classification_ratios_total'
# Filter out rows with NA values in functional_classification_seasonal_ratio
filtered_data <- seasonal_ratios %>%
  filter(!is.na(seasonal_ratio)) %>% 
  filter(seasonal_ratio != Inf) %>%
  mutate(trail = (functional_classification =="Trail or Shared Use Path"))

# Conduct ANOVA
anova_result <- aov(seasonal_ratio ~ trail, data = filtered_data)
summary(anova_result)

# Check assumptions
# 1. Homogeneity of variances
levene_test <- leveneTest(functional_classification_seasonal_ratio ~ functional_classification, data = filtered_data)
print(levene_test)

# 2. Normality of residuals
shapiro_test <- shapiro_test(anova_result$residuals)
print(shapiro_test)

# If ANOVA is significant, perform post-hoc tests
if(summary(anova_result)[[1]][["Pr(>F)"]][1] < 0.05) {
  post_hoc <- TukeyHSD(anova_result)
  print(post_hoc)
}

# Plot the means with confidence intervals for better visualization
ggplot(filtered_data, aes(x = functional_classification, y = functional_classification_seasonal_ratio)) +
  geom_boxplot() +
  labs(
    title = "Seasonal Ratios by Functional Classification",
    x = "Functional Classification",
    y = "Seasonal Ratio"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



top_vol<- madt %>%
  filter(state == "DC", month == "Jan") %>%
  filter(flow_type == "bike")%>%
  # group_by(year, segment_area_id, segment_name) %>%
  # summarise(volume = sum(volume, na.rm = TRUE)) %>%
  # arrange(desc(volume)) %>%
  # top_n(5, wt = volume)
  group_by(year, segment_area_id, segment_name) %>%
  summarise(volume = sum(volume, na.rm = TRUE)) %>%
  arrange(year, desc(volume)) %>%
  group_by(year) %>%
  slice_head(n = 5) %>%
  ungroup() %>% 
  rename(
    "Year" = year,
    "Segment Area ID" = segment_area_id,
    "Segment Name" = segment_name,
    "Volume" = volume
  )

highest_volume_months <- seasonal_ratios %>%
  count(functional_classification, high_vol_month) %>% 
  mutate(high_vol_month = factor(high_vol_month, levels = month.abb))

ggplot(highest_volume_months, aes(x = high_vol_month, y = n, fill = functional_classification)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Highest Volume Months by Functional Classification",
    x = "Month",
    y = "Count",
    fill = "Functional Classification"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  ) +
  facet_wrap(~ functional_classification, scales = "free_y")

seasonal_ratios<- seasonal_ratios %>% 
  ungroup()

highest_volume_months <- seasonal_ratios %>%
  select(functional_classification, high_vol_month) %>% 
  count(functional_classification, high_vol_month) %>%
  group_by(functional_classification) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  mutate(high_vol_month = factor(high_vol_month, levels = month.abb))

# Plot
ggplot(highest_volume_months, aes(x = functional_classification, y = percentage, fill = high_vol_month)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip()+
  labs(title = "Percentage of Highest Volume Months by Functional Classification",
       x = "Month",
       y = "Percentage",
       fill = "Functional Classification") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))



  seasonal_ratios_boxplot 
    ggplot(seasonal_ratios, aes(x = functional_classification, y = seasonal_ratio, fill = functional_classification)) +
    geom_boxplot(outlier.shape = NA) +
    scale_y_continuous(limits = c(0, 10))+
    labs(title = "Seasonal Ratios by Functional Classification",
         x = "Functional Classification",
         y = "Seasonal Ratio") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

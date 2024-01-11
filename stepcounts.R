#Written by Radhika Desai (credit accordingly when using this code)
#Finds daily and weekly step counts from Fitabase data in excel format

library(data.table)
library(jsonlite)
library(dplyr)
library(tidyverse)
library(lubridate)
library(readxl)


#df_list <- list()
combined_df <- data.frame()

#change below path to be the pathway to each individual participant file
steppath <- ("C:/Users/username/Drive/Documents/")
stepfiles <-list.files(steppath, pattern = "*.xlsx", full.names = TRUE)
for (step_file in stepfiles) {
  #Read the JSON file into a data frame
  data <- read_excel(step_file)  # Adjust flatten as needed
  
  # Add the data frame to the list
  combined_df <- rbind(combined_df, data)
}
#total step counts per day, identify all rows within a day, sum of all "value" per day

combined_df$dateTime <- as.POSIXct(combined_df$dateTime, format="%m/%d/%y %H:%M:%S")
combined_df$stepcounts <- as.numeric(combined_df$value)
# Extract the date from "dateTime"
combined_df$date <- as.Date(combined_df$dateTime)

# Sum the "value" for each day
sum_values_per_day <- combined_df %>%
  group_by(date) %>%
  summarise(total_value = (sum(stepcounts, na.rm = TRUE)))

# View the result
print(sum_values_per_day)

#step counts by week 

sum_values_per_day$date <- as.Date(sum_values_per_day$date)

# Add a new column with the week start date
sum_values_per_day <- sum_values_per_day %>%
  mutate(week_start = floor_date(date, "week"))

# Summarize the values for each week if needed
sum_values_per_week <- sum_values_per_day %>%
  group_by(week_start) %>%
  summarise(total_value = sum(total_value, na.rm = TRUE))

# View the result
print(sum_values_per_week)

#To write as csv
fwrite(sum_values_per_day, file = "filename_per_day.csv", row.names = FALSE) 
fwrite(sum_values_per_week, file = "filename_per_week.csv", row.names = FALSE) 

#Written by Radhika Desai (credit accordingly when using this code)
#Finds Fitbit wear-time duration by date and codes as valid day

install.packages(rjson) #install packages of libraries below 
library(data.table)
library(rjson)
library(jsonlite)
library(dplyr)
library(tidyverse)
library(lubridate)
#make sure all libraries are installed and open

df_list <- list()
combined_df <- data.frame()

#change below path to be the pathway to each individual subject pa files
jsonpath <- ("C:/Users/username/Drive/Documents/")
json_files <-list.files(jsonpath, pattern = "*.json", full.names = TRUE)
for (json_file in json_files) {
  # Read the JSON file into a data frame
  data <- fromJSON(json_file, flatten = TRUE)  # Adjust flatten as needed
  
  # Add the data frame to the list
  combined_df <- rbind(combined_df, data)
}

combined_df$parsed_datetime <- mdy_hms(combined_df$dateTime)
combined_df$date_only <- as.Date(combined_df$parsed_datetime)
result_df <- combined_df %>%
  group_by(date_only) %>%
  summarize(
    start_time = min(parsed_datetime),
    end_time = max(parsed_datetime),
    duration = as.numeric(max(parsed_datetime) - min(parsed_datetime))
  )
# if wear time is 10 hours or greater, mark as 1 else mark as 0 
result_df$validday <- ifelse(result_df$duration >= 10, 1, 0)

transposed_df <- data.frame(t(result_df[, -1]))  #transposes data so each wear time as row becomes column
colnames(transposed_df) <- result_df$date_only
rownames(transposed_df) <- NULL

#writes to csv file where 1st row is wear date, 2nd start wear date, 3rd is end wear date, 4th wear duration, 5th valid day
fwrite(transposed_df, file = "Filename.csv", row.names = FALSE) 

####################################################################



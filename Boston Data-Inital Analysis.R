#Name: Tanveer Negum
#Initail Analysis
#Date: 27 APril 2025

# ----------------------------------------
# Step 1: Load Libraries
# ----------------------------------------
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(lubridate)) install.packages("lubridate")
if (!require(readxl)) install.packages("readxl")
library(tidyverse)
library(lubridate)
library(readxl)

# ----------------------------------------
# Step 2: Read and Clean Boston CSVs (2016–2024)
# ----------------------------------------

# List all Boston year files
files <- list.files(pattern = "Boston_.*\\.csv")

# Get all unique column names
all_columns <- unique(unlist(lapply(files, function(f) names(read_csv(f, n_max = 0)))))

# Define safe reader
read_and_fix <- function(file, all_cols) {
  df <- read_csv(file, col_types = cols(.default = "c"))
  missing_cols <- setdiff(all_cols, names(df))
  df[missing_cols] <- NA
  df <- df[, all_cols]
  df$source_file <- file
  return(df)
}

# Read and combine all files
crime_list <- lapply(files, read_and_fix, all_cols = all_columns)
crime_data <- bind_rows(crime_list)

# ----------------------------------------
# Step 3: Load and Merge Metadata (Offense Codes)
# ----------------------------------------

# Load offense codes (for better labeling)
offense_codes <- read_excel("RMS_Offense_Codes.xlsx")

# Check available column names
names(offense_codes)

# Let's assume it has "OFFENSE_CODE" and "NAME" (adjust if different)
# Merge with crime data based on OFFENSE_CODE
if ("OFFENSE_CODE" %in% names(crime_data) && "OFFENSE_CODE" %in% names(offense_codes)) {
  crime_data <- crime_data %>%
    left_join(offense_codes, by = "OFFENSE_CODE")
}

# Now we have a readable NAME for offenses (instead of numeric code!)

# ----------------------------------------
# Step 4: Clean and Transform
# ----------------------------------------
crime_data <- crime_data %>%
  mutate(
    OCCURRED_ON_DATE = as.Date(OCCURRED_ON_DATE),
    Year = year(OCCURRED_ON_DATE),
    Month = month(OCCURRED_ON_DATE, label = TRUE),
    DayOfWeek = wday(OCCURRED_ON_DATE, label = TRUE),
    SHOOTING = str_to_upper(SHOOTING),
    SHOOTING = ifelse(SHOOTING %in% c("Y", "YES", "1", "TRUE"), "Y", NA)
  ) %>%
  filter(!is.na(OCCURRED_ON_DATE), Year >= 2016, Year <= 2024)

# ----------------------------------------
# Step 5: EDA - Data Summary
# ----------------------------------------

# Missing values
crime_data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Count") %>%
  print()

# Unique value summary
crime_data %>%
  summarise(across(everything(), n_distinct)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Unique_Values") %>%
  print()

# ----------------------------------------
# Step 6: Visualizations
# ----------------------------------------

# 1. Total Crimes per Year
crime_data %>%
  count(Year) %>%
  ggplot(aes(x = factor(Year), y = n)) +
  geom_col(fill = "#2C3E50") +
  geom_text(aes(label = n), vjust = -0.5, size = 3.5) +
  labs(title = "Total Crime Incidents per Year (2016–2024)",
       x = "Year", y = "Number of Incidents") +
  theme_minimal(base_size = 12)

# 2. Shooting Trend by Year
crime_data %>%
  filter(SHOOTING == "Y") %>%
  count(Year) %>%
  ggplot(aes(x = Year, y = n)) +
  geom_line(color = "red", size = 1.3) +
  geom_point(color = "darkred", size = 3) +
  geom_text(aes(label = n), vjust = -0.6, size = 3.2) +
  labs(title = "Shooting Incidents by Year (2016–2024)",
              x = "Year", y = "Shooting Count") +
  scale_x_continuous(breaks = 2016:2024) +
  theme_minimal(base_size = 12)

# 3. Top 10 Crime Types (using Offense Name)
if ("OFFENSE_CODE_GROUP" %in% names(crime_data)) {
  
  top10_crime <- crime_data %>%
    filter(!is.na(OFFENSE_CODE_GROUP)) %>%
    count(OFFENSE_CODE_GROUP, sort = TRUE) %>%
    slice_max(order_by = n, n = 10)
  
  if(nrow(top10_crime) > 0){
    ggplot(top10_crime, aes(x = reorder(OFFENSE_CODE_GROUP, n), y = n)) +
      geom_col(fill = "#0073C2FF") +
      geom_text(aes(label = n), hjust = -0.1, size = 3.5) +
      coord_flip() +
      labs(title = "Top 10 Crime Categories (2016–2024)",
           x = "Crime Type", y = "Number of Incidents") +
      theme_minimal(base_size = 12)
  } else {
    cat("No Top 10 Crimes found to plot.\n")
  }
} else {
  cat("OFFENSE_CODE_GROUP column is missing.\n")
}

# 4. Crime by Day of the Week
crime_data %>%
  count(Year, DayOfWeek) %>%
  ggplot(aes(x = DayOfWeek, y = n, fill = factor(Year))) +
  geom_col(position = "dodge") +
  labs(title = "Crime Frequency by Day of the Week and Year",
       x = "Day of Week", y = "Number of Incidents",
       fill = "Year") +
  theme_minimal(base_size = 12)


# 5. Crime by District
crime_data %>%
  filter(!is.na(DISTRICT)) %>%
  count(DISTRICT, sort = TRUE) %>%
  ggplot(aes(x = reorder(DISTRICT, n), y = n)) +
  geom_col(fill = "#16A085") +
  geom_text(aes(label = n), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(title = "Crime Frequency by Police District (2016–2024)",
       x = "District", y = "Number of Incidents") +
  theme_minimal(base_size = 12)

# 6. Heatmap - Month vs Day of Week
crime_data %>%
  count(Year, Month, DayOfWeek) %>%
  ggplot(aes(x = Month, y = DayOfWeek, fill = n)) +
  geom_tile(color = "white") +
  facet_wrap(~ Year) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Crime Heatmap: Month vs Day of Week (Yearly Comparison)",
       x = "Month", y = "Day of Week", fill = "Count") +
  theme_minimal(base_size = 12)

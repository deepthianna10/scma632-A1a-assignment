setwd("C:\\Users\\deepthi\\Downloads")
setwd("C:\\Users\\deepthi\\Downloads")
setwd("C:\\Users\\HP\\Downloads")
getwd()
# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load required libraries
libraries <- c("dplyr", "readr", "readxl", "tidyr", "ggplot2", "BSDA")
lapply(libraries, install_and_load)

# Reading the file into R
data <- read.csv("NSSO68.csv")

# Filtering for state
df <- data %>%
  filter(state_1 == "ARP")

#checking the filter
unique(data$state_1)
# Filtering for state
#df <- data %>%
 # filter(state == 12)

# Display dataset info
cat("Dataset Information:\n")
print(names(df))
print(head(df))
print(dim(df))

# Finding missing values
missing_info <- colSums(is.na(df))
cat("Missing Values Information:\n")
print(missing_info)

# Subsetting the data
arpnew <- df %>%
  select(state_1, District, Region, Sector, State_Region, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day)

# Impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}
arpnew$Meals_At_Home <- impute_with_mean(arpnew$Meals_At_Home)

# Finding outliers and removing them
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}

outlier_columns <- c("ricepds_v", "chicken_q")
for (col in outlier_columns) {
  apnew <- remove_outliers(apnew, col)
}

# Summarize consumption
arpnew$total_consumption <- rowSums(arpnew[, c("ricepds_v", "Wheatpds_q", "chicken_q", "pulsep_q", "wheatos_q")], na.rm = TRUE)

# Summarize and display top consuming districts and regions
summarize_consumption <- function(group_col) {
  summary <- arpnew %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption)) %>%
    arrange(desc(total))
  return(summary)
}

district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")

cat("Top 3 Consuming Districts:\n")
print(head(district_summary, 3))
cat("bottom 3 Consuming Districts:\n")
print(tail(district_summary, 3))

cat("Region Consumption Summary:\n")
print(region_summary)

# Rename districts and sectors
district_mapping <- c("23" = "Chittoor", "06" = "Rangareddi", "14" = "East Godavari", "13" = "Visakhapatnam")
sector_mapping <- c("2" = "URBAN", "1" = "RURAL")

arpnew$District <- as.character(arpnew$District)
arpnew$Sector <- as.character(arpnew$Sector)
arpnew$District <- ifelse(arpnew$District %in% names(district_mapping), district_mapping[arpnew$District], arpnew$District)
arpnew$Sector <- ifelse(arpnew$Sector %in% names(sector_mapping), sector_mapping[arpnew$Sector], arpnew$Sector)


# Test for differences in mean consumption between urban and rural
rural <- arpnew %>%
  filter(Sector == "RURAL") %>%
  select(total_consumption)

urban <- arpnew %>%
  filter(Sector == "URBAN") %>%
  select(total_consumption)


z_test_result <- z.test(rural, urban, alternative = "two.sided", mu = 0, sigma.x = 2.56, sigma.y = 2.34, conf.level = 0.95)

if (z_test_result$p.value < 0.05) {
  cat("P value is <", 0.05, ", Therefore we reject the null hypothesis.\n")
  cat("There is a difference between mean consumptions of urban and rural.\n")
} else {
  cat("P value is >=", 0.05, ", Therefore we fail to reject the null hypothesis.\n")
  cat("There is no significant difference between mean consumptions of urban and rural.\n")
}
#IPL
library(dplyr)
setwd("C:\\Users\\HP\\OneDrive\\Desktop\\deepthi  asignments scma")
getwd()
# Load the dataset
match_details <- read.csv("C:\\Users\\HP\\OneDrive\\Desktop\\deepthi  asignments scma\\Cricket_data (1).csv", stringsAsFactors = FALSE)
# Load player salary data
player_salary <- read_excel("C:\\Users\\HP\\OneDrive\\Desktop\\deepthi  asignments scma\\Cricket_data (1).csv")
# Convert Date column to datetime format
match_details$Date <- as.Date(match_details$Date, format = "%d-%m-%Y")

# Filter last three years of data
last_three_years <- match_details %>% 
  filter(Date >= "2024-01-01")

# Filter last two years of data
last_three_years <- match_details %>% 
  filter(Date >= "2023-01-01")

# Filter last one year of data
last_three_years <- match_details %>% 
  filter(Date >= "2022-01-01")

# Calculate performance metrics
performance <- last_three_years %>% 
  group_by(Striker) %>% 
  summarise(Total_Runs = sum(runs_scored), Balls_Faced = n())

# Clean and transform salary data
player_salary$Salary <- as.character(player_salary$Salary) # ensure Salary is a character vector
player_salary$Salary <- gsub("s", "", player_salary$Salary)
player_salary$Salary <- gsub(",", "", player_salary$Salary)

player_salary$Salary <- ifelse(grepl("lakh", tolower(player_salary$Salary)), 
                               as.integer(as.numeric(gsub(" lakh", "", player_salary$Salary)) * 100000), 
                               ifelse(grepl("crore", tolower(player_salary$Salary)), 
                                      as.integer(as.numeric(gsub(" crore", "", player_salary$Salary)) * 10000000), 
                                      as.integer(as.numeric(player_salary$Salary))))

# Replace NAs with 0
player_salary$Salary[is.na(player_salary$Salary)] <- 0

names(performance)
names(player_salary)

performance <- performance %>% rename(Player = Striker)
merged_data <- inner_join(performance, player_salary, by = "Player")

merged_data <- inner_join(performance, player_salary, by = c("Player" = "Player"))

common_columns <- intersect(names(performance), names(player_salary))
common_columns
merged_data <- inner_join(performance, player_salary, by = "Player")
common_column <- common_columns[1]
merged_data <- inner_join(performance, player_salary, by = common_column)
common_cols <- intersect(names(performance), names(player_salary))
merged_data <- performance %>%
  inner_join(player_salary, by = setNames(common_columns, common_columns))

# Correlation analysis
corr_matrix <- cor(merged_data[c("Total_Runs", "Balls_Faced", "Salary")])
print(corr_matrix)

# Regression analysis
df <- merged_data

# Define X and y
X <- df[, c("Balls_Faced", "Total_Runs")]
y <- df$Salary

# Fit the linear regression model
X <- as.matrix(df[, c("Balls_Faced", "Total_Runs")])
model <- lm(y ~ X)
model <- lm(y ~ Balls_Faced + Total_Runs, data = df)

# Print the summary of the model
summary(model)

# Get the coefficients
coefficients <- tidy(model)

# Print the coefficients
print(coefficients)

# Get the R-squared value
r_squared <- glance(model)

# Print the R-squared value
print(r_squared)

# Load the ggplot2 library
library(ggplot2)

# Create a scatterplot of the data with a regression line
ggplot(df, aes(x = Balls_Faced, y = Total_Runs)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) + 
  labs(x = "Balls Faced", y = "Total Runs") + 
  theme_classic()

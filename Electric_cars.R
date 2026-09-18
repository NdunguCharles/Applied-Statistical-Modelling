#load packages
library(tidyverse)
library(readr)
library(ggplot2)
library(car)
library(lmtest)
library(broom)

#Import data
data <- read_csv("/Users/charles/Desktop/Applied statistical modelling/data/Electriccars.csv")
#Examine structure
str(data)
#View first observations
head(data)
#Summary statistics
summary(data)
#check dimensions
dim(data)

#2. Clean the data
# Examine the suspicious columns
summary(data$...6)
summary(data$...7)

# Clean column names (replace spaces and special characters with underscores)
names(data_clean) <- make.names(names(data_clean))
# Number of rows before removing duplicates
nrow(data)

data <- data %>%
  distinct()

# Number of rows after removing duplicates
nrow(data)
#check missing values
colSums(is.na(data))
#Remove incomplete observations
# Drop empty extra columns and filter NAs on key columns
data_clean <- data %>%
  select(-starts_with("...")) %>%
  drop_na(Year, `BEV average price (USD)`, `Global Sales Volume`)

# Results
str(data_clean)
summary(data_clean)
dim(data_clean)

# Convert variables to numeric
data_clean <- data_clean %>%
  mutate(
    Year = as.numeric(Year),
    BEV_Price = as.numeric(`BEV average price (USD)`),
    Global_Sales = as.numeric(`Global Sales Volume`),
    Mileage = as.numeric(`Mileage (Km)`),
    Battery_Price = as.numeric(`Lithium Ion Battery Price (USD)`),
    New_Car_Price = as.numeric(`Average price of new car`)
  )

str(data_clean)
#Check count of negative values per numeric column
colSums(data_clean[, c(
  "BEV_Price",
  "Global_Sales",
  "Mileage",
  "Battery_Price",
  "New_Car_Price"
)] < 0, na.rm = TRUE)


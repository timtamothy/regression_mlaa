# Import packages
library(tidyverse)
library(here)
library(future.apply)
library(dplyr)
library(DataExplorer)
library(funModeling)
library(Hmisc)
library(future.apply)

# Import Data
df <- read_csv(here("repurchase_training.csv"))

# Summary Data of df
str(df)

## Simple EDA (Initial)

basic_eda <- function(data)
{
  glimpse(data)
  print(status(data))
  freq(data)
  print(profiling_num(data))
  plot_num(data)
  describe(data)
}

basic_eda(df)

## Basic Data Cleaning

# remove ID from the dataset

df1 <- df %>% 
  select(-ID)

colnames(df1)

# Factor all columns as they are categorical

to_factor <- colnames(df1)

df1[to_factor] <- future_lapply(df1[to_factor], factor)

status(df1)

basic_eda(df1)

# 52.77% of gender are null,
# 85.56% of age brackets are null












age_distinct <- df %>% 
  filter(age_band %in% c('1. <25', '2. 25 to 34', '3. 35 to 44', '4. 45 to 54', 
                         '5. 55 to 64', '6. 65 to 74', '7. 75+'))

gender_distinct <- df %>% 
  filter(gender %in% c('Male', 'Female'))  


basic_eda(age_distinct)
# if age is listed, almost all data will also have gender

basic_eda(gender_distinct)
# out of all gender, still 70% of data does not have age info

# EDA categorical plot function
plot_cats <- function(data, x_axis, y_axis)
{
  data %>% 
    ggplot(aes(x=x_axis, y=y_axis)) +
    geom_hex() +
    scale_fill_viridis_c()
}


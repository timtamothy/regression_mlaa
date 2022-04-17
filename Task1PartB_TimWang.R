# Import packages
library(tidyverse)
library(here)
library(future.apply)
library(dplyr)
library(DataExplorer)
library(funModeling)
library(Hmisc)
library(future.apply)
library(polycor)
library(ISLR)

#-------------------------------------------

## Import Data
df <- read_csv(here("repurchase_training.csv"))

# Summary Data of df
str(df)

#-------------------------------------------

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

#-------------------------------------------

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


# Creating some datasets without null values in gender and age brackeds
age_distinct <- df1 %>% 
  filter(age_band %in% c('1. <25', '2. 25 to 34', '3. 35 to 44', '4. 45 to 54', 
                         '5. 55 to 64', '6. 65 to 74', '7. 75+'))

gender_distinct <- df1 %>% 
  filter(gender %in% c('Male', 'Female'))  


basic_eda(age_distinct)
# if age is listed, almost all data will also have gender

basic_eda(gender_distinct)
# out of all gender, still 70% of data does not have age info

# Look at any correlations
colnames(df1)
names <- colnames(df1)

polychor(df1$sched_serv_warr, df1$sched_serv_paid)
#0.867 these are highly correlated

polychor(df1$sched_serv_warr, df1$total_paid_services)
#0.6711689 also correlated

polychor(df1$sched_serv_warr, df1$mth_since_last_serv)
#.30, not very correlated

polychor(df1$sched_serv_warr, df1$num_dealers_visited)
#.4457

polychor(df1$sched_serv_warr, df1$age_band)
# -.1939

polychor(df1$sched_serv_warr, df1$car_model)
# .056

polychor(df1$sched_serv_warr, df1$age_of_vehicle_years)
# 0.444

polychor(df1$sched_serv_warr, df1$non_sched_serv_warr)
# 0.4857

polychor(df1$sched_serv_warr, df1$non_sched_serv_paid)
# 0.403

polychor(df1$sched_serv_warr, df1$total_services)
# 0.8406

polychor(df1$sched_serv_warr, df1$annualised_mileage)
# 0.7707

polychor(df1$sched_serv_warr, df1$num_serv_dealer_purchased)
# 0.6277

# scheduled serv warranty highly correlated with 
# sched_serv_paid
# total_services
# annualised mileage
# consider reducing the features here

### polychor on the rest of the variables

polychor(df1$total_paid_services, df1$sched_serv_warr)
polychor(df1$total_paid_services, df1$sched_serv_paid)
# .751
polychor(df1$total_paid_services, df1$mth_since_last_serv)
polychor(df1$total_paid_services, df1$num_dealers_visited)
polychor(df1$total_paid_services, df1$age_band)
polychor(df1$total_paid_services, df1$car_model)
polychor(df1$total_paid_services, df1$age_of_vehicle_years)
polychor(df1$total_paid_services, df1$non_sched_serv_warr)
polychor(df1$total_paid_services, df1$non_sched_serv_paid)
# non sched serv paid .87
polychor(df1$total_paid_services, df1$total_services)
# total services .81
polychor(df1$total_paid_services, df1$annualised_mileage)
# annualised mileage .71
polychor(df1$total_paid_services, df1$num_serv_dealer_purchased)

polychor(df1$car_model, df1$car_segment)
# -0.339, ignore

polychor(df1$mth_since_last_serv, df1$sched_serv_warr)
polychor(df1$mth_since_last_serv, df1$sched_serv_paid)
polychor(df1$mth_since_last_serv, df1$total_paid_services)
polychor(df1$mth_since_last_serv, df1$mth_since_last_serv)
polychor(df1$mth_since_last_serv, df1$num_dealers_visited)
polychor(df1$mth_since_last_serv, df1$age_band)
polychor(df1$mth_since_last_serv, df1$car_model)
polychor(df1$mth_since_last_serv, df1$age_of_vehicle_years)
polychor(df1$mth_since_last_serv, df1$non_sched_serv_warr)
polychor(df1$mth_since_last_serv, df1$non_sched_serv_paid)
polychor(df1$mth_since_last_serv, df1$total_services)
polychor(df1$mth_since_last_serv, df1$annualised_mileage)
polychor(df1$mth_since_last_serv, df1$num_serv_dealer_purchased)

polychor(df1$num_dealers_visited, df1$sched_serv_warr)
polychor(df1$num_dealers_visited, df1$sched_serv_paid)
polychor(df1$num_dealers_visited, df1$total_paid_services)
polychor(df1$num_dealers_visited, df1$mth_since_last_serv)
polychor(df1$num_dealers_visited, df1$num_dealers_visited)
polychor(df1$num_dealers_visited, df1$age_band)
polychor(df1$num_dealers_visited, df1$car_model)
polychor(df1$num_dealers_visited, df1$age_of_vehicle_years)
polychor(df1$num_dealers_visited, df1$non_sched_serv_warr)
polychor(df1$num_dealers_visited, df1$non_sched_serv_paid)
polychor(df1$num_dealers_visited, df1$total_services)
polychor(df1$num_dealers_visited, df1$annualised_mileage)
polychor(df1$num_dealers_visited, df1$num_serv_dealer_purchased)

polychor(df1$age_band, df1$sched_serv_warr)
polychor(df1$age_band, df1$sched_serv_paid)
polychor(df1$age_band, df1$total_paid_services)
polychor(df1$age_band, df1$mth_since_last_serv)
polychor(df1$age_band, df1$num_dealers_visited)
polychor(df1$age_band, df1$age_band)
polychor(df1$age_band, df1$car_model)
polychor(df1$age_band, df1$age_of_vehicle_years)
polychor(df1$age_band, df1$non_sched_serv_warr)
polychor(df1$age_band, df1$non_sched_serv_paid)
polychor(df1$age_band, df1$total_services)
polychor(df1$age_band, df1$annualised_mileage)
polychor(df1$age_band, df1$num_serv_dealer_purchased)

polychor(df1$car_model, df1$sched_serv_warr)
polychor(df1$car_model, df1$sched_serv_paid)
polychor(df1$car_model, df1$total_paid_services)
polychor(df1$car_model, df1$mth_since_last_serv)
polychor(df1$car_model, df1$num_dealers_visited)
polychor(df1$car_model, df1$age_band)
polychor(df1$car_model, df1$car_model)
polychor(df1$car_model, df1$age_of_vehicle_years)
polychor(df1$car_model, df1$non_sched_serv_warr)
polychor(df1$car_model, df1$non_sched_serv_paid)
polychor(df1$car_model, df1$total_services)
polychor(df1$car_model, df1$annualised_mileage)
polychor(df1$car_model, df1$num_serv_dealer_purchased)

polychor(df1$age_of_vehicle_years, df1$sched_serv_warr)
polychor(df1$age_of_vehicle_years, df1$sched_serv_paid)
polychor(df1$age_of_vehicle_years, df1$total_paid_services)
polychor(df1$age_of_vehicle_years, df1$mth_since_last_serv)
polychor(df1$age_of_vehicle_years, df1$num_dealers_visited)
polychor(df1$age_of_vehicle_years, df1$age_band)
polychor(df1$age_of_vehicle_years, df1$car_model)
polychor(df1$age_of_vehicle_years, df1$age_of_vehicle_years)
polychor(df1$age_of_vehicle_years, df1$non_sched_serv_warr)
polychor(df1$age_of_vehicle_years, df1$non_sched_serv_paid)
polychor(df1$age_of_vehicle_years, df1$total_services)
polychor(df1$age_of_vehicle_years, df1$annualised_mileage)
polychor(df1$age_of_vehicle_years, df1$num_serv_dealer_purchased)

polychor(df1$non_sched_serv_warr, df1$sched_serv_warr)
polychor(df1$non_sched_serv_warr, df1$sched_serv_paid)
polychor(df1$non_sched_serv_warr, df1$total_paid_services)
polychor(df1$non_sched_serv_warr, df1$mth_since_last_serv)
polychor(df1$non_sched_serv_warr, df1$num_dealers_visited)
polychor(df1$non_sched_serv_warr, df1$age_band)
polychor(df1$non_sched_serv_warr, df1$car_model)
polychor(df1$non_sched_serv_warr, df1$age_of_vehicle_years)
polychor(df1$non_sched_serv_warr, df1$non_sched_serv_warr)
polychor(df1$non_sched_serv_warr, df1$non_sched_serv_paid)
polychor(df1$non_sched_serv_warr, df1$total_services)
polychor(df1$non_sched_serv_warr, df1$annualised_mileage)
polychor(df1$non_sched_serv_warr, df1$num_serv_dealer_purchased)

polychor(df1$total_services, df1$sched_serv_warr)
#.841 high
polychor(df1$total_services, df1$sched_serv_paid)
#.780 high
polychor(df1$total_services, df1$total_paid_services)
polychor(df1$total_services, df1$mth_since_last_serv)
polychor(df1$total_services, df1$num_dealers_visited)
polychor(df1$total_services, df1$age_band)
polychor(df1$total_services, df1$car_model)
polychor(df1$total_services, df1$age_of_vehicle_years)
polychor(df1$total_services, df1$non_sched_serv_warr)
#.833 high
polychor(df1$total_services, df1$non_sched_serv_paid)
polychor(df1$total_services, df1$total_services)
polychor(df1$total_services, df1$annualised_mileage)
#.807 high
polychor(df1$total_services, df1$num_serv_dealer_purchased)

polychor(df1$annualised_mileage, df1$sched_serv_warr)
polychor(df1$annualised_mileage, df1$sched_serv_paid)
polychor(df1$annualised_mileage, df1$total_paid_services)
polychor(df1$annualised_mileage, df1$mth_since_last_serv)
polychor(df1$annualised_mileage, df1$num_dealers_visited)
polychor(df1$annualised_mileage, df1$age_band)
polychor(df1$annualised_mileage, df1$car_model)
polychor(df1$annualised_mileage, df1$age_of_vehicle_years)
polychor(df1$annualised_mileage, df1$non_sched_serv_warr)
polychor(df1$annualised_mileage, df1$non_sched_serv_paid)
polychor(df1$annualised_mileage, df1$total_services)
#.807 high
polychor(df1$annualised_mileage, df1$annualised_mileage)
polychor(df1$annualised_mileage, df1$num_serv_dealer_purchased)

polychor(df1$num_serv_dealer_purchased, df1$sched_serv_warr)
polychor(df1$num_serv_dealer_purchased, df1$sched_serv_paid)
polychor(df1$num_serv_dealer_purchased, df1$total_paid_services)
polychor(df1$num_serv_dealer_purchased, df1$mth_since_last_serv)
polychor(df1$num_serv_dealer_purchased, df1$num_dealers_visited)
polychor(df1$num_serv_dealer_purchased, df1$age_band)
polychor(df1$num_serv_dealer_purchased, df1$car_model)
polychor(df1$num_serv_dealer_purchased, df1$age_of_vehicle_years)
polychor(df1$num_serv_dealer_purchased, df1$non_sched_serv_warr)
polychor(df1$num_serv_dealer_purchased, df1$non_sched_serv_paid)
polychor(df1$num_serv_dealer_purchased, df1$total_services)
polychor(df1$num_serv_dealer_purchased, df1$annualised_mileage)
polychor(df1$num_serv_dealer_purchased, df1$num_serv_dealer_purchased)

#-------------------------------------------

# Feature Engineering

# There exist many columns that have high correlations with one another
# Suggest feature engineering/reduction in order to improve this
# PCA?

# Create a new column with mean of:
# total_services, total_paid_services, sched_serv_paid, sched_serv_warr
# non_sched_serv_warr, annualised_mileage

df1$services_mean <- (df$total_paid_services + df$total_services +
                        df$sched_serv_paid + df$sched_serv_warr +
                        df$non_sched_serv_paid + df$annualised_mileage)/6

str(df1)

#-------------------------------------------

# View validation data

df2 <- read_csv(here("repurchase_validation.csv"))

basic_eda(df2)

# EDA categorical plot function
df1 %>% 
  ggplot(aes(x=Target, fill=df1$num_serv_dealer_purchased)) +
  geom_bar()


plot_cats <- function(data, x_axis, y_axis)
{
  data %>% 
    ggplot(aes(x=x_axis, y=y_axis)) +
    geom_hex() +
    scale_fill_viridis_c()
}


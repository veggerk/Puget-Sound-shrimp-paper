## 03_data-cleaning-oni.R

## This script is for cleaning the raw ONI such that it's able to be merged
## with the main shrimp data for modeling

## ONI data needs to be reordered so that the previous 12 month average can be calculated
## and added to the dataset. Trawl data needs to have CPUE calculated and the unique
## sampling event added so that trawl data can be linked to specific trawls and 
## associated with meta data


#### requirements ####

library(here)
library(readr)
library(httr)
library(dplyr)
library(tidyr)


## ONI raw file location
raw_file_loc <- "https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat"

## ONI raw file name to save
oni_raw_file <- "raw_ONI_data.txt"

## ONI clean file name
oni_tidy <- "oni_data_for_analysis.csv"

## (first year of shrimp data) - 1
yr_first <- 1998

## last year of shrimp data
yr_last <- 2019


#### read raw ONI data ####

## scrape raw data from web & save to txt file
raw_file_loc %>% 
  GET(config = config(ssl_verifypeer = FALSE)) %>%
  content("text") %>%
  cat(file = here("data", "raw", oni_raw_file), sep = "/n")


## column names to assign
col_names <- c("year",
               "DJF", "JFM", "FMA", "MAM",
               "AMJ", "MJJ", "JJA", "JAS",
               "ASO", "SON", "OND", "NDJ")

## raw data
oni_raw <- here("data", "raw", oni_raw_file) %>%
  read_tsv(skip = yr_first - 1950 + 1,
           col_names = FALSE,
           n_max = yr_last - yr_first + 1) %>%
  separate(col = X1,
           into = col_names,
           sep = "\\s+",
           convert = TRUE)


#### clean data ####

## convert to long/tidy format & compute annual ONI values
oni_clean <- oni_raw %>%
  pivot_longer(cols = colnames(oni_raw[-1]),
               names_to = "months", 
               values_to = "oni") %>%
  mutate(
    year_lagged = case_when(
      months %in% col_names[2:5] ~ year,
      TRUE ~ year + 1L
    )
  ) %>%
  filter(year_lagged >= yr_first + 1 & year_lagged <= yr_last) %>%
  group_by(year_lagged) %>%
  summarise(oni_annual = mean(oni)) %>%
  `colnames<-`(c("year", "oni"))

  
## write clean data to file
oni_clean %>% write_csv(here("data", "clean", "oni_data_for_analysis.csv"))



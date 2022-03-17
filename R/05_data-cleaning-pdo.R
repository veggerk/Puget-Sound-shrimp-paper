## 03_data-cleaning-oni.R

## This script is for cleaning the raw PDO such that it's able to be merged
## with the main shrimp data for modeling

## PDO data needs to be reordered so that the previous 12 month average can be calculated
## and added to the dataset. Trawl data needs to have CPUE calculated and the unique
## sampling event added so that trawl data can be linked to specific trawls and 
## associated with meta data


#### requirements ####

library(here)
library(readr)
library(httr)
library(dplyr)
library(tidyr)


## PDO raw file location
raw_file_loc <- "https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat"

## PDO raw file name to save
pdo_raw_file <- "raw_PDO_data.txt"

## PDO clean file name
pdo_tidy <- "pdo_data_for_analysis.csv"

## (first year of shrimp data) - 1
yr_first <- 1998

## last year of shrimp data
yr_last <- 2019


#### read raw PDO data ####

## scrape raw data from web & save to txt file
raw_file_loc %>% 
  GET(config = config(ssl_verifypeer = FALSE)) %>%
  content("text") %>%
  cat(file = here("data", "raw", pdo_raw_file), sep = "/n")


## column names to assign
col_names <- c("year",
               "Jan", "Feb", "Mar", "Apr",
               "May", "Jun", "Jul", "Aug",
               "Sep", "oct", "Nov", "Dec")

## raw data
pdo_raw <- here("data", "raw", pdo_raw_file) %>%
  read_tsv(skip = yr_first - 1854 + 1,
           col_names = FALSE,
           n_max = yr_last - yr_first + 1) %>%
  separate(col = X1,
           into = col_names,
           sep = "\\s+",
           convert = TRUE)


#### clean data ####

## convert to long/tidy format & compute annual PDO values
pdo_clean <- pdo_raw %>%
  pivot_longer(cols = colnames(pdo_raw[-1]),
               names_to = "months", 
               values_to = "pdo") %>%
  mutate(
    year_lagged = case_when(
      months %in% col_names[2:5] ~ year,
      TRUE ~ year + 1L
    )
  ) %>%
  filter(year_lagged >= yr_first + 1 & year_lagged <= yr_last) %>%
  group_by(year_lagged) %>%
  summarise(pdo_annual = mean(pdo)) %>%
  `colnames<-`(c("year", "pdo"))

  
## write clean data to file
pdo_clean %>% write_csv(here("data", "clean", "pdo_data_for_analysis.csv"))



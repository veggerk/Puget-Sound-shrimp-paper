## 02_data-cleaning-shrimp.R

## This script is for cleaning the raw catch data into a format suitable 
## for modeling.

#### setup ####

library(here)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)

## raw file name
raw_file_name_shrimp <- "puget_sound_inverts.xlsx"

## clean file names
clean_file_name_trawl <- "trawl_data_for_analysis.csv"
clean_file_name_shrimp <- "shrimp_data_for_analysis.csv"

## shrimp genera to include
genera <- c("Crangon alaskensis", "Pandalus platyceros","Pandalus eous / jordani")


#### read data ####

## raw file locations
clean_file_loc_trawl <- here("data", "clean", clean_file_name_trawl)
raw_file_loc_shrimp <- here("data", "raw", raw_file_name_shrimp)

## raw trawl data
data_clean_trawl <- read_csv(clean_file_loc_trawl,
                           na = c("", "NA"))

## raw shrimp data
data_raw_shrimp <- read_xlsx(raw_file_loc_shrimp, sheet = "data",
                             na = c("", "present", "not specified"))


#### clean data ####

## shrimp data
data_clean_shrimp <- data_raw_shrimp %>%
  filter(latin_name %in% genera) %>%
  group_by(latin_name, year) %>%
  summarise(total_count = sum(number))%>%
  arrange(latin_name, year) %>%
  left_join(data_clean_trawl, by = "year") %>%
  mutate(cpue = total_count / trawl_dist_total)%>%
select(latin_name, year,cpue)


#### write data ####

## raw file location
clean_data_loc <- here("data", "clean", clean_file_name_shrimp)

data_clean_shrimp %>% write_csv(clean_data_loc)


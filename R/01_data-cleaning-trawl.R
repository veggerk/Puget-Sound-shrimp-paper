## 01_data-cleaning-trawl.R

## This script is for cleaning up the trawl data.

# Description of raw data fields: 
 
# We'll describe the relevant fields we used. 
# Additional metadata can be obtained from Chelsea Wood at UW SAFS. 

# year = year that row of data was collected
# 
# month =  month that row of data was collected
#   
# day =  day that row of data was collected
# 
# date = date that row of data was collected
# 
# intended_depth_m = the depth in meters of the trawl being conducted 
# 
# shift = the time of day when that row of data was collected. There were 5 different shifts.
# 
# trawl_dist_m = the total distance the trawl was done in meters




#### setup ####

library(here)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

## raw file name
raw_file_name_trawl <- "puget_sound_trawlmaster.xlsx"

## clean file name
clean_file_name_trawl <- "trawl_data_for_analysis.csv"


#### read data ####

## raw file locations
raw_file_loc_trawl <- here("data", "raw", raw_file_name_trawl)

## raw trawl data
data_raw_trawl <- read_xlsx(raw_file_loc_trawl,
                            na = c("", "NA"))


#### clean data ####

#3 remove NAs in 2011
data_raw_trawl_no_na <- data_raw_trawl %>% drop_na(trawl_dist_m)

## correcting dates in 2007, adding unique sampling event, these changes aren't needed
## for the initial analysis, but they may be nice to have later, so I'm keeping them in. 
correct_date <- ymd('2007-05-12')

data_raw_trawl_no_na[292:295, "date"] <- correct_date

data_raw_trawl_no_na[292:295, "day"] <- 12

## create new column of time, year, and depth, which can be used as a unique ID
## for each trawl
data_raw_trawl_no_na$time_year_depth <- paste(data_raw_trawl_no_na$`shift`,
                                              data_raw_trawl_no_na$`year`,
                                              data_raw_trawl_no_na$`intended_depth_m`,
                                              sep = "_")

## trawl lengths for CPUE
trawl_lengths <- data_raw_trawl_no_na %>%
  filter(year >= 1999) %>%
  group_by(year) %>%
  summarise(trawl_dist_total = sum(trawl_dist_m) / 1000)


#### write data ####

## raw file location
clean_data_loc <- here("data", "clean", clean_file_name_trawl)

trawl_lengths %>% write_csv(clean_data_loc)

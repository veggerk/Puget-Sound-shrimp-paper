## 01_data-cleaning-trawl.R

## This script is for cleaning up the trawl data. 
# The datasheet is: 'puget_sound_trawlmaster.xlsx'

# Description of raw data fields: 
 
# year =	year that row of data was collected
# month	= month that row of data was collected
# day	= day that row of data was collected
# date = date that row of data was collected
# gear = specifies the net type used to trawl
# vessel = the name of the vessel, sometimes not recorded
# station	= numeric coding for trawl depth and trawl time, repeated each year, ex: intended depth of '10m' and shift of 'afternoon' = 1.1
# intended_depth_m = the target depth in meters of the trawl being conducted 
# shift	= the time of day. There were 5 different shifts.
# predicted_tide_category	= type of tide during trawl, sometimes not recorded
# wire_out_feet	= the trawl wire deployed in feet
# trawl_dist_nm	= the distance trawled in nautical miles
# trawl_dist_m = the total distance the trawl was done in meters, used to calculate CPUE
# trawl_direction_degrees_mag	= trawl distance in degrees mag, sometimes not recorded
# trawl_speed_knots	= the trawl speed in knots
# time_start_set = the exact time the trawl was set
# depth_m_start_set	= the actual depth in meters of the trawl net
# predicted_tide_m_start_set = the approximate tide elevation when the trawl was set
# depth_MLLW_m_start_set = the actual depth in meters of the trawl net
# lat_start_set	= the latitude of the trawl being set
# long_start_set = the longitude of the trawl being set
# time_start_tow = the exact time the trawling started
# depth_m_start_tow	= the actual depth in meters of the trawl net when trawling started
# predicted_tide_m_start_tow = the approximate tide elevation when the trawling started
# depth_MLLW_m_start_tow = the actual depth in meters of the trawl net when trawling started
# lat_start_tow	= the latitude of the trawl when the trawling started
# long_start_tow = the longitude of the trawl when the trawling started
# time_end_tow = the exact time the trawling ended
# depth_m_end_tow	= the actual depth in meters of the trawl net when the trawl ended
# predicted_tide_m_end_tow = the approximate tide elevation when the trawl ended
# depth_MLLW_m_end_tow = the actual depth in meters of the trawl net when the trawl ended
# lat_end_tow = the latitude of when the trawl ended
# long_end_tow = the longitude of when the trawl ended
# notes	= general notes about the trawl



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

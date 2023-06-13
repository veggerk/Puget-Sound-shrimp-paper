## 02_data-cleaning-shrimp.R

## This script is for cleaning the raw invert catch data into a format suitable 
## for modeling. The datasheet is: "puget_sound_inverts.xlsx"

# Description of raw data fields: 

# year = year that row of data was collected
# month =  month that row of data was collected
# day =  day that row of data was collected
# date = date that row of data was collected
# shift = the time of day when that row of data was collected. There were 5 different shifts.
# depth_m = the depth in meters that row of data was collected
# taxonomic group = the rough taxonomic classification of that row of data
# common_name = the common name for a specific species
# latin = the scientific name of a specific species
# species = combination of common name and latin name into one column
# sex = the gender of that species, if it was possible to determine
# number = the number of individuals of that species collected 
# notes = miscellaneous notes from data collection




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






crangon<-subset(data_clean_shrimp,subset = latin_name=="Crangon alaskensis")
pink_shrimp<-subset(data_clean_shrimp,subset = latin_name=="Pandalus eous / jordani")
spot_shrimp<-subset(data_clean_shrimp,subset = latin_name=="Pandalus platyceros")


hist(log(crangon$cpue))
shapiro.test(log(crangon$cpue))

hist(log(pink_shrimp$cpue))
shapiro.test(log(pink_shrimp$cpue))


hist(log(spot_shrimp$cpue))
spot_shrimp <- spot_shrimp[-1, ]
shapiro.test(log(spot_shrimp$cpue))

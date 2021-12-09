## 01.1_data-cleaning_trawl.R

## This script is for cleaning the trawl data such that it's able to be merged
## with the main shrimp data for modeling

## Trawl data needs to have CPUE calculated and the unique
## sampling event added so that trawl data can be linked to specific trawls and 
## associated with meta data. 

## Unique sampling event may or may not be needed for analysis, 
## but it's nice to have when doing analysis that includes further variables like depth
## and timing. 

## I'll also remove some columns that are not necessary, and pair the data down to something
## a bit more concise.

## there is a typo in one set of dates. In year 2007, there are 4 trawls that have dates 
## entered wrong, likely a data entry error. These 4 rows will be corrected. 

#### requirements ####

library(here)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

## data directories
raw_data_dir <- here("data", "raw")
clean_data_dir <- here("data", "clean")



## trawl raw file name
raw_file_trawl <- "puget_sound_trawlmaster.csv"

## trawl clean file name
trawl_cleaned_up <- "trawl_data_for_analysis.csv"

#### read trawl data ####

trawl_data_raw <- read_csv(file.path(raw_data_dir, raw_file_trawl))


#### calc CPUE from trawl data ####

# calculate CPUE
# CPUE_m2_of_net<-(trawl_data_raw$`trawl distance (m)`)*(trawl_data_raw$`net opening(m)`)

# add CPUE data into dataset
# trawl.new<-cbind(trawl_data_raw,CPUE_m2_of_net)

# do a 'text to columns' style operation to extract year, month, and day data 
dates<-data.frame(str_split_fixed(trawl_data_raw$date, "-", 3))
colnames(dates)<-c("year","month","day")

# add the newly created columns back in
trawl_cleaned_upr<-cbind(trawl_data_raw,dates)

# create new column of time, year, and depth, which can be used as a unique ID for each 
# trawl
trawl_cleaned_upr$time.year.depth <- paste(trawl_cleaned_upr$`time category`,
                                          trawl_cleaned_upr$`year`,
                                          trawl_cleaned_upr$`depth target`,
                                          sep = "_")



# calc the yearly total trawl length, this can actually be used as the CPUE, 
# since the net opening never changed, this will simply be shrimp per meter of trawl, 
# instead of shrimp per cubic meter of net CPUE. I'll comment out the code for the cubic 
# CPUE, but leave it in, in case it's needed later.


CPUE_yearly_trawl_length<-tapply(X=trawl_cleaned_upr$`trawl distance (m)`,
                            INDEX = trawl_cleaned_upr$year, 
                            FUN = sum)


trawl_cleaned_up<-cbind(trawl_cleaned_upr,CPUE_yearly_trawl_length)


#### write trawl finished data ####

trawl_cleaned_upr %>% write_csv(file.path(clean_data_dir, trawl_cleaned_up))






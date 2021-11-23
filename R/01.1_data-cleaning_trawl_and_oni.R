## 01.1_data-cleaning_trawl_and_oni.R

## This script is for cleaning the raw ONI and trawl data such that it's able to be merged
## with the main shrimp data for modeling

## ONI data needs to be reordered so that the previous 12 month average can be calculated
## and added to the dataset. Trawl data needs to have CPUE calculated and the unique
## sampling event added so that trawl data can be linked to specific trawls and 
## associated with meta data



#### requirements ####

library(here)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

## data directories
raw_data_dir <- here("data", "raw")
clean_data_dir <- here("data", "clean")




## oni raw file name
raw_file_oni <- "oni_raw.csv"

## oni clean file name
oni_tidy <- "oni_data_for_analysis.csv"

#### read oni data ####

oni_raw <- read_csv(file.path(raw_data_dir, raw_file_oni))


#### clean oni data ####

#change format of raw ONI data
oni_tidyr<-pivot_longer(oni_raw, 
        cols=c("DJF","JFM","FMA","MAM","AMJ","MJJ","JJA","JAS","ASO","SON","OND","NDJ",),
        names_to = "months", 
        values_to = "oni_values")
  

oni_tidyr %>% write_csv(file.path(clean_data_dir, oni_tidy))



## trawl raw file name
raw_file_trawl <- "trawl_data_raw.csv"

## trawl clean file name
trawl_cleaned_up <- "trawl_data_for_analysis.csv"

#### read trawl data ####

trawl_data_raw <- read_csv(file.path(raw_data_dir, raw_file_trawl))


#### calc CPUE from trawl data ####

#calculate CPUE
CPUE_m2_of_net<-(trawl_data_raw$`trawl distance (m)`)*(trawl_data_raw$`net opening(m)`)

# add CPUE data into dataset
trawl.new<-cbind(trawl_data_raw,CPUE_m2_of_net)

# do a 'text to columns' operation to extract year, month, and day data 
dates<-data.frame(str_split_fixed(trawl.new$date, "-", 3))
colnames(dates)<-c("year","month","day")

# add the newly created columns back in
trawl_cleaned_upr<-cbind(trawl.new,dates)

# create new column of time, year, and depth, which can be used as a unique ID for each 
# trawl
trawl_cleaned_upr$time.year.depth <- paste(trawl_cleaned_upr$`time category`,
                                          trawl_cleaned_upr$`year`,
                                          trawl_cleaned_upr$`depth target`,
                                          sep = "_")

#### write trawl finished data ####

trawl_cleaned_upr %>% write_csv(file.path(clean_data_dir, trawl_cleaned_up))


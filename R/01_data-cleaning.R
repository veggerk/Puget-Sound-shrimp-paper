## 01_data-cleaning.R

## This script is for cleaning the raw catch data into a format suitable for modeling.

## descriptions of each column in the dataset


## X1 = a column of numbers representing an original ordering of the dataset that's no 
## longer relevant 

## year = the year each row of data was collected

## X = a column of numbers representing a previous ordering of the dataset that's no 
## longer relevant 

## time.year.depth = a "unique sampling event ID" representing each unique instance where
## a sample was collected; aka a sampling event. In this case, the year, the time of day 
##sampling occurred, and the depth at which the nets were deployed. During each sampling year,
## one trawl was conducted at each target depth, during each discreet sampling time, resulting 
## in 20 sampling events in a 1 year period. (5 set sampling times, 4 set sampling depths)

## date = the date that sampling occurred during a given year. A series of sampling events 
## took place over the course of a 24 hour period, starting on one day, and ending sometime
## during the next. Thus for example: '14-15May' can be interpreted as a sampling event which
## occurred during the period of May 14th to May 15th on a given year. Since multiple sampling 
## events occurred within one time frame, and sampling was conducted during just one 24 hour period
## each year, this column is relatively useless for organizing the data.

## time.category = The approximate time of day that a sampling event occurred. Each year, 
## sampling was conducted in discreet blocks of time five times over the course of 24 hours.
## Defined (in order) as: "afternoon", "evening", "night", "early morning", and "morning"

## depth.target..m. = The target depth in meters at which a trawl was conducted. There were
#four target depths trawled during the study. During each discreet 'time category' that sampling
#was occurring, one trawl at each target depth occurred.

## group = the broad species groups that samples fall into. This dataset contains ALL the 
## shrimp data that was collected, with fish previously excluded. Thus the only species 
## group in the dataset is shrimp.

## common.name = the common names of the shrimp collected

## genus.species = the genus of the shrimp collected

## genus.species.updated = an updated genus of shrimp collected. This lumping was done 
## post hoc for analysis. The lumping was redone at a later date, thus this column is 
## obsolete.

## number = the number of individuals collected during that sampling event (time.year.depth)

## CPUE.m2.of.net = the area of the net that was dragged through the water on given sampling
## event. (length x width) sometimes the net was trawled for shorter or longer lengths.

## shrimp.cpue = The catch per unit effort of that group of individuals during that 
## sampling event (time.year.depth). Units are shrimp per meter squared of net (width of 
## net multiplied by the length of the trawl)

## avg.oni.previous.12.months = The average monthly Oceanic Nino Index score for the previous
## 12 months prior to sampling.

## el.nino.la.nina = The classification of what climate cycle the previous 12 months were 
## based on the 'avg.oni.previous.12.months' column. By definition a La Nina is when the 
## value is less than or equal to -0.5, and by definition an El Nino is when the value is 
## greater than or equal to 0.5. Values falling between the two numbers are clasified as 
## neither condition being present.

#### setup ####

library(here)
library(readxl)
library(readr)
library(dplyr)
library(tidyr)

## raw file name
raw_file_name <- "puget_sound_inverts.xlsx"

## clean file name
clean_file_name <- "shrimp_data_for_analysis.csv"

## genera to include
genera <- c("Crangon", "Pandalus")


#### read data ####

## raw file location
raw_file_loc <- here("data", "raw", raw_file_name)

## raw data
data_raw <- read_xlsx(raw_file_loc, sheet = "data",
                      na = c("", "present", "not specified"))


#### clean data ####

data_clean <- data_raw %>%
  separate(latin_name, "genus",
           extra = "drop", fill = "right") %>%
  group_by(genus, year) %>%
  summarise(total_count = sum(number)) %>%
  filter(genus %in% genera) %>%
  arrange(genus, year) %>%
  as.data.frame() %>%
  print()
  
#### write data ####

## raw file location
clean_data_loc <- here("data", "clean", clean_file_name)

data_clean %>% write_csv(clean_data_loc)

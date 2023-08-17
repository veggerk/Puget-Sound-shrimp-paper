
# packages needed
library(here)
library(tidyverse)

# bring in all data sheets

# file name 
x1999 <- "1999.csv"
x2000 <- "2000.csv"
x2001 <- "2001.csv"
x2002 <- "2002.csv"
x2003 <- "2003.csv"
x2004 <- "2004.csv"
x2005 <- "2005.csv"
x2006 <- "2006.csv"
x2007 <- "2007.csv"
x2008 <- "2008.csv"
x2009 <- "2009.csv"
x2010 <- "2010.csv"
x2011 <- "2011.csv"
x2012 <- "2012.csv"
x2013 <- "2013.csv"
x2014 <- "2014.csv"
x2015 <- "2015.csv"
x2016 <- "2016.csv"
x2017 <- "2017.csv"
x2018 <- "2018.csv"
x2019 <- "2019.csv"



# file locations
file_loc_sets1<- here("data/raw/temp and salinity", x1999)
file_loc_sets2 <- here("data/raw/temp and salinity", x2000)
file_loc_sets3 <- here("data/raw/temp and salinity", x2001)
file_loc_sets4 <- here("data/raw/temp and salinity", x2002)
file_loc_sets5 <- here("data/raw/temp and salinity", x2003)
file_loc_sets6 <- here("data/raw/temp and salinity", x2004)
file_loc_sets7 <- here("data/raw/temp and salinity", x2005)
file_loc_sets8 <- here("data/raw/temp and salinity", x2006)
file_loc_sets9 <- here("data/raw/temp and salinity", x2007)
file_loc_sets10 <- here("data/raw/temp and salinity", x2008)
file_loc_sets11 <- here("data/raw/temp and salinity", x2009)
file_loc_sets12 <- here("data/raw/temp and salinity", x2010)
file_loc_sets13 <- here("data/raw/temp and salinity", x2011)
file_loc_sets14 <- here("data/raw/temp and salinity", x2012)
file_loc_sets15 <- here("data/raw/temp and salinity", x2013)
file_loc_sets16 <- here("data/raw/temp and salinity", x2014)
file_loc_sets17 <- here("data/raw/temp and salinity", x2015)
file_loc_sets18 <- here("data/raw/temp and salinity", x2016)
file_loc_sets19 <- here("data/raw/temp and salinity", x2017)
file_loc_sets20 <- here("data/raw/temp and salinity", x2018)
file_loc_sets21 <- here("data/raw/temp and salinity", x2019)


# import files

set1 <- read_csv(file_loc_sets1,
                 na = c("", "NA"))

set2 <- read_csv(file_loc_sets2,
                 na = c("", "NA"))

set3 <- read_csv(file_loc_sets3,
                 na = c("", "NA"))

set4 <- read_csv(file_loc_sets4,
                 na = c("", "NA"))

set5 <- read_csv(file_loc_sets5,
                 na = c("", "NA"))

set6 <- read_csv(file_loc_sets6,
                 na = c("", "NA"))

set7 <- read_csv(file_loc_sets7,
                 na = c("", "NA"))

set8 <- read_csv(file_loc_sets8,
                 na = c("", "NA"))

set9 <- read_csv(file_loc_sets9,
                 na = c("", "NA"))

set10 <- read_csv(file_loc_sets10,
                 na = c("", "NA"))

set11 <- read_csv(file_loc_sets11,
                 na = c("", "NA"))

set12 <- read_csv(file_loc_sets12,
                 na = c("", "NA"))

set13 <- read_csv(file_loc_sets13,
                 na = c("", "NA"))

set14 <- read_csv(file_loc_sets14,
                  na = c("", "NA"))

set15 <- read_csv(file_loc_sets15,
                  na = c("", "NA"))

set16 <- read_csv(file_loc_sets16,
                  na = c("", "NA"))

set17 <- read_csv(file_loc_sets17,
                  na = c("", "NA"))

set18 <- read_csv(file_loc_sets18,
                  na = c("", "NA"))

set19 <- read_csv(file_loc_sets19,
                  na = c("", "NA"))

set20 <- read_csv(file_loc_sets20,
                  na = c("", "NA"))

set21 <- read_csv(file_loc_sets21,
                  na = c("", "NA"))



# join all the result files together
temp_salinity_data<-rbind(set1,set2,set3,set4,set5,set6,set7,set8,set9,set10,
                   set11,set12,set13,set14,set15,set16,set17,set18,set19,
                   set20,set21)



# remove unneeded  columns
temp_salinity_data<-temp_salinity_data[ ,c(-4,-6,-8)]



#fix date

# set format
temp_salinity_data$Sample_Date <- mdy_hm(temp_salinity_data$Sample_Date)

# pull out year
year <- year(temp_salinity_data$Sample_Date)

# add year as new column
temp_salinity_data <- cbind(temp_salinity_data,year)




# subset data to just our trawl depth range: 10m - 70m  
temp_salinity_data<- filter(.data = temp_salinity_data, 
                            temp_salinity_data$Sample_Depth >= 10 & 
                              temp_salinity_data$Sample_Depth <= 70)

# clean temp data
temp_data<-temp_salinity_data %>%
  group_by(year) %>%
  summarize(temp = mean(Sample_Temperature_field))


# clean salinity data
salinity_data <- temp_salinity_data %>%
  group_by(year) %>%
  summarize(salinity = mean(Salinity_field))


# save the organized data file to the clean data folder.

clean_file_name_data <- "temperature_data.csv"
clean_data_loc <- here("data/clean", clean_file_name_data)
temp_data %>% write_csv(clean_data_loc)

clean_file_name_data <- "salinity_data.csv"
clean_data_loc <- here("data/clean", clean_file_name_data)
salinity_data %>% write_csv(clean_data_loc)


ggplot(data = temp_data, aes(x=year, y=temp))+
  geom_line()

ggplot(data = salinity_data, aes(x=year, y=salinity))+
  geom_line()




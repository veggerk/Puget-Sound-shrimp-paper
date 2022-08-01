

library(here)
library(readxl)
library(readr)
library(dplyr)


## raw file name
raw_file_name_shrimp <- "puget_sound_inverts.xlsx"

raw_file_loc_shrimp <- here("data", "raw", raw_file_name_shrimp)

## raw shrimp data
data_raw_shrimp <- read_xlsx(raw_file_loc_shrimp, sheet = "data",
                             na = c("", "present", "not specified"))


## shrimp genera to include
genera <- c("Crangon alaskensis", "Pandalus platyceros","Pandalus eous / jordani")

common<-"shrimp"

#### summarize shrimp catch data ####

## shrimp data
data_shrimp_of_interest <- data_raw_shrimp %>%
  filter(latin_name %in% genera) %>%
  group_by(latin_name) %>%
  summarise(total_count = sum(number))

data_shrimp_summmary <- data_raw_shrimp %>%
  filter(taxonomic_group %in% common) %>%
  group_by(taxonomic_group)


unique_species_of_shrimp<-unique(data_shrimp_summmary$common_name)

aggregate(data_shrimp_summmary$number, by=list(data_shrimp_summmary$common_name), FUN=sum)




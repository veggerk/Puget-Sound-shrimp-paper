## 01_data-cleaning.R

## This script is for cleaning the raw catch data into a format suitable for modeling.

#### requirements ####

library(here)
library(readr)
library(dplyr)

## data directories
raw_data_dir <- here("data", "raw")
clean_data_dir <- here("data", "clean")

## raw file name
raw_file <- "shrimp.master.v3.csv"


#### read data ####

data_raw <- read_csv(file.path(raw_data_dir, raw_file))




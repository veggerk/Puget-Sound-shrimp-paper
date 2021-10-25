## 01_model-fitting.R

## This script is for fitting time series models to the data.

#### requirements ####

library(here)
library(readr)
library(dplyr)
library(MARSS)

## data directories
clean_data_dir <- here("data", "clean")

## clean file name
clean_file <- "shrimp_data_for_analysis.csv"


#### read data ####

data_clean <- read_csv(file.path(clean_data_dir, clean_file))




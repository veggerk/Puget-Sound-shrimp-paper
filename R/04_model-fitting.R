## 01_model-fitting.R

## This script is for fitting time series models to the data.

#### requirements ####

library(here)
library(readr)
library(dplyr)
library(tidyr)
library(MARSS)


## data directories
clean_data_dir <- here("data", "clean")

## clean file names
clean_file_shrimp <- "shrimp_data_for_analysis.csv"
clean_file_oni <- "oni_data_for_analysis.csv"


#### read data ####

## cleaned shrimp data
shrimp_data <- read_csv(here(clean_data_dir, clean_file_shrimp))

## ONI data
oni_data <- read_csv(here(clean_data_dir, clean_file_oni))


#### transform data ####

## shrimp data for MARSS
shrimp_trans <- shrimp_data %>%
  pivot_wider(names_from = genus, values_from = cpue) %>%
  select(-year) %>%
  log() %>%
  t()


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

## number of time series
nn <- nrow(shrimp_trans)

## years of data
tt <- ncol(shrimp_trans)


#### model 1: RW with no biases & unique states ####

## process model
BB <- diag(nn)

UU <- matrix(0, nrow = nn)

QQ <- matrix(list(0), nn, nn)
diag(QQ) <- c("q", "q")

## obs model
ZZ <- diag(nn)

AA <- matrix(0, nrow = nn)

RR <- matrix(list(0), nn, nn)
diag(RR) <- c("r", "r")

mod_list <- list(
  B = BB,
  U = UU,
  Q = QQ,
  Z = ZZ,
  A = AA,
  R = RR
)

con_list <- list(maxit = 2000)

mod_1 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 2: RW with shared bias & unique states ####

## process model
UU <- matrix("u", nrow = nn)

mod_list$U = UU

mod_2 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 3: RW with unique biases & unique states ####

## process model
UU <- matrix(c("1", "2"), nrow = nn)

mod_list$U = UU

mod_3 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 4: RW with shared ONI & unique states ####

## process model
UU <- matrix(0, nrow = nn)

CC <- matrix("C", nrow = 2)

cc <- matrix(oni_data$oni, nrow = 1)

mod_list$U = UU
mod_list$C = CC
mod_list$c = cc

mod_4 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 5: RW with unique ONI & unique states ####

## process model
CC <- matrix(c("1", "2"), nrow = 2)

mod_list$C = CC

mod_5 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 6: RW with no bias & shared state ####

## process model
BB <- matrix(1)

UU <- matrix(0)

CC <- matrix(0)

cc <- matrix(0, nrow = 1, ncol = tt)

QQ <- matrix("q")

## obs model
ZZ <- matrix(1, nrow = nn, ncol = 1)

mod_list <- list(
  B = BB,
  U = UU,
  C = CC,
  c = cc,
  Q = QQ,
  Z = ZZ,
  A = AA,
  R = RR
)

mod_6 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 7: RW with bias & shared state ####

## process model
UU <- matrix("u")

mod_list$U = UU

mod_7 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 8: RW with ONI & shared state ####

## process model
UU <- matrix(0)

CC <- matrix("C")

cc <- matrix(oni_data$oni, nrow = 1)

mod_list$U = UU
mod_list$C = CC
mod_list$c = cc

mod_8 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model selection ####

aicc <- c(mod_1$AICc, mod_2$AICc, mod_3$AICc, mod_4$AICc,
          mod_5$AICc, mod_6$AICc, mod_7$AICc, mod_8$AICc)
names(aicc) <- paste0("mod_", seq(8))

aicc %>%
  sort() %>%
  round(1) %>%
  magrittr::subtract(min(.))




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
clean_file_pdo <- "pdo_data_for_analysis.csv"
clean_file_oni <- "oni_data_for_analysis.csv"

#### read data ####

## cleaned shrimp data
shrimp_data <- read_csv(here(clean_data_dir, clean_file_shrimp))

## pdo data
pdo_data <- read_csv(here(clean_data_dir, clean_file_pdo))

## ONI data
oni_data <- read_csv(here(clean_data_dir, clean_file_oni))

all_data<-rbind(pdo_data$pdo,oni_data$oni)

#### transform data ####

## shrimp data for MARSS
shrimp_trans <- shrimp_data %>%
  pivot_wider(names_from = latin_name, values_from = cpue) %>%
  select(-year) %>%
  log() %>%
  scale() %>%
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
diag(QQ) <- rep("q",nn)

## obs model
ZZ <- diag(nn)

AA <- matrix(0, nrow = nn)

RR <- matrix(list(0), nn, nn)
diag(RR) <- rep("r",nn)

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
UU <- matrix(c("1", "2","3"), nrow = nn)

mod_list$U = UU

mod_3 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 4: RW with shared pdo & unique states ####

## process model
UU <- matrix(0, nrow = nn)

CC <- matrix("C", nrow = 3)

cc <- matrix(pdo_data$pdo, nrow = 1)

mod_list$U = UU
mod_list$C = CC
mod_list$c = cc

mod_4 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 5: RW with unique pdo & unique states ####

## process model
CC <- matrix(c("1", "2","3"), nrow = 3)

mod_list$C = CC

mod_5 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 6: RW with unique oni & unique states ####

## process model

cc <- matrix(oni_data$oni, nrow = 1)

CC <- matrix(c("1", "2","3"), nrow = 3)

mod_list$C = CC
mod_list$c = cc

mod_6 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 7: RW with no bias & shared state ####

## process model
BB <- matrix(1)

UU <- matrix(0)

CC <- matrix(0)

cc <- matrix(0, nrow = 1, ncol = tt)

QQ <- matrix("q")

## obs model
ZZ <- matrix(1, nrow = nn, ncol = 1)

# AA <- matrix(list(0), nrow = 2, ncol = 1)
# AA[2] <- "P"

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

mod_7 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 8: RW with bias & shared state ####

## process model
UU <- matrix("u")

mod_list$U = UU

mod_8 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 9: RW with PDO & shared state ####

## process model
UU <- matrix(0)

CC <- matrix("C")

cc <- matrix(pdo_data$pdo, nrow = 1)

mod_list$U = UU
mod_list$C = CC
mod_list$c = cc

mod_9 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 10: RW with ONI & shared state ####

## process model
UU <- matrix(0)

CC <- matrix("C")

cc <- matrix(oni_data$oni, nrow = 1)

mod_list$U = UU
mod_list$C = CC
mod_list$c = cc

mod_10 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 11: RW with PDO, ENSO & shared state ####

## process model
UU <- matrix(0)

CC <- matrix(c("PDO","ENSO"),nrow=1,ncol=2)

cc <- all_data

mod_list$U = UU
mod_list$C = CC
mod_list$c = cc

mod_11 <- MARSS(shrimp_trans, model = mod_list, control = con_list)





#### model 12: RW with ONI & shared state ####

## process model
UU <- matrix(0)

CC <- matrix("C")

cc <- matrix(oni_data$oni, nrow = 1)

mod_list$U = UU
mod_list$C = CC
mod_list$c = cc

mod_12 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 13: RW with shared ONI & unique states ####

## process model
UU <- matrix(0, nrow = nn)

CC <- matrix("C", nrow = 3)

cc <- matrix(oni_data$oni, nrow = 1)

ZZ<-diag(nn)
BB<-diag(nn)
QQ <- matrix(list(0), nn, nn)
diag(QQ) <- rep("q",nn)

mod_list$U = UU
mod_list$C = CC
mod_list$c = cc
mod_list$Z = ZZ
mod_list$B = BB
mod_list$Q = QQ



mod_13 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 14: RW with PDO, ENSO & unique state ####

## process model
UU <- matrix(0, nrow = 3)
CC <- matrix(c("PDO.1", "PDO.2", "PDO.3", "ONI.1", "ONI.2", "ONI.3"), nrow = 3, ncol = 2)

cc <- all_data
ZZ<-diag(3)
BB<-diag(3)

QQ <- matrix(list(0), nn, nn)
diag(QQ) <- rep("q",nn)

mod_list$Q = QQ
mod_list$B = BB
mod_list$Z = ZZ
mod_list$U = UU
mod_list$C = CC
mod_list$c = cc

mod_14 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model selection ####

aicc <- c(mod_1$AICc, mod_2$AICc, mod_3$AICc, mod_4$AICc,
          mod_5$AICc, mod_6$AICc, mod_7$AICc, mod_8$AICc, 
          mod_9$AICc, mod_10$AICc, mod_11$AICc, mod_12$AICc, 
          mod_13$AICc, mod_14$AICc)
names(aicc) <- paste0("mod_", seq(14))

aicc %>%
  sort() %>%
  round(1) %>%
  magrittr::subtract(min(.))

# mod_8 mod_11  mod_9  mod_2  mod_7  mod_4 mod_10 mod_12  mod_3  mod_5 mod_14  mod_1 mod_13  mod_6 
# 0.0    1.1    2.1    4.7    5.0    5.9    6.8    6.8    9.8   11.1   16.4   22.8   24.2   29.1 

## model 8 and 11 are the top models 
mod_8
mod_11
#### plot fits ####

pdf(file = here("figures", "model_fit_PDO_and_ENSO.pdf"),
    height = 5, width = 6)

par(mai = c(0.9, 0.9, 0.1, 0.1),
    omi = rep(0.1, 4))

## ts of years
years <- shrimp_data %>%
  pivot_wider(names_from = latin_name, values_from = cpue) %>%
  select(year) %>%
  unlist()

## plot data and fit
matplot(years, t(shrimp_trans),
        type = "o", lty = "solid", pch = 16, las = 1,
        xlab = "Year", ylab = "Standardized log (CPUE)",
        col = c("#e41a1c", "#377eb8","#4daf4a"))
lines(years, as.vector(mod_11$states))
lines(years, as.vector(mod_11$states + 2 * mod_11$states.se),
      col = "gray")
lines(years, as.vector(mod_11$states - 2 * mod_11$states.se),
      col = "gray")
text(1999, 1.5, expression(italic("Northern crangon shrimp")),
     pos = 4, col = "#e41a1c")
text(1999, 1.2, expression(italic("Pink shrimp")),
     pos = 4, col = "#377eb8")
text(1999, 0.9, expression(italic("Spot shrimp")),
     pos = 4, col = "#4daf4a")

dev.off()

## 13_model-fitting temp and salinity.R


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
clean_file_temp <- "temperature_data.csv"
clean_file_salinity <- "salinity_data.csv"

#### read data ####

## cleaned shrimp data
shrimp_data <- read_csv(here(clean_data_dir, clean_file_shrimp))

## pdo data
temp_data <- read_csv(here(clean_data_dir, clean_file_temp))

## ONI data
salinity_data <- read_csv(here(clean_data_dir, clean_file_salinity))

all_covars <- rbind(temp_data$temp, salinity_data$salinity)

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


#### model 24: RW with no biases & unique states ####

## process model
BB <- diag(nn)

UU <- matrix(0, nrow = nn)

QQ <- matrix(list(0), nn, nn)
diag(QQ) <- rep("q", nn)

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

mod_24 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 25: RW with shared bias & unique states ####

## process model
UU <- matrix("u", nrow = nn)

mod_list$U <- UU

mod_25 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 26: RW with unique biases & unique states ####

## process model
UU <- matrix(c("1", "2","3"), nrow = nn)

mod_list$U = UU

mod_26 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 27: RW with shared temp & unique states ####

## process model
UU <- matrix(0, nrow = nn)

CC <- matrix("C", nrow = nn)

cc <- matrix(temp_data$temp, nrow = 1)

mod_list$U <- UU
mod_list$C <- CC
mod_list$c <- cc

mod_27 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 28: RW with unique temp & unique states ####

## process model
CC <- matrix(c("1", "2","3"), nrow = nn)

mod_list$C <- CC

mod_28 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 29: RW with unique salinity & unique states ####

## process model

cc <- matrix(salinity_data$salinity, nrow = 1)

CC <- matrix(c("1", "2","3"), nrow = nn)

mod_list$C <- CC
mod_list$c <- cc

mod_29 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 30: RW with no bias & shared state ####

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

mod_30 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 31: RW with bias & shared state ####

## process model
UU <- matrix("u")

mod_list$U <- UU

mod_31 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 32: RW with temp & shared state ####

## process model
UU <- matrix(0)

CC <- matrix("C")

cc <- matrix(temp_data$temp, nrow = 1)

mod_list$U <- UU
mod_list$C <- CC
mod_list$c <- cc

mod_32 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 33: RW with salinity & shared state ####

## process model
UU <- matrix(0)

CC <- matrix("C")

cc <- matrix(salinity_data$salinity, nrow = 1)

mod_list$U <- UU
mod_list$C <- CC
mod_list$c <- cc

mod_33 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 34: RW with temp, salinity & shared state ####

## process model
UU <- matrix(0)

CC <- matrix(c("temp", "salinity"), nrow = 1, ncol = 2)

cc <- all_covars

mod_list$U <- UU
mod_list$C <- CC
mod_list$c <- cc

mod_34 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 35: RW with shared salinity & unique states ####

## process model
UU <- matrix(0, nrow = nn)

CC <- matrix("C", nrow = 3)

cc <- matrix(salinity_data$salinity, nrow = 1)

BB <- diag(nn)
QQ <- matrix(list(0), nn, nn)
diag(QQ) <- rep("q", nn)

## obs model
ZZ <- diag(nn)

mod_list$U <- UU
mod_list$C <- CC
mod_list$c <- cc
mod_list$Z <- ZZ
mod_list$B <- BB
mod_list$Q <- QQ

mod_35 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 36: RW with temp, salinity & unique states ####

## process model
UU <- matrix(0, nrow = nn)
CC <- matrix(c(paste("temp", seq(nn), sep = "."),
               paste("salinity", seq(nn), sep = ".")),
             nrow = 3, ncol = 2)

cc <- all_covars
BB <- diag(nn)

QQ <- matrix(list(0), nn, nn)
diag(QQ) <- rep("q", nn)

## obs model
ZZ <- diag(nn)

mod_list$Q <- QQ
mod_list$B <- BB
mod_list$Z <- ZZ
mod_list$U <- UU
mod_list$C <- CC
mod_list$c <- cc

mod_36 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 37: RW with temp, salinity & unique states ####

## process model
CC <- matrix(c(rep("temp", nn),
               rep("salinity", nn)),
             nrow = 3, ncol = 2)

mod_list$C <- CC

mod_37 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 38: RW with no bias & shared state for Pandalus ####

## number of states
pp <- 2

## process model
UU <- matrix(0, nrow = pp)

CC <- matrix(rep(0, pp),
             nrow = pp, ncol = 2)

cc <- all_covars

BB <- diag(pp)

QQ <- matrix(list(0), pp, pp)
diag(QQ) <- rep("q", pp)

## obs model
ZZ <- matrix(c(1, 0, 0, 0, 1, 1),
             nrow = nn, ncol = pp)

mod_list$Q <- QQ
mod_list$B <- BB
mod_list$Z <- ZZ
mod_list$U <- UU
mod_list$C <- CC
mod_list$c <- cc

mod_38 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 39: RW with shared bias & shared state for Pandalus ####

## process model
UU <- matrix("u", nrow = pp)

mod_list$U <- UU

mod_39 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 40: RW with unique biases & shared state for Pandalus ####

## process model
UU <- matrix(paste("u", seq(pp), sep = "."), nrow = pp)

mod_list$U <- UU

mod_40 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 41: RW with shared temp & shared state for Pandalus ####

## process model
UU <- matrix(0, nrow = pp)

CC <- matrix("C", nrow = pp)

cc <- matrix(temp_data$temp, nrow = 1)

mod_list$U <- UU
mod_list$C <- CC
mod_list$c <- cc

mod_41 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 42: RW with unique temp & shared state for Pandalus ####

## process model
CC <- matrix(paste("C", seq(pp), sep = "."), nrow = pp)

mod_list$C <- CC

mod_42 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 43: RW with shared salinity & shared state for Pandalus ####

## process model
CC <- matrix("C", nrow = pp)

cc <- matrix(salinity_data$salinity, nrow = 1)

mod_list$C <- CC
mod_list$c <- cc

mod_43 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 44: RW with unique salinity & shared state for Pandalus ####

## process model
CC <- matrix(paste("C", seq(pp), sep = "."), nrow = pp)

mod_list$C <- CC

mod_44 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 45: RW with shared temp & salinity & shared state for Pandalus ####

## process model
CC <- matrix(c("temp", "temp", "salinity", "salinity"),
             nrow = pp, ncol = 2)

cc <- all_covars

mod_list$C <- CC
mod_list$c <- cc

mod_45 <- MARSS(shrimp_trans, model = mod_list, control = con_list)


#### model 46: RW with unique temp & salinity & shared state for Pandalus ####

## process model
CC <- matrix(c(paste("temp", seq(pp), sep = "."),
               paste("salinity", seq(pp), sep = ".")),
             nrow = pp, ncol = 2)

mod_list$C <- CC

mod_46 <- MARSS(shrimp_trans, model = mod_list, control = con_list)




#### model selection ####

aicc <- c(mod_24$AICc, mod_25$AICc, mod_26$AICc, mod_27$AICc,
          mod_28$AICc, mod_29$AICc, mod_30$AICc, mod_31$AICc, 
          mod_32$AICc, mod_33$AICc, mod_34$AICc, mod_35$AICc, 
          mod_36$AICc, mod_37$AICc, mod_38$AICc, mod_39$AICc,
          mod_40$AICc, mod_41$AICc, mod_42$AICc, mod_43$AICc,
          mod_44$AICc, mod_45$AICc, mod_46$AICc)
names(aicc) <- paste0("mod_", seq(length(aicc))+23)

aicc %>%
  sort() %>%
  round(1) %>%
  magrittr::subtract(min(.))

# mod_31 mod_33 mod_32 mod_34  mod_43 mod_41 mod_45 mod_44  mod_35  mod_42 mod_27 mod_37 
# 0.0    0.1    0.5    0.9    2.4    2.8    3.2   4.7      4.9     5.0    5.2    5.8 
# mod_46  mod_29 mod_28 mod_36 
# 6.3     9.9   10.2    14.7   

## model 31,32, 33, 34 are the top models 
mod_31
mod_32
mod_33
mod_34

MARSSparamCIs(mod_32)


# mod_8: simple model with bias: delta AICc: 0
# mod_11: ONI and PDO model: delta AICc: 1.1
# mod_34: temp and salinity model: delta AICc: 0.9



#### plot fits model with ONI, PDO, salinity, and temp ####

pdf(file = here("figures", "fig_03_model_fits_with_temp_and_salinity_added.pdf"),
    height = 7, width =10)

par(mai = c(0.9, 0.9, 0.3, 0.1),
    omi = rep(0.1, 4),mfrow=c(1,3))

## ts of years
years <- shrimp_data %>%
  pivot_wider(names_from = latin_name, values_from = cpue) %>%
  select(year) %>%
  unlist()

## plot data and fit PDO and ONI models
matplot(years, t(shrimp_trans),
        type = "o", lty = "solid", pch = 16, las = 1,
        xlab = "Year", ylab = "Standardized log (CPUE)", main="A",
        col = c("#d95f02", "#7570b3","#1b9e77"), xaxt='n')
axis(side=1, at=seq(1999, 2019, by=5))
lines(years, as.vector(mod_11$states),col="black")
text(1999, 1.5, expression(italic("Northern crangon shrimp")),
     pos = 4, col = "#d95f02")
text(1999, 1.2, expression(italic("Pink shrimp")),
     pos = 4, col = "#7570b3")
text(1999, 0.9, expression(italic("Spot shrimp")),
     pos = 4, col = "#1b9e77")

## plot data and fit temp and salinity model
matplot(years, t(shrimp_trans),
        type = "o", lty = "solid", pch = 16, las = 1,
        xlab = "Year", ylab = "Standardized log (CPUE)", main="B",
        col = c("#d95f02", "#7570b3","#1b9e77"), xaxt='n')
axis(side=1, at=seq(1999, 2019, by=5))
lines(years, as.vector(mod_34$states),col="black")
text(1999, 1.5, expression(italic("Northern crangon shrimp")),
     pos = 4, col = "#d95f02")
text(1999, 1.2, expression(italic("Pink shrimp")),
     pos = 4, col = "#7570b3")
text(1999, 0.9, expression(italic("Spot shrimp")),
     pos = 4, col = "#1b9e77")

#### plot fits top model ####

## plot data and fit
matplot(years, t(shrimp_trans),
        type = "o", lty = "solid", pch = 16, las = 1,
        xlab = "", ylab = "",main="C",
        col = c("#d95f02", "#7570b3","#1b9e77"), xaxt='n')
axis(side=1, at=seq(1999, 2019, by=5))
lines(years, as.vector(mod_8$states),col="black")


dev.off()


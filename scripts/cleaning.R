# Camille Voegeli
# Script made to clean the raw data to usable data
# Made for Les Bieds project
# started 20.03.24


# If the exact value measured during the 2mn measurments are needed, they can be
# accessed with the “extraction_gaz.R“ script

# Library -----------------------------------------------------------------
library(readr)
library(tidyr)
library(dplyr)
library(stringr)

rm(list=ls())

# Data --------------------------------------------------------------------
gas <- read_csv("processed_data/gas_data.csv")

bad_r2 <- gas[gas$r2 < 0.75,] #Check wich data have a bad r2
gas_good <-gas[gas$r2 > 0.75,]# keep only data with r2> 0.75

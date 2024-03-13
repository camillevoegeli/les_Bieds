# Camille Voegeli
# Script made to clean the raw data to usable data
# Made for Les Bieds project
# started 11.01.24


# THIS IS USED WITH DATA EXTRACTED FROM SOILFLUXPRO, WHICH ARE NOT
# IDENTICAL TO THE ONE DIRECTLY FROM THE CHAMBER

# Library -----------------------------------------------------------------
library(readr)
library(tidyr)
library(dplyr)
library(stringr)

rm(list=ls())

# Data --------------------------------------------------------------------
gas <- read_csv("raw_data/gas_bieds.csv")
units <- gas[c(1:2),]
colnames(gas) <- gas[1,]
gas <- gas[-c(1:2),]

colnames(gas) <- c("DOY", "date_time", "PA", "T_PA", "altitude", "TA", "el_time",
                   "ch4_dry", "CO2_dry", "h20", "TS_1", "EC_2", "SWC_2", "TS_2", 
                   "timestamp","volume_tot", "latitude", "longitude", "comment", 
                   "label", "C02_deadband", "Fch4", "Fch4_dCdt", "Fch4_r2", 
                   "Fch4_cv", "Fch4_c0","Fch4_cx","Fch4_a", "Fch4_t0", "Fch4_sei",
                   "Fch4_ses", "Fch4_lin", "Fch4_lin_dCdt", "Fch4_lin_r2", 
                   "Fch4_lin_cv", "Fch4_lin_se", "Fch4_lin_ssn", "Fco2_c0", 
                   "Fco2", "Fco2_dCdt", "Fco2_r2", "Fco2_cv", "Fco2_cx",
                   "Fco2_a", "Fco2_t0", "Fco2_sei", "Fco2_ses","Fco2_lin", 
                   "Fco2_lin_dCdt", "Fco2_lin_r2", "Fco2_lin_cv", "Fco2_lin_se",
                   "Fco2_lin_ssn")

gas <- gas[,-c(7,11,15,19,21,31,53)] #these columns are redundant or not measured
gas <- gas %>% relocate("label")
gas[,c(2,4:46)] <- lapply(gas[,c(2,4:46)], as.numeric)

gas <- gas %>% separate( date_time, c("date","time"), sep=" ", remove=T) %>% #split the datetime column 
  mutate(type = ifelse(str_detect(label, "nee"), "nee", "respi")) #create new column : 
#detect if nee is in the label and write it in the new type column, otherwise write respi

# double measure ----------------------------------------------------------
# this code is to select only the last measure when we had to repeat it on a plot
gas <- separate(gas,label,c("label","Try"),sep = "_(?=[0-9])", remove = FALSE) %>% #split LABEl column when there is _X
  mutate(Try = as.numeric(Try)) %>% 
  replace_na(list(Try=0)) %>% 
  group_by(label,date) %>% 
  slice_max(Try) %>% 
  select(-Try) %>% 
  mutate(label = gsub("2$", "", label)) #somehow the code above also select 2nd repetition
# without the "_" before 2. This line remove the 2 from the label

# write_csv(gas, "processed_data/gas_data_pr.csv")


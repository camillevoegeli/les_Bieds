# script made for the analysis of Les Bieds data
# project lead by l'Azuré 
# Camille Voegeli
# 12.12.2023


# library -----------------------------------------------------------------

library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

rm(list=ls())

# Data --------------------------------------------------------------------
gas_bieds <- read_csv("raw_data/gas_bieds.csv")
colnames(gas_bieds) <- gas_bieds[1,]

env <- gas_bieds[,c(1:2,5:6,10:12,15,25)] #select interesting columns
units <- env[c(1,2),] #to keep units somewhere
env <- env[-c(1,2),] %>% select(LABEL, everything()) #second part to move the label column to first position
env[, c(2, 4:9)] <- lapply(env[, c(2, 4:9)], as.numeric) # apply as.numeric to the columns that need it
colnames(env)[colnames(env) == "DOY initial_value"] <- "DOY" #change column name
colnames(env)[colnames(env) == "DATE_TIME initial_value"] <- "datetime"

env <- env %>% separate( datetime, c("date","time"), sep=" ", remove=T) %>% #split the datetime column 
  mutate(type = ifelse(str_detect(LABEL, "nee"), "nee", "respi")) %>% #create new column : 
  #detect if nee is in the label and write it in the new type column, otherwise write respi
  mutate(ID = c(1:263)) # add number under the new ID column

#remove first measurment when it was taken twice
env <- separate(env,LABEL,c("plot","Try"),sep = "_(?=[0-9])", remove = FALSE) %>% #split LABEl column when there is _X
  mutate(Try = as.numeric(Try)) %>% 
  replace_na(list(Try=0)) %>% 
  group_by(plot,date) %>% 
  slice_max(Try) #when a sample has the same plot and date, keep only the one with higher Try

nee <- filter(env, type=="nee") #give nee data
resp <- filter(env, type=="respi")#give respi data
# outliers ----------------------------------------------------------------

boxplot(nee$FCH4_DRY)
outliers <- identify(x = rep(1, length(nee$FCH4_DRY)), y = nee$FCH4_DRY, #to identify outliers
                     labels = nee$ID)
nee <- nee[!(nee$ID %in% c("248", "251", "253")), ] #remove detected outliers

boxplot(nee$FCH4_DRY)

boxplot(resp$FCH4_DRY)
outliers <- identify(x = rep(1, length(resp$FCH4_DRY)), y = resp$FCH4_DRY,
                     labels = resp$ID)
resp<- resp[!(resp$ID %in% c("1","252","250")),]
# graphs ------------------------------------------------------------------


g_nee <- ggplot(nee, (aes(x=DOY, y=FCH4_DRY, color= plot))) +
  geom_line() +
  theme_minimal()
g_nee

g_resp <- ggplot(resp, (aes(x=DOY, y=FCH4_DRY, color= plot))) +
  geom_line() +
  theme_minimal()
g_resp

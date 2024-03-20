# Camille Voegeli
# Gas analysis of Les Bieds sites
# project lead by L'Azurée, with Lin'éco
# 01.02.2024


# library -----------------------------------------------------------------
library(readr)
library(tidyverse)
library(RColorBrewer)
library(readxl)

rm(list=ls())

# data
gas <- read_csv("processed_data/gas_data.csv")
env <- read_excel("raw_data/environment.xlsx")

pal <- c("aquamarine","brown","aquamarine4","gold","blue", "bisque3",
           "blueviolet","aquamarine3" ,"brown1", "cadetblue1", "chartreuse", 
           "chocolate1", "darkgreen","darkorchid1", "bisque", "limegreen", 
           "magenta","olivedrab3")

# cleaning ----------------------------------------------------------------

gas <- gas[gas$r2 > 0.75,]


## separate as list -------------------------------------------------------
# for easier manipulation, create list with different object for respi and nee
# this creates nested lists (lists inside lists)

# Extract unique gas and protocol types
gas_types <- unique(gas$gas)
protocol_types <- unique(gas$protocol)

# Initialize lists
data <- lapply(gas_types, function(g) {
  setNames(lapply(protocol_types, function(p) {
    gas[gas$gas == g & gas$protocol == p, ]
  }), protocol_types)
})
# Set the main list names
names(data) <- gas_types

# Convert plot column to character type in nee and respi for both co2 and ch4
for (gas in c("co2", "ch4")) { #these 2 line allow to access to the same column in all objects of the list
  for (process in c("nee", "respi")) {
    data[[gas]][[process]]$plot <- as.character(data[[gas]][[process]]$plot)
  }
}

#View(data$ch4$respi) #to see these data in full


# Analysis ----------------------------------------------------------------

## Graphs ------------------------------------------------------------------
##### Lineplots #####
#see md file for comments on graphs
g_all_r_co2 <- ggplot(data$co2$respi, aes(x=date, y= F_o, colour=plot)) +
  geom_line() +
  geom_point() + 
  scale_color_manual(values = pal) +
  labs(title=expression("CO"[2]*" respiration, toutes les placettes"), 
       x="date", y=expression("CO"[2]*" (µmol m"^{-2}*"s"^{-1}*")"))

g_all_r_co2
  
#grouped by site
g_all_r_co2_s <- ggplot(data$co2$respi, aes(x=date, y= F_o, colour=site)) +
  geom_point() +
  geom_smooth(se=F)+
  labs(title=expression("CO"[2]*" respiration, par parcelle"), 
       x="date", y=expression("CO"[2]*" (µmol m"^{-2}*"s"^{-1}*")")) +
  scale_color_manual(values = pal)
g_all_r_co2_s

g_all_n_co2 <- ggplot(data$co2$nee, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point() + 
  scale_color_manual(values = pal)+
  labs(title=expression("CO"[2]*" NEE, toutes les placettes"), 
       x="date", y=expression("CO"[2]*" (µmol m"^{-2}*"s"^{-1}*")")) 
g_all_n_co2

#grouped by site
g_all_n_co2_s <- ggplot(data$co2$nee, aes(x=date, y= F_o, colour=site)) +
  geom_point() +
  geom_smooth(se=F) +
  labs(title=expression("CO"[2]*" NEE, par parcelle"), 
       x="date", y=expression("CO"[2]*" (µmol m"^{-2}*"s"^{-1}*")")) +
  scale_color_manual(values = pal)

g_all_n_co2_s


##ch4####
g_all_r_ch4 <- ggplot(data$ch4$respi, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point()+ 
  scale_color_manual(values = pal) +
  labs(title=expression("CH"[4]*", respiration, toutes les placettes"), 
       x="date", y=expression("CH"[2]*" (nmol m"^{-2}*"s"^{-1}*")"))

g_all_r_ch4

# by site
g_all_r_ch4_s <- ggplot(data$ch4$respi, aes(x=date, y= F_o, color= site))+
  geom_point() +
  geom_smooth(se=F)+ 
  scale_color_manual(values = pal) +
  labs(title=expression("CH"[4]*", respiration, par parcelle"), 
       x="date", y=expression("CH"[2]*" (nmol m"^{-2}*"s"^{-1}*")"))
g_all_r_ch4_s

g_all_n_ch4 <- ggplot(data$ch4$nee, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point()+ 
  scale_color_manual(values = pal) +
  labs(title=expression("CH"[4]*", NEE, toutes les placettes"), 
       x="date", y=expression("CH"[2]*" (nmol m"^{-2}*"s"^{-1}*")"))
g_all_n_ch4

#### Without May ####
gas_no_may <- list()

for (gas in c("co2", "ch4")) {
  for (process in c("nee", "respi")) {
    gas_no_may[[paste(gas, process, sep = "_")]] <- data[[gas]][[process]][data[[gas]][[process]]$date != "2023-05-31", ]
  }
}

g_all_r_ch4_may <- ggplot(gas_no_may$ch4_respi, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point()
g_all_r_ch4_may

g_all_n_ch4 <- ggplot(gas_no_may$ch4_nee, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point()
g_all_n_ch4

#### boxplot ####

b_all_r_co2 <- ggplot(data$co2$respi, aes(x=date, y= F_o,group = date))+
  geom_boxplot() +
  geom_jitter(aes(colour=plot))
b_all_r_co2

b_all_n_co2 <- ggplot(data$co2$nee, aes(x=date, y= F_o, group=date))+
  geom_boxplot() +
  geom_jitter(aes(colour=plot))
b_all_n_co2

b_all_r_ch4 <- ggplot(gas_no_may$ch4_respi, aes(x=date, y= F_o, group= date))+
  geom_boxplot() +
  geom_jitter(aes(colour=plot))
b_all_r_ch4

b_all_n_ch4 <- ggplot(gas_no_may$ch4_nee, aes(x=date, y= F_o, group= date))+
  geom_boxplot() +
  geom_jitter(aes(colour=plot))
b_all_n_ch4


# PAR ---------------------------------------------------------------------
env_filtered <- env %>% filter(!is.na(luminosité))

g_par <- ggplot(env_filtered, aes(x = date, y = luminosité, group = date)) +
  geom_boxplot() +
  geom_jitter(aes(colour = plot))

g_par




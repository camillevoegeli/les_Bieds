# Camille Voegeli
# Gas analysis of Les Bieds sites
# project lead by L'Azurée, with Lin'éco
# 01.02.2024


# library -----------------------------------------------------------------
library(readr)
library(tidyverse)
library(RColorBrewer)
library(readxl)
library(ggthemes)


# data
gas_data <- read_csv("processed_data/gas_data.csv")
env <- read_excel("raw_data/environment.xlsx")

pal <- c("1" =  "aquamarine","2"="brown","3"="aquamarine4","4"="gold","5"= "blue",
         "6" = "bisque3","7"= "blueviolet","8"= "aquamarine3" ,"9"="brown1", 
         "10" ="cadetblue1", "11" = "chartreuse","12"= "chocolate1",
         "13"="darkgreen","14"="darkorchid1", "15"= "darkblue", "16"="limegreen", 
          "17" = "magenta","18"= "olivedrab3")

pal_site <- c("2073"= "red", "2517"= "blue", "2215"= "green", "HM"= "orange", 
              "PL" = "violet")

gas_data$plot <- factor(gas_data$plot, levels = 1:18)
env$plot <- gsub("^BIE(\\d+).*", "\\1", env$plot)

env$site <- ifelse(env$plot %in% c("1", "2", "3", "4", "13", "14"), "2215",
                   ifelse(env$plot %in% c("5", "6", "7","8","9"), "2517",
                          ifelse(env$plot %in% c("10", "11", "12"), "2073",
                                 ifelse(env$plot %in% c("15", "16"), "HM",
                                 "PL"))))
# cleaning ----------------------------------------------------------------
bad <- gas_data[gas_data$r2 < 0.75,]
gas_data <- gas_data[gas_data$r2 > 0.75,]


## separate as list -------------------------------------------------------
# for easier manipulation, create list with different object for respi and nee
# this creates nested lists (lists inside lists)

# Extract unique gas and protocol types
gas_types <- unique(gas_data$gas)
protocol_types <- unique(gas_data$protocol)

# Initialize lists
data <- lapply(gas_types, function(g) {
  setNames(lapply(protocol_types, function(p) {
    gas_data[gas_data$gas == g & gas_data$protocol == p, ]
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


# Function to convert 'plot' column to factor with specified levels
plot_to_factor <- function(df) {
  df$plot <- factor(df$plot, levels = 1:18)  # Specify levels from 1 to 18
  return(df)
}

# Apply function to all dataframes within nested list structure using lapply()
data <- lapply(data, function(lst) {
  lapply(lst, plot_to_factor)
})

#View(data$ch4$respi) #to see these data in full


# Analysis ----------------------------------------------------------------

summary(data$co2$nee$F_o)
summary(data$co2$respi$F_o)

summary(data$ch4$respi$F_o)
summary(data$ch4$nee$F_o)

## Graphs ------------------------------------------------------------------
##### all sites#####
#see md file for comments on graphs
g_all_r_co2 <- ggplot(data$co2$respi, aes(x=date, y= F_o, colour=plot)) +
  geom_line() +
  geom_point() + 
  scale_color_manual(values = pal) +
  labs(title=expression("CO"[2]*" respiration, toutes les placettes"), 
       x="date", y=expression("CO"[2]*" (µmol m"^{-2}*"s"^{-1}*")"))+
  theme_pander()

g_all_r_co2
# ggsave(filename="co2_respi.png", plot= g_all_r_co2,path = "figures/graphs/all_plots/")
  
#grouped by site (regression LOESS)
g_all_r_co2_s <- ggplot(data$co2$respi, aes(x=date, y= F_o, colour=site)) +
  geom_point() +
  geom_smooth(se=F)+
  labs(title=expression("CO"[2]*" respiration, par parcelle"), 
       x="date", y=expression("CO"[2]*" (µmol m"^{-2}*"s"^{-1}*")")) +
  scale_color_manual(values = pal_site)+
  theme_pander()
g_all_r_co2_s
# ggsave(filename="co2_respi_loess.png", plot= g_all_r_co2_s,path = "figures/graphs/all_plots/")


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
  scale_color_manual(values = pal_site)

g_all_n_co2_s


##ch4####
g_all_r_ch4 <- ggplot(data$ch4$respi, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point()+ 
  scale_color_manual(values = pal) +
  labs(title=expression("CH"[4]*", respiration, toutes les placettes"), 
       x="date", y=expression("CH"[4]*" (nmol m"^{-2}*"s"^{-1}*")"))+
  theme_pander()

g_all_r_ch4

# by site
g_all_r_ch4_s <- ggplot(data$ch4$respi, aes(x=date, y= F_o, color= site))+
  geom_point() +
  geom_smooth(se=F)+ 
  scale_color_manual(values = pal_site) +
  labs(title=expression("CH"[4]*", respiration, par parcelle"), 
       x="date", y=expression("CH"[4]*" (nmol m"^{-2}*"s"^{-1}*")"))
g_all_r_ch4_s

g_all_n_ch4 <- ggplot(data$ch4$nee, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point()+ 
  scale_color_manual(values = pal) +
  labs(title=expression("CH"[4]*", NEE, toutes les placettes"), 
       x="date", y=expression("CH"[4]*" (nmol m"^{-2}*"s"^{-1}*")"))
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

g_all_n_ch4_may <- ggplot(gas_no_may$ch4_nee, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point()
g_all_n_ch4_may

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


# temperature -------------------------------------------------------------
temp <- env %>% filter(!is.na(`T°C a 30 cm`))

g_temp <- ggplot(temp[temp$méthode == "nee",], aes(x = date, y = `T°C a 30 cm`, color = plot)) +
  geom_line(aes(group= plot)) +
  geom_point()+
  scale_color_manual(values = pal) +
  ylab("T°C a 30 cm")
g_temp

temp_surf <- gas_data %>%
  filter(gas == "co2" & protocol == "respi")
temp_surf$plot <- factor(temp_surf$plot)
g_temp_surf <- ggplot(temp_surf,aes(x= date, y= soilp_t, colour = plot))+ 
  geom_line(aes(group= plot)) +
  geom_point() +
  scale_color_manual(values = pal)+
  ylab("T°C sol en surface")
g_temp_surf

g_temp_loess <- ggplot(temp[temp$méthode == "nee",], aes(x = date, y = `T°C a 30 cm`, color = site)) +
  geom_point()+
  geom_smooth(se=F)+
  scale_color_manual(values = pal_site) +
  ylab("T°C a 30 cm")
g_temp_loess
# ggsave(filename="temp_prof_loess.png", plot= g_temp_loess,path = "figures/graphs/all_plots/")


g_temp_surf_loess <- ggplot(temp_surf,aes(x= date, y= soilp_t, colour = site))+ 
  geom_point() +
  geom_smooth(se=F)+
  scale_color_manual(values = pal_site)+
  ylab("T°C sol en surface")
g_temp_surf_loess
# ggsave(filename="temp_surf_loess.png", plot= g_temp_surf_loess,path = "figures/graphs/all_plots/")

# Per site -----------------------------------------------------------------

## 2073 --------------------------------------------------------------------
b_2073 <- list()

for (gas in c("co2", "ch4")) {
  for (process in c("nee", "respi")) {
    b_2073[[paste(gas, process, sep = "_")]] <- data[[gas]][[process]][data[[gas]][[process]]$site == "2073", ]
  }
}

#ch4 
g_2073_r_ch4 <- ggplot(b_2073$ch4_respi, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point()+
  scale_color_manual(values = pal) +
  labs(title=expression("CH"[4]*", respiration, 2073"), 
       x="date", y=expression("CH"[4]*" (nmol m"^{-2}*"s"^{-1}*")"))
g_2073_r_ch4

g_2073_n_ch4 <- ggplot(b_2073$ch4_nee, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point()+
  scale_color_manual(values = pal) +
  labs(title=expression("CH"[4]*", NEE, 2073"), 
       x="date", y=expression("CH"[4]*" (nmol m"^{-2}*"s"^{-1}*")"))
g_2073_n_ch4

#without may
no_may_2073 <- lapply(gas_no_may, function(x) {
  # Filter rows where "site" column equals "2073"
  x[x$site == "2073", ]
})

g_2073_r_ch4_may <- ggplot(no_may_2073$ch4_respi, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point() +
  scale_color_manual(values = pal) +
  labs(title=expression("CH"[4]*", respiration, 2073"), 
       x="date", y=expression("CH"[4]*" (nmol m"^{-2}*"s"^{-1}*")"))
g_2073_r_ch4_may

g_2073_n_ch4_may <- ggplot(no_may_2073$ch4_nee, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point()+
  scale_color_manual(values = pal) +
  labs(title=expression("CH"[4]*", NEE, 2073"), 
       x="date", y=expression("CH"[4]*" (nmol m"^{-2}*"s"^{-1}*")"))
g_2073_n_ch4_may

#co2
g_2073_r_co2 <- ggplot(b_2073$co2_respi, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point() +
  labs(title=expression("CO"[2]*" respiration, 2073"), 
       x="date", y=expression("CO"[2]*" (µmol m"^{-2}*"s"^{-1}*")")) +
  scale_color_manual(values = pal)
g_2073_r_co2

g_2073_n_co2 <- ggplot(b_2073$co2_nee, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point() +
  labs(title=expression("CO"[2]*" NEE, 2073"), 
       x="date", y=expression("CO"[2]*" (µmol m"^{-2}*"s"^{-1}*")")) +
  scale_color_manual(values = pal)
g_2073_n_co2


## 2517 --------------------------------------------------------------------
b_2517 <- list()

for (gas in c("co2", "ch4")) {
  for (process in c("nee", "respi")) {
    b_2517[[paste(gas, process, sep = "_")]] <- data[[gas]][[process]][data[[gas]][[process]]$site == "2517", ]
  }
}

#ch4 
g_2517_r_ch4 <- ggplot(b_2517$ch4_respi, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point()+
  scale_color_manual(values = pal) +
  labs(title=expression("CH"[4]*", respiration, 2517"), 
       x="date", y=expression("CH"[4]*" (nmol m"^{-2}*"s"^{-1}*")"))
g_2517_r_ch4

g_2517_n_ch4 <- ggplot(b_2517$ch4_nee, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point()+
  scale_color_manual(values = pal) +
  labs(title=expression("CH"[4]*", NEE, 2517"), 
       x="date", y=expression("CH"[4]*" (nmol m"^{-2}*"s"^{-1}*")"))
g_2517_n_ch4

#CO2
g_2517_r_co2 <- ggplot(b_2517$co2_respi, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point()+
  scale_color_manual(values = pal) +
  labs(title=expression("CO"[2]*", respiration, 2517"), 
       x="date", y=expression("CO"[2]*" (µmol m"^{-2}*"s"^{-1}*")"))
g_2517_r_co2

g_2517_n_co2 <- ggplot(b_2517$co2_nee, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point()+
  scale_color_manual(values = pal) +
  labs(title=expression("CO"[2]*", NEE, 2517"), 
       x="date", y=expression("CO"[2]*" (µmol m"^{-2}*"s"^{-1}*")"))
g_2517_n_co2


## 2215 --------------------------------------------------------------------
b_2215 <- list()

for (gas in c("co2", "ch4")) {
  for (process in c("nee", "respi")) {
    b_2215[[paste(gas, process, sep = "_")]] <- data[[gas]][[process]][data[[gas]][[process]]$site == "2215", ]
  }
}
#ch4 
g_2215_r_ch4 <- ggplot(b_2215$ch4_respi, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point()+
  scale_color_manual(values = pal) +
  labs(title=expression("CH"[4]*", respiration, 2215"), 
       x="date", y=expression("CH"[4]*" (nmol m"^{-2}*"s"^{-1}*")"))
g_2215_r_ch4

g_2215_n_ch4 <- ggplot(b_2215$ch4_nee, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point()+
  scale_color_manual(values = pal) +
  labs(title=expression("CH"[4]*", NEE, 2215"), 
       x="date", y=expression("CH"[4]*" (nmol m"^{-2}*"s"^{-1}*")"))
g_2215_n_ch4

#CO2
g_2215_r_co2 <- ggplot(b_2215$co2_respi, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point()+
  scale_color_manual(values = pal) +
  labs(title=expression("CO"[2]*", respiration, 2215"), 
       x="date", y=expression("CO"[2]*" (µmol m"^{-2}*"s"^{-1}*")"))
g_2215_r_co2

g_2215_n_co2 <- ggplot(b_2215$co2_nee, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point()+
  scale_color_manual(values = pal) +
  labs(title=expression("CO"[2]*", NEE, 2215"), 
       x="date", y=expression("CO"[2]*" (µmol m"^{-2}*"s"^{-1}*")"))
g_2215_n_co2

## HM --------------------------------------------------------------------
b_HM <- list()

for (gas in c("co2", "ch4")) {
  for (process in c("nee", "respi")) {
    b_HM[[paste(gas, process, sep = "_")]] <- data[[gas]][[process]][data[[gas]][[process]]$site == "HM", ]
  }
}
#ch4 
g_HM_r_ch4 <- ggplot(b_HM$ch4_respi, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point()+
  scale_color_manual(values = pal) +
  labs(title=expression("CH"[4]*", respiration, HM"), 
       x="date", y=expression("CH"[4]*" (nmol m"^{-2}*"s"^{-1}*")"))
g_HM_r_ch4

g_HM_n_ch4 <- ggplot(b_HM$ch4_nee, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point()+
  scale_color_manual(values = pal) +
  labs(title=expression("CH"[4]*", NEE, HM"), 
       x="date", y=expression("CH"[4]*" (nmol m"^{-2}*"s"^{-1}*")"))
g_HM_n_ch4

#CO2
g_HM_r_co2 <- ggplot(b_HM$co2_respi, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point()+
  scale_color_manual(values = pal) +
  labs(title=expression("CO"[2]*", respiration, HM"), 
       x="date", y=expression("CO"[2]*" (µmol m"^{-2}*"s"^{-1}*")"))
g_HM_r_co2

g_HM_n_co2 <- ggplot(b_HM$co2_nee, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point()+
  scale_color_manual(values = pal) +
  labs(title=expression("CO"[2]*", NEE, HM"), 
       x="date", y=expression("CO"[2]*" (µmol m"^{-2}*"s"^{-1}*")"))
g_HM_n_co2

## PL--------------------------------------------------------------------
b_PL <- list()

for (gas in c("co2", "ch4")) {
  for (process in c("nee", "respi")) {
    b_PL[[paste(gas, process, sep = "_")]] <- data[[gas]][[process]][data[[gas]][[process]]$site == "PL", ]
  }
}
#ch4 
g_PL_r_ch4 <- ggplot(b_PL$ch4_respi, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point()+
  scale_color_manual(values = pal) +
  labs(title=expression("CH"[4]*", respiration, PL"), 
       x="date", y=expression("CH"[4]*" (nmol m"^{-2}*"s"^{-1}*")"))
g_PL_r_ch4

g_PL_n_ch4 <- ggplot(b_PL$ch4_nee, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point()+
  scale_color_manual(values = pal) +
  labs(title=expression("CH"[4]*", NEE, PL"), 
       x="date", y=expression("CH"[4]*" (nmol m"^{-2}*"s"^{-1}*")"))
g_PL_n_ch4

#CO2
g_PL_r_co2 <- ggplot(b_PL$co2_respi, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point()+
  scale_color_manual(values = pal) +
  labs(title=expression("CO"[2]*", respiration, PL"), 
       x="date", y=expression("CO"[2]*" (µmol m"^{-2}*"s"^{-1}*")"))
g_PL_r_co2

g_PL_n_co2 <- ggplot(b_PL$co2_nee, aes(x=date, y= F_o, color= plot))+
  geom_line() +
  geom_point()+
  scale_color_manual(values = pal) +
  labs(title=expression("CO"[2]*", NEE, PL"), 
       x="date", y=expression("CO"[2]*" (µmol m"^{-2}*"s"^{-1}*")"))
g_PL_n_co2


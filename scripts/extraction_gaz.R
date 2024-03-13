#Script to import gas data from Licor
#Adapted from a script by Isacco Darini made in July 2022
#September 2023
#Camille Voegeli

# Description -------------------------------------------------------------

# This script contains all necessary elements to transform the deeply nested
# JavaScript objects obtained from the Li-COR automatic gas sampler into a
# tidy tibble in R.
# Description of the original .json file structure and variables meaning can be 
# found here:
# https://www.licor.com/env/support/Smart-Chamber/topics/data-files.html



# library -----------------------------------------------------------------
library(readxl)
library(tidyverse)
library(jsonlite)
library(rstudioapi)
library(dplyr)
library(purrr)
library(lubridate)
library(tidyr)
library(stringr)
library(readr)
library(ggplot2)
library(rioja)
library(vegan)
library(tibble)
library(forcats)

'%!in%' <- function(x,y)!('%in%'(x,y))

# Function ----------------------------------------------------------------

tidy_gas_json <- function(json) {
  
  data_tb <- as_tibble(json)
  
  data_unnest <- 
    data_tb %>% 
    unnest_longer(datasets) %>%     #unnest alows to go to long format             
    unnest_wider(datasets, 
                 names_repair = "universal") %>%  
    unnest_longer(reps, 
                  names_repair = "universal") %>% 
    unnest_wider(reps, 
                 names_repair = "universal") %>% 
    select(-labels) %>% 
    unnest_wider(header, 
                 names_repair = "universal") %>%
    rowwise() %>%
    mutate(across(c(data, summary), ~ list(as_tibble(.x)))) %>%
    # just take the mean of the summary variables
    # for some reason they are strings, so they also need conversion
    mutate(summary = list(slice(
      mutate(summary, across(everything(), ~ as.numeric(.x))), 
      2))) %>% 
    unnest_wider(summary,
                 names_repair = "universal") %>% 
    unnest_wider(footer,
                 names_repair = "universal") %>% 
    unnest_longer(fluxes,
                  names_repair = "universal") %>% 
    unnest_wider(fluxes,
                 names_repair = "universal")
  
}


# Multiple files example --------------------------------------------------

tidy_multi_tb <- 
  list.files("raw_data/json_2023") %>% 
  map(~ read_json(file.path("raw_data/json_2023", .x))) %>% 
  map(tidy_gas_json) %>% 
  bind_rows()

# Cleaning ----------------------------------------------------------------
#add DOY
tidy_multi_tb$DOY <- as.numeric(format(as.POSIXct(tidy_multi_tb$Date, format="%Y-%m-%d %H:%M:%S"), "%j")) +
  as.numeric(format(as.POSIXct(tidy_multi_tb$Date, format="%Y-%m-%d %H:%M:%S"), "%H")) / 24 +
  as.numeric(format(as.POSIXct(tidy_multi_tb$Date, format="%Y-%m-%d %H:%M:%S"), "%M")) / (24 * 60)

tidy_multi_tb$datasets_id <- gsub("nee2", "nee_2", tidy_multi_tb$datasets_id)
tidy_multi_tb$datasets_id <- gsub("respi2", "respi_2", tidy_multi_tb$datasets_id)

data <- tidy_multi_tb %>%
  drop_na(F_o) %>% 
  #Extract Site (and date) from name...1
  separate(name...1, c("site","day"), sep = "_",
           remove = TRUE, convert = TRUE) %>% 
  #extract Plot and Test from datasest_id
  separate(datasets_id,c("plot","protocol"),sep = "_",
           remove = FALSE, convert = FALSE) %>%  #Errors come from iteration number
  separate(plot, c("site", "plot"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])",
           remove = TRUE)%>% #Transform dates into a usable format :
  mutate(date = as_datetime(date)) %>% 
  select(!day) %>% 
  separate(Date, c("date", "time"), sep = " ", remove = T,) %>% 
  rename(gas = name...40)

# write_csv(data, "raw_data/full_dataset_Bieds.csv")

data_select <- data %>% 
  select("date","time","latitude","longitude","altitude","soilp_t","gas","F_o",
         "F_cv","r2", "datasets_id", "site","plot","protocol", "DOY")

# this code is to select only the last measure when we had to repeat it on a plot
data_select <- separate(data_select,datasets_id,c("label","Try"),sep = "_(?=[0-9])", remove = FALSE) %>% #split LABEl column when there is _X
  mutate(Try = as.numeric(Try)) %>% 
  replace_na(list(Try=0)) %>% 
  group_by(label,date) %>% 
  slice_max(Try) %>% 
  select(-Try)%>% 
  select(!c("datasets_id","site"))
    #somehow the code above also select 2nd repetition
  # without the "_" before 2. This line remove the 2 from the label

data_select <- data_select %>%
  mutate(protocol = ifelse(protocol == "ne", "nee", protocol))

data_select <- data_select %>%
  mutate(site = ifelse(plot %in% c(5:9), "2517",
                       ifelse(plot %in% c(1:4, 13:14), "2215",
                              ifelse(plot %in% c(10:12), "2073",
                                     ifelse(plot %in% c(15:16), "HM",
                                            ifelse(plot %in% c(17:18), "PL", "")))))
  )



write_csv(data_select, "processed_data/gas_data.csv")





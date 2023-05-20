## EMU: Pull US unemployment rates
## John Kearns
## May 2022

## load packages ----------------

library(Quandl)
library(tidyverse)

## load state abbrvs ------------

master_dir = "C:/Users/16094/Documents/Year 1 - Princeton/Fall 2022/Fintech/Project/Fintech-Trends-and-State-and-Local-Finance/"
folder = master_dir
do_folder = paste0(master_dir,"Do/")
data_folder = paste0(master_dir,"Data/")
results_folder = paste0(master_dir,"Results/")
charts_folder = paste0(master_dir,"Charts/")

state_codes <- read.csv(file = paste0(folder, "Data/fips_codes.csv"), stringsAsFactors = FALSE)

## loop through states ----------

# get rid of territories

state_abrv <- state_codes %>%
  filter(fips<60)

state_unemp <- NULL

for(i in 1:51){
  state <- state_abrv[i,2]
  series <- Quandl(paste0("FRED/", state, "UR"), api_key="6zd6MoTaa7CBpgaZxBSH")

  series <- series %>%
    mutate(stabrv = state)
  state_unemp <- bind_rows(state_unemp, series)

  print(i/51)
}

state_civlf <- NULL

for(i in 1:51){
  state <- state_abrv[i,2]
  series <- Quandl(paste0("FRED/", state, "LFN"), api_key="6zd6MoTaa7CBpgaZxBSH")

  series <- series %>%
    mutate(stabrv = state,
           units = "thousands")
  state_civlf <- bind_rows(state_civlf, series)

  print(i/51)
} # thousands of persons

write.csv(state_unemp, file = paste0(folder, "Data/Raw/BLS/state_unemp.csv"))
write.csv(state_civlf, file = paste0(folder, "Data/Raw/BLS/state_civlf.csv"))

# Code to generate provincial and regional-scale data needed to generate
# summaries for the Pacific Salmon Explorer

rm(list = ls(all=TRUE)); #Remove all the objects in the memory
# One time command to set the ProgRegSummaries as the highest level directory
#here::set_here()
setwd(here::here())
#setwd("~/Dropbox (Salmon Watersheds)/X Drive/1_PROJECTS/Population Methods and Analysis/ProgRegSummaries/data")
# Delete plyr and reshape3 if not loading them doesn't break your code lol, load as necessary
#library(plyr)
#require(reshape2)  # see if you even need this anymore
library(tidyverse)  # good default load capturing ggplot2 and dplyr
library(broom)
library(geosphere)



#read in CU level data
cc_file <- read.csv("data/dataset_1part1.Dec072020_CC.csv", header = T)
fraser_file <- read.csv("data/dataset_1part1.Jul282020_Fraser.csv", header = T)
vimi_file <- read.csv("data/dataset_1part1.Dec082020_VIMI.csv", header = T)
nass_file <- read.csv("data/dataset_1part1.Dec092020_Nass.csv", header = T)
skeena_file <- read.csv("data/dataset_1part1.Dec092020_Skeena.csv", header = T)

# combine files from each region
cu_dat <- rbind(cc_file,fraser_file,vimi_file,nass_file,skeena_file)
cu_dat <- select(cu_dat,CUID,Species,Year,Total.run,Region)

#check species names
unique(cu_dat$Species)
cu_dat$Species[cu_dat$Species=="River Sockeye"] <- "Sockeye"
cu_dat$Species[cu_dat$Species=="Lake Sockeye"] <- "Sockeye"


# Attempt at selecting 80% runs?
# ------------------------------------------------------------------------------
cu_dat <- as_tibble(cu_dat)  # tibble print method is prettier

max_runs <- 
    cu_dat %>% 
    drop_na(Total.run) %>%  # tidyverse alternative for 
    group_by(CUID, Species) %>% 
    slice(which.max(Total.run)) %>% 
    dplyr::rename(max_run = Total.run) %>%
    ungroup()  # don't forget this step as this can bite you later

total_max_run <- 
  max_runs %>% 
  group_by(Species, Region) %>% 
  summarise(total_max_run = sum(max_run)) %>% 
  ungroup()


test <- 
cu_dat %>% 
  drop_na(Total.run) %>% 
  left_join(., max_runs %>% select(-Year)) %>% 
  left_join(., total_max_run) %>% 
  group_by(Year, Species, Region) %>% 
  arrange(Year, Species, Region, -max_run) %>% 
  mutate(c1 = cumsum(max_run)) %>%
  mutate(percent = c1 / total_max_run) %>%
  mutate(pop1 = lag(percent), pop_cur = percent) %>% 
  # Identify if there is a CU that hits the 80% cutoff
  mutate(cutoff = ifelse(pop_cur >= 0.8 & pop1 < 0.8, TRUE, FALSE)) %>% 
  ungroup() %>%
  # Reorder the columns to make checking easier
  select(CUID, Region, Species, Year, Total.run, c1, total_max_run, percent, pop1, pop_cur, cutoff)

years_min80 <- 
  test %>% 
    slice(which(cutoff == TRUE)) %>% 
    select(Species, Year, Region)

# Can lop off the c1:cutoff if you want
output <- left_join(years_min80, test)

# ------------------------------------------------------------------------------
  
#create provincial run size summary file 1, raw and relative run size by species, region, year

d1<- na.omit(cu_dat) %>% 
  group_by(., Species, Region, Year) %>% 
  summarise(prov_runsize_raw = sum(Total.run)) ## raw run size


dat2 <- d1 %>% 
  group_by(Species,Region) %>%
  mutate(prov_runsize_relative = (prov_runsize_raw / max(prov_runsize_raw))*100) #relative run size      

#setwd("~/Dropbox (Salmon Watersheds)/X Drive/1_PROJECTS/Population Methods and Analysis/ProgRegSummaries/output")
write.csv(dat2, "output/Prov_runsize_1_20210521.csv", row.names=FALSE)

#create provincial run size summary file 2, start year, end year, percent change

#setwd("~/Dropbox (Salmon Watersheds)/X Drive/1_PROJECTS/Population Methods and Analysis/ProgRegSummaries/data")

dat3 <- dat2 %>% group_by(Species,Region) %>%
  summarize(enddecade_year = max(Year))

dat3 <- dat3 %>% 
  mutate(startdecade_year = (enddecade_year-9)) #relative run size    

dat4 <- dat2 %>% 
  group_by(Species,Region) %>%
  summarise(change_lastdecade = ((mean(tail(prov_runsize_raw,10))-mean(prov_runsize_raw)) / mean(prov_runsize_raw))*100) #relative run size      

dat5 <- left_join(dat3, dat4, by=c("Region","Species"))

#setwd("~/Dropbox (Salmon Watersheds)/X Drive/1_PROJECTS/Population Methods and Analysis/ProgRegSummaries/output")
write.csv(dat2, "output/Prov_runsize_2_20210521.csv", row.names=FALSE)


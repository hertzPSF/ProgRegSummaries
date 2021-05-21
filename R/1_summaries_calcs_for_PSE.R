# Code to generate provincial and regional-scale data needed to generate
# summaries for the Pacific Salmon Explorer

rm(list = ls(all=TRUE)); #Remove all the objects in the memory
setwd("~/Dropbox (Salmon Watersheds)/X Drive/1_PROJECTS/Population Methods and Analysis/ProgRegSummaries/data")
library(ggplot2)
#library(plyr)
require(reshape2)
library(dplyr)
library(broom)
library(geosphere)



#read in CU level data
cc_file <- read.csv("dataset_1part1.Dec072020_CC.csv", header = T)
fraser_file <- read.csv("dataset_1part1.Jul282020_Fraser.csv", header = T)
vimi_file <- read.csv("dataset_1part1.Dec082020_VIMI.csv", header = T)
nass_file <- read.csv("dataset_1part1.Dec092020_Nass.csv", header = T)
skeena_file <- read.csv("dataset_1part1.Dec092020_Skeena.csv", header = T)

# combine files from each region
cu_dat <- rbind(cc_file,fraser_file,vimi_file,nass_file,skeena_file)
cu_dat <- select(cu_dat,CUID,Species,Year,Total.run,Region)

#check species names
unique(cu_dat$Species)
cu_dat$Species[cu_dat$Species=="River Sockeye"] <- "Sockeye"
cu_dat$Species[cu_dat$Species=="Lake Sockeye"] <- "Sockeye"

#create provincial run size summary file 1, raw and relative run size by species, region, year

d1<- na.omit(cu_dat) %>% 
  group_by(., Species, Region, Year) %>% 
  summarise(prov_runsize_raw = sum(Total.run)) ## raw run size


dat2 <- d1 %>% 
  group_by(Species,Region) %>%
  mutate(prov_runsize_relative = (prov_runsize_raw / max(prov_runsize_raw))*100) #relative run size      

setwd("~/Dropbox (Salmon Watersheds)/X Drive/1_PROJECTS/Population Methods and Analysis/ProgRegSummaries/output")
write.csv(dat2, "Prov_runsize_1_20210521.csv", row.names=FALSE)

#create provincial run size summary file 2, start year, end year, percent change

setwd("~/Dropbox (Salmon Watersheds)/X Drive/1_PROJECTS/Population Methods and Analysis/ProgRegSummaries/data")

dat3 <- dat2 %>% group_by(Species,Region) %>%
  summarize(enddecade_year = max(Year))

dat3 <- dat3 %>% 
  mutate(startdecade_year = (enddecade_year-9)) #relative run size    

dat4 <- dat2 %>% 
  group_by(Species,Region) %>%
  summarise(change_lastdecade = ((mean(tail(prov_runsize_raw,10))-mean(prov_runsize_raw)) / mean(prov_runsize_raw))*100) #relative run size      

dat5 <- left_join(dat3, dat4, by=c("Region","Species"))

setwd("~/Dropbox (Salmon Watersheds)/X Drive/1_PROJECTS/Population Methods and Analysis/ProgRegSummaries/output")
write.csv(dat2, "Prov_runsize_2_20210521.csv", row.names=FALSE)


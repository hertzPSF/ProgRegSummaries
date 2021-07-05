# Code to generate provincial and regional-scale data needed to generate
# summaries for the Pacific Salmon Explorer
# The code was originally written by Eric Hertz at the Pacific Salmon Foundation ehertz@psf.ca 

rm(list = ls(all=TRUE)); #Remove all the objects in the memory
setwd("~/Dropbox (Salmon Watersheds)/X Drive/1_PROJECTS/Population Methods and Analysis/ProgRegSummaries")
library(tidyverse)
library(broom)
library(geosphere)


#### 1 Read in and manipulateddata

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


#### 2 Generate files needed for Provincial summaries
#Select only the years that have CUs that comprise 80% of the maximum historical run size
# ------------------------------------------------------------------------------
cu_dat <- as_tibble(cu_dat) 

max_runs <- 
    cu_dat %>% 
    drop_na(Total.run) %>%  
    group_by(CUID, Species) %>% 
    slice(which.max(Total.run)) %>% 
    dplyr::rename(max_run = Total.run) %>%
    ungroup()  

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

test$cutoff[is.na(test$cutoff)] <- TRUE

years_min80 <- 
  test %>% 
    slice(which(cutoff == TRUE)) %>% 
    select(Species, Year, Region)

output <- left_join(years_min80, test)

# ------------------------------------------------------------------------------
  
#create provincial run size summary file 1, raw and relative run size by species, region, year

## create column for raw run size
d1<- na.omit(cu_dat) %>% 
  group_by(., Species, Region, Year) %>% 
  summarise(prov_runsize_raw = sum(Total.run)) 

## create column for relative run size
d2 <- d1 %>% 
  group_by(Species,Region) %>%
  mutate(prov_runsize_relative = (prov_runsize_raw / max(prov_runsize_raw))*100) #relative run size      

## join files so that only years with greater than 80% of historical CUs are shown
dat2 <- inner_join(d2,years_min80)

#setwd("~/Dropbox (Salmon Watersheds)/X Drive/1_PROJECTS/Population Methods and Analysis/ProgRegSummaries/output")

fraser_dat <- read.csv("output/Fraser_total_runsize.csv", header = T) # add in aggregated fraser data
dat2 <- rbind(dat2, fraser_dat)

dat2$Species[dat2$Species=="Pink (Odd)"] <- "Pink"
dat2$Species[dat2$Species=="Pink (Even)"] <- "Pink"

write.csv(dat2, "output/Prov_runsize_1_20210521.csv", row.names=FALSE)

#create provincial run size summary file 2, start year, end year, percent change


dat3 <- dat2 %>% group_by(Species,Region) %>%
  summarize(enddecade_year = max(Year))

dat3 <- dat3 %>% 
  mutate(startdecade_year = (enddecade_year-9)) #relative run size    

dat4 <- dat2 %>% 
  group_by(Species, Region) %>%
  summarise(change_lastdecade = ((mean(tail(prov_runsize_raw,10))-mean(prov_runsize_raw)) / mean(prov_runsize_raw))*100) #relative run size      

start_end_years <- 
  dat2 %>% 
  group_by(Species, Region) %>% 
  summarise(start_year = min(Year), end_year = max(Year), decade_start = max(Year) - 10)

dat4 <- dat2 %>% 
  left_join(., start_end_years) %>%
  mutate(time_chunk = case_when((Year >= start_year & Year < decade_start) ~ paste(start_year, decade_start, sep = ' - '), 
                                (Year >= decade_start) ~ paste(decade_start, end_year, sep = ' - '))) %>% 
  # to make calculating % change simpler, since year ranges are different for species - region combos
  mutate(time_chunk_id = case_when((Year >= start_year & Year < decade_start) ~ 'historical', 
                                (Year >= decade_start) ~ 'recent decade')) %>% 
  group_by(Species, Region, time_chunk, time_chunk_id) %>%
  summarise(mean_runsize = mean(prov_runsize_raw)) %>% 
  pivot_wider(id_cols = c(Species, Region), names_from = time_chunk_id, values_from = mean_runsize) %>% 
  mutate(change_lastdecade = ((`recent decade` - historical) / historical)*100)

dat5 <- left_join(dat3, dat4, by=c("Region","Species"))

dat5 <- select(dat5, Species, Region, enddecade_year, startdecade_year, change_lastdecade)

write.csv(dat5, "output/Prov_runsize_2_20210629.csv", row.names=FALSE)

#### 3 Generate files needed for regional summaries
#calculate average of run size for each region and species by year

## create column for raw run size
dat6<- na.omit(cu_dat) %>% 
  group_by(., Species, Region, Year) %>% 
  summarise(reg_runsize_raw = mean(Total.run)) 

## create column for relative run size
cu_dat2 <- na.omit(cu_dat) %>% 
  group_by(CUID) %>%
  mutate(runsize_relative = (Total.run / max(Total.run))*100)

dat7<- na.omit(cu_dat2) %>% 
  group_by(., Species, Region, Year) %>% 
  summarise(reg_runsize_relative = mean(runsize_relative)) 

dat8 <- left_join(dat6, dat7, by=c("Region","Species","Year"))

dat8 <- inner_join(dat8,years_min80) 

dat8$Species[dat8$Species=="Pink (Odd)"] <- "Pink"
dat8$Species[dat8$Species=="Pink (Even)"] <- "Pink"

write.csv(dat8, "output/Reg_runsize_1_20210525.csv", row.names=FALSE)


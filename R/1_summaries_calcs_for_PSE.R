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


d1<- na.omit(cu_dat) %>% 
  group_by(., Species, Region, Year) %>% 
  summarise(prov_runsize_raw = sum(Total.run))


###old code
ncc_co <- filter(abund_file, SpeciesId == 'CO')%>%
  select('Year', 'Total.Run')%>%
  na.omit()

d1<- group_by(ncc_co, Year) %>% 
  summarise(total = sum(Total.Run))


get_cor <- function(x, y, use = 'complete.obs') {
  cor_val <- cor(x, y, use)
  return(data_frame(cor_val = cor_val))
}

chum_r2 <-
  expanded_df%>%
  group_by(CU_Name.x, CU_Name.y) %>%
  do(get_cor(.$Total.Run.x, .$Total.Run.y, use = 'complete.obs')) %>%
  filter(CU_Name.x != CU_Name.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(CU_Name.x, CU_Name.y)), 
         max_pop = max(c(CU_Name.x, CU_Name.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)

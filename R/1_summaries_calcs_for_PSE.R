###### Plots of abundance and productivity at the Conservation Unit level
###### for the manuscript "Spatiotemporal patterns in salmon..."

rm(list = ls(all=TRUE)); #Remove all the objects in the memory
setwd("~/Dropbox (Personal)/Central coast salmon/Manuscripts/Spatiotemporal patterns in salmon abundance and productivity/R_code")
library(ggplot2)
#library(plyr)
require(reshape2)
library(dplyr)
library(broom)
library(geosphere)



#read in CU level data
abund_file <- read.csv("OUTPUT_TRTC_CU_2017.csv", header = T)
productivity_file <- read.csv("OUTPUT_AGE_CU_2017.csv", header = T)


alldates <- c(1960,1970,1980,1990,2000,2010)
yyy <- c(6.5,16.5,26.5,36.5,46.5,56.5)
#
#### 


ncc_co <- filter(abund_file, SpeciesId == 'CO')%>%
  select('Year', 'Total.Run')%>%
  na.omit()

d1<- group_by(ncc_co, Year) %>% 
  summarise(total = sum(Total.Run))


ncc_ck <- filter(abund_file, SpeciesId == 'CN')%>%
  select('Year', 'Total.Run')%>%
  na.omit()

d2<- group_by(ncc_ck, Year) %>% 
  summarise(total = sum(Total.Run))

ncc_cm <- filter(abund_file, SpeciesId == 'CM')%>%
  select('Year', 'Total.Run')%>%
  na.omit()

d3<- group_by(ncc_cm, Year) %>% 
  summarise(total = sum(Total.Run))

ncc_pk <- filter(abund_file, SpeciesId %in% c('PKe', 'PKo'))%>%
  select('Year', 'Total.Run')%>%
  na.omit()

d4<- group_by(ncc_pk, Year) %>% 
  summarise(total = sum(Total.Run))

ncc_sx <- filter(abund_file, SpeciesId == 'SX')%>%
  select('Year', 'Total.Run')%>%
  na.omit()

d5<- group_by(ncc_sx, Year) %>% 
  summarise(total = sum(Total.Run))

ncc_frame <- full_join(d1,d3, by='Year')

ncc_frame <- full_join(ncc_frame,d5, by='Year')

ncc_frame <- full_join(ncc_frame,d4, by='Year')


ncc_frame <- full_join(ncc_frame,d2, by='Year')


colnames (ncc_frame) <- c("Year", "Coho",'Chum','Sockeye','Pink','Chinook')

write.csv(ncc_frame, file="ncc_frame.csv")

ncc_frame <- select(ncc_frame,-Year)%>%
  as.matrix(ncc_frame)%>%
  t()

quartz(width=7, height=3)
par(mar=c(2.5,3.5,1.5,1)+.1,mfrow=c(1,1)) 
yy <- barplot(ncc_frame/1000000, col=c('blue','green','red','pink','black'), space=0, ylim = c(0,55), axes=F, axisnames=F)  
axis(1, at=yyy, labels = alldates,  line=0, cex.axis=1, tck=-0.02, mgp=c(3,0.3,0), col.ticks='grey40')
axis(2, las=2, cex.axis=1, tck=-0.02, mgp=c(3,0.5,0), col.ticks ='grey40')
box(lwd=2, col='grey40')
mtext("Number of salmon (millions)", side=2, line=2)
mtext("Return Year", side=1, line=1.5)
#legend("top",  legend=c("Spawners","Catch"), fill=c('slategray','powderblue'), border=c("black","black"),lty=c(0,0), col=c(NA,NA),lwd=c(0,0),bty="n", cex=0.85, ncol=2, seg.len=0.85) 	
#rect(53,0,66.5,550000, col = rgb(0,0.2,0.5,1/16), lty=0)
#dev.off()
##
#### % diff







abund_file <- abund_file%>%
  filter(CU_Name != 'Asitika')%>%
  filter(CU_Name != 'Backland')%>%
  filter(CU_Name != 'Damdochax')%>%
  filter(CU_Name != 'Damshilgwit')%>%
  filter(CU_Name != 'Johnston')%>%
  filter(CU_Name != 'Kitwancool')%>%
  filter(CU_Name != 'Lower Nass - Portland')%>%
  filter(CU_Name != 'Mathers')%>%
  filter(CU_Name != 'Motase')%>%
  filter(CU_Name != 'Skeena River-high interior')%>%
  filter(CU_Name != 'Upper Nass River')%>%
  filter(CU_Name != 'Canoona')%>% # missing nearly all 'early' year 
  filter(CU_Name != 'Freeda')%>% # missing nearly all 'early' year 
  filter(CU_Name != 'Fred Wright')%>% # missing nearly all 'early' year 
  filter(CU_Name != 'Meziadin') # missing nearly all 'early' year 


spp_list <- split(abund_file, abund_file$SpeciesId)


expanded_df <-
  left_join(spp_list$CM, select(spp_list$CM, CU_Name, Year, Total.Run), by = "Year" ) %>%
  arrange(CU_Name.x, CU_Name.y) %>% 
  as_data_frame()


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



quartz()
hist(chum_r2$cor_val)

expanded_df_early <- filter(expanded_df, Year < 1985)
expanded_df_late <- filter(expanded_df, Year > 1984)


chum_r2_early <-
  expanded_df_early%>%
  group_by(CU_Name.x, CU_Name.y) %>%
  do(get_cor(.$Total.Run.x, .$Total.Run.y, use = 'complete.obs')) %>%
  filter(CU_Name.x != CU_Name.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(CU_Name.x, CU_Name.y)), 
         max_pop = max(c(CU_Name.x, CU_Name.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)


quartz()
hist(chum_r2_early$cor_val)

chum_r2_late <-
  expanded_df_late%>%
  group_by(CU_Name.x, CU_Name.y) %>%
  do(get_cor(.$Total.Run.x, .$Total.Run.y, use = 'complete.obs')) %>%
  filter(CU_Name.x != CU_Name.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(CU_Name.x, CU_Name.y)), 
         max_pop = max(c(CU_Name.x, CU_Name.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)


quartz()
par(mfrow=c(1,2))
hist(chum_r2_early$cor_val, main = "", xlab="Correlation Coefficient")
abline(v=mean(chum_r2_early$cor_val), lwd=5)
hist(chum_r2_late$cor_val, main = "", xlab="Correlation Coefficient")
abline(v=mean(chum_r2_late$cor_val), lwd=5)

quartz()
d <- density(chum_r2_early$cor_val, ylim=2)
e <- density(chum_r2_late$cor_val)
plot(e)
lines(d, col='blue')
sm.density.compare(mpg, cyl, xlab="Miles Per Gallon")
?density

####### sockeye
## sockeye ##

expanded_df_sx <-
  left_join(spp_list$SX, select(spp_list$SX, Population, Year, Total.Run), by = "Year" ) %>%
  arrange(Population.x, Population.y)

#remove populations with less than 30 years of data


sx_r2 <-
  expanded_df_sx%>%
  group_by(Population.x, Population.y) %>%
  do(get_cor(.$Total.Run.x, .$Total.Run.y, use = 'complete.obs')) %>%
  filter(Population.x != Population.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(Population.x, Population.y)), 
         max_pop = max(c(Population.x, Population.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)


quartz()
hist(sx_r2$cor_val)



expanded_df_early <- filter(expanded_df_sx, Year < 1985)
expanded_df_late <- filter(expanded_df_sx, Year > 1984)


sx_r2_early <-
  expanded_df_early%>%
  group_by(Population.x, Population.y) %>%
  do(get_cor(.$Total.Run.x, .$Total.Run.y, use = 'complete.obs')) %>%
  filter(Population.x != Population.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(Population.x, Population.y)), 
         max_pop = max(c(Population.x, Population.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)




sx_r2_late <-
  expanded_df_late%>%
  group_by(Population.x, Population.y) %>%
  do(get_cor(.$Total.Run.x, .$Total.Run.y, use = 'complete.obs')) %>%
  filter(Population.x != Population.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(Population.x, Population.y)), 
         max_pop = max(c(Population.x, Population.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)


quartz()
par(mfrow=c(1,2))
hist(sx_r2_early$cor_val, main = "", xlab="Correlation Coefficient")
abline(v=mean(sx_r2_early$cor_val), lwd=5)
hist(sx_r2_late$cor_val, main = "", xlab="Correlation Coefficient")
abline(v=mean(sx_r2_late$cor_val), lwd=5)

mean(sx_r2_late$cor_val)
mean(sx_r2_early$cor_val)




 ## coho ##


expanded_df_co <-
  left_join(spp_list$CO, select(spp_list$CO, Population, Year, Total.Run), by = "Year" ) %>%
  arrange(Population.x, Population.y)


co_r2 <-
  expanded_df_co%>%
  group_by(Population.x, Population.y) %>%
  do(get_cor(.$Total.Run.x, .$Total.Run.y, use = 'complete.obs')) %>%
  filter(Population.x != Population.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(Population.x, Population.y)), 
         max_pop = max(c(Population.x, Population.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)


quartz()
hist(co_r2$cor_val)

co_df_early <- filter(expanded_df_co, Year < 1985)
co_df_late <- filter(expanded_df_co, Year > 1984)


co_r2_early <-
  co_df_early%>%
  group_by(Population.x, Population.y) %>%
  do(get_cor(.$Total.Run.x, .$Total.Run.y, use = 'complete.obs')) %>%
  filter(Population.x != Population.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(Population.x, Population.y)), 
         max_pop = max(c(Population.x, Population.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)


quartz()
hist(chum_r2_early$cor_val)

co_r2_late <-
  co_df_late%>%
  group_by(Population.x, Population.y) %>%
  do(get_cor(.$Total.Run.x, .$Total.Run.y, use = 'complete.obs')) %>%
  filter(Population.x != Population.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(Population.x, Population.y)), 
         max_pop = max(c(Population.x, Population.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)


quartz()
par(mfrow=c(1,2))
hist(co_r2_early$cor_val, main = "", xlab="Correlation Coefficient")
abline(v=mean(co_r2_early$cor_val), lwd=5)
hist(co_r2_late$cor_val, main = "", xlab="Correlation Coefficient")
abline(v=mean(co_r2_late$cor_val), lwd=5)



#PKe
expanded_df_pke <-
  left_join(spp_list$PKe, select(spp_list$PKe, Population, Year, Total.Run), by = "Year" ) %>%
  arrange(Population.x, Population.y)


pke_r2 <-
  expanded_df_pke%>%
  group_by(Population.x, Population.y) %>%
  do(get_cor(.$Total.Run.x, .$Total.Run.y, use = 'complete.obs')) %>%
  filter(Population.x != Population.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(Population.x, Population.y)), 
         max_pop = max(c(Population.x, Population.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)


quartz()
hist(pke_r2$cor_val)

pke_df_early <- filter(expanded_df_pke, Year < 1985)
pke_df_late <- filter(expanded_df_pke, Year > 1984)


pke_r2_early <-
  pke_df_early%>%
  group_by(Population.x, Population.y) %>%
  do(get_cor(.$Total.Run.x, .$Total.Run.y, use = 'complete.obs')) %>%
  filter(Population.x != Population.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(Population.x, Population.y)), 
         max_pop = max(c(Population.x, Population.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)



pke_r2_late <-
  pke_df_late%>%
  group_by(Population.x, Population.y) %>%
  do(get_cor(.$Total.Run.x, .$Total.Run.y, use = 'complete.obs')) %>%
  filter(Population.x != Population.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(Population.x, Population.y)), 
         max_pop = max(c(Population.x, Population.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)


quartz()
par(mfrow=c(1,2))
hist(pke_r2_early$cor_val, main = "", xlab="Correlation Coefficient")
abline(v=mean(pke_r2_early$cor_val), lwd=5)
hist(pke_r2_late$cor_val, main = "", xlab="Correlation Coefficient")
abline(v=mean(pke_r2_late$cor_val), lwd=5)


#PKo

expanded_df_pko <-
  left_join(spp_list$PKo, select(spp_list$PKo, Population, Year, Total.Run), by = "Year" ) %>%
  arrange(Population.x, Population.y)



pko_r2 <-
  expanded_df_pko%>%
  group_by(Population.x, Population.y) %>%
  do(get_cor(.$Total.Run.x, .$Total.Run.y, use = 'complete.obs')) %>%
  filter(Population.x != Population.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(Population.x, Population.y)), 
         max_pop = max(c(Population.x, Population.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)


quartz()
hist(pke_r2$cor_val)

pko_df_early <- filter(expanded_df_pko, Year < 1985)
pko_df_late <- filter(expanded_df_pko, Year > 1984)


pko_r2_early <-
  pko_df_early%>%
  group_by(Population.x, Population.y) %>%
  do(get_cor(.$Total.Run.x, .$Total.Run.y, use = 'complete.obs')) %>%
  filter(Population.x != Population.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(Population.x, Population.y)), 
         max_pop = max(c(Population.x, Population.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)



pko_r2_late <-
  pko_df_late%>%
  group_by(Population.x, Population.y) %>%
  do(get_cor(.$Total.Run.x, .$Total.Run.y, use = 'complete.obs')) %>%
  filter(Population.x != Population.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(Population.x, Population.y)), 
         max_pop = max(c(Population.x, Population.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)


quartz()
par(mfrow=c(1,2))
hist(pko_r2_early$cor_val, main = "", xlab="Correlation Coefficient")
abline(v=mean(pko_r2_early$cor_val), lwd=5)
hist(pko_r2_late$cor_val, main = "", xlab="Correlation Coefficient")
abline(v=mean(pko_r2_late$cor_val), lwd=5)


### plot 5 x 2 fig of panels for histograms
quartz(width=8, height=10)
par(mfrow=c(5,2))
?hist
#chum
hist(chum_r2_early$cor_val, xlim=c(-1,1), main = "Chum 1954-1984", xlab="Correlation Coefficient")
abline(v=mean(chum_r2_early$cor_val), lwd=5)
hist(chum_r2_late$cor_val, xlim=c(-1,1), main = "Chum 1985-2014", xlab="Correlation Coefficient")
abline(v=mean(chum_r2_late$cor_val), lwd=5)

#coho
hist(co_r2_early$cor_val, xlim=c(-1,1), main = "Coho 1954-1984", xlab="Correlation Coefficient")
abline(v=mean(co_r2_early$cor_val), lwd=5)
hist(co_r2_late$cor_val, xlim=c(-1,1), main = "Coho 1985-2014", xlab="Correlation Coefficient")
abline(v=mean(co_r2_late$cor_val), lwd=5)

#sx
hist(sx_r2_early$cor_val, xlim=c(-1,1), main = "Sockeye 1954-1984", xlab="Correlation Coefficient")
abline(v=mean(sx_r2_early$cor_val), lwd=5)
hist(sx_r2_late$cor_val, xlim=c(-1,1), main = "Sockeye 1985-2014", xlab="Correlation Coefficient")
abline(v=mean(sx_r2_late$cor_val), lwd=5)

#pink even
hist(pke_r2_early$cor_val, xlim=c(-1,1), main = "Pink even 1954-1984", xlab="Correlation Coefficient")
abline(v=mean(pke_r2_early$cor_val), lwd=5)
hist(pke_r2_late$cor_val, xlim=c(-1,1), main = "Pink even 1985-2014", xlab="Correlation Coefficient")
abline(v=mean(pke_r2_late$cor_val), lwd=5)

#pink odd
hist(pko_r2_early$cor_val, xlim=c(-1,1), main = "Pink odd 1954-1984", xlab="Correlation Coefficient")
abline(v=mean(pko_r2_early$cor_val), lwd=5)
hist(pko_r2_late$cor_val, xlim=c(-1,1), main = "Pink odd 1985-2014", xlab="Correlation Coefficient")
abline(v=mean(pko_r2_late$cor_val), lwd=5)

##### try overlapping
### plot 5 x 2 fig of panels for histograms
quartz(width=8, height=8)
par(mfrow=c(2,3))
?hist
#chum
hist(chum_r2_early$cor_val, xlim=c(-1,1), main = "Chum", xlab="Correlation Coefficient", ylim=c(0,25), col=rgb(1,0,0,alpha=0.3), breaks=12)
#abline(v=mean(chum_r2_early$cor_val), lwd=5)
hist(chum_r2_late$cor_val, xlim=c(-1,1), main = "Chum 1985-2014", xlab="Correlation Coefficient", add=T, col=rgb(0,0,1,alpha=0.3), breaks=12)
#abline(v=mean(chum_r2_late$cor_val), lwd=5)
?hist
#coho
hist(co_r2_early$cor_val, xlim=c(-1,1), main = "Coho", xlab="Correlation Coefficient", ylim=c(0,30), col=rgb(1,0,0,alpha=0.3), breaks=12)
#abline(v=mean(co_r2_early$cor_val), lwd=5)
hist(co_r2_late$cor_val, xlim=c(-1,1), main = "Coho", xlab="Correlation Coefficient", add=T, col=rgb(0,0,1,alpha=0.3), breaks=12)
#abline(v=mean(co_r2_late$cor_val), lwd=5)

#sx
hist(sx_r2_early$cor_val, xlim=c(-1,1), main = "Sockeye", xlab="Correlation Coefficient", col=rgb(1,0,0,alpha=0.3), breaks=12)
#abline(v=mean(sx_r2_early$cor_val), lwd=5)
hist(sx_r2_late$cor_val, xlim=c(-1,1), main = "Sockeye 1985-2014", xlab="Correlation Coefficient", add=T, col=rgb(0,0,1,alpha=0.3), breaks=12)
#abline(v=mean(sx_r2_late$cor_val), lwd=5)

#pink even
hist(pke_r2_early$cor_val, xlim=c(-1,1), main = "Pink even", xlab="Correlation Coefficient", col=rgb(1,0,0,alpha=0.3), breaks=8)
#abline(v=mean(pke_r2_early$cor_val), lwd=5)
hist(pke_r2_late$cor_val, xlim=c(-1,1), main = "Pink even 1985-2014", xlab="Correlation Coefficient", add=T, col=rgb(0,0,1,alpha=0.3), breaks=8)
#abline(v=mean(pke_r2_late$cor_val), lwd=5)

#pink odd
hist(pko_r2_early$cor_val, xlim=c(-1,1), main = "Pink odd", xlab="Correlation Coefficient", col=rgb(1,0,0,alpha=0.3), breaks=8)
#abline(v=mean(pko_r2_early$cor_val), lwd=5)
hist(pko_r2_late$cor_val, xlim=c(-1,1), main = "Pink odd 1985-2014", xlab="Correlation Coefficient", add=T, col=rgb(0,0,1,alpha=0.3), breaks=8)
#abline(v=mean(pko_r2_late$cor_val), lwd=5)

#### modify prod. data
productivity_file <- read.csv("ncc_age_CU_20170303.csv", header = T)

prod_file <- productivity_file%>%
  filter(CU_Name != 'Asitika')%>%
  filter(CU_Name != 'Backland')%>%
  filter(CU_Name != 'Damdochax')%>%
  filter(CU_Name != 'Damshilgwit')%>%
  filter(CU_Name != 'Johnston')%>%
  filter(CU_Name != 'Kitwancool')%>%
  filter(CU_Name != 'Lower Nass - Portland')%>%
  filter(CU_Name != 'Mathers')%>%
  filter(CU_Name != 'Motase')%>%
  filter(CU_Name != 'Skeena River-high interior')%>%
  filter(CU_Name != 'Upper Nass River')%>%
  filter(CU_Name != 'Canoona')%>% # missing nearly all 'early' year 
  filter(CU_Name != 'Freeda')%>% # missing nearly all 'early' year 
  filter(CU_Name != 'Fred Wright')%>% # missing nearly all 'early' year 
  filter(CU_Name != 'Meziadin')%>%
  filter(CU_Name != 'Asitika')%>%
  filter(CU_Name != 'Skeena River-high interior')%>%
  filter(CU_Name != 'Damshilgwit')%>%
  filter(CU_Name != 'Motase')%>%
  filter(CU_Name != 'Backland')%>%
  filter(CU_Name != 'Johnston')%>%
  filter(CU_Name != 'Awun')



#figure out which CUs don't have good survival data
p_file <- prod_file %>%
  filter(!is.na(survival))

p_file <- group_by(p_file, StatArea.CU)

delay2 <- summarize(p_file,  survival = n(), na.omit=T)

#### remove CUs with less than 25 estimates of survival
prod_file <- prod_file%>%
  filter(StatArea.CU != 'CM_25')%>%
  filter(StatArea.CU != 'CM_26')%>%
  filter(StatArea.CU != 'CM_31')%>%
  filter(StatArea.CU != 'CO_34')%>%
  filter(StatArea.CU != 'SX_L-19-45')%>%
  filter(StatArea.CU != 'SX_L-19-46')%>%
  filter(StatArea.CU != 'SX_L-19-50')%>%
  filter(StatArea.CU != 'SX_L-19-62')%>%
  filter(StatArea.CU != 'SX_L-22-03')%>%
  filter(StatArea.CU != 'SX_L-20-06')%>%
  filter(StatArea.CU != 'SX_L-19-24')

# late sockeye data limitations 
## also remove pops with less than 10 estimates in early or late period
prod_file <- prod_file%>%
  filter(StatArea.CU != 'SX_L-17-05')%>%
  filter(StatArea.CU != 'SX_L-17-07')%>%
  filter(StatArea.CU != 'SX_L-19-26')%>%
  filter(StatArea.CU != 'SX_L-19-33')%>%
  filter(StatArea.CU != 'SX_L-19-34')%>%
  filter(StatArea.CU != 'SX_L-19-39')%>%
  filter(StatArea.CU != 'SX_L-19-60')%>%
  filter(StatArea.CU != 'SX_L-19-70')




#figure out which CUs don't have good survival data
sx_file <- prod_file %>%
  filter(Year > 1985)%>%
  filter(!is.na(survival))

sx_file <- group_by(sx_file, StatArea.CU)

delay3 <- summarize(sx_file,  survival = n(), na.omit=T)


sp_list <- split(prod_file, prod_file$SpeciesId)


expanded_df_prod <-
  left_join(sp_list$CM, select(sp_list$CM, Population, Year, survival,Lat,Long), by = "Year" ) %>%
  arrange(Population.x, Population.y) %>% 
  as_data_frame()

distm(c(-126.9764, 51.39301), c(-126.7238, 51.73276), fun = distHaversine)

#get_dist <- function(xlon, xlat, ylon, ylat, fun = distHaversine) {
 # dist_val <- distm(cbind(xlon,xlat), cbind(ylon,ylat), fun)
#  return(data_frame(dist_val = dist_val))
#}

expanded_df_prod_2<-
expanded_df_prod%>%
  group_by(Population.x, Population.y) %>%
  mutate(CTD= distHaversine(cbind(Long.x, Lat.x), cbind(Long.y, Lat.y)))%>%
  filter(Population.x != Population.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(Population.x, Population.y)), 
         max_pop = max(c(Population.x, Population.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)
  
?left_join
#chum_dist_prod <-
 # expanded_df_prod%>%
  #group_by(Population.x, Population.y) %>%
  #do(get_dist(.$Long.x, .$Lat.x, .$Lon.y, .$Lat.x)) %>%
  #filter(Population.x != Population.y)%>%
  #ungroup()%>%
  #rowwise()%>%
  #mutate(min_pop = min(c(Population.x, Population.y)), 
   #      max_pop = max(c(Population.x, Population.y)))%>%
  #distinct(min_pop, max_pop, .keep_all = TRUE)


get_cor <- function(x, y, use = 'complete.obs') {
  cor_val <- cor(x, y, use)
  return(data_frame(cor_val = cor_val))
}

chum_r2_prod <-
  expanded_df_prod%>%
  group_by(Population.x, Population.y) %>%
  do(get_cor(.$survival.x, .$survival.y, use = 'complete.obs')) %>%
  filter(Population.x != Population.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(Population.x, Population.y)), 
         max_pop = max(c(Population.x, Population.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)

chum_data <- cbind(chum_r2_prod,expanded_df_prod_2)

quartz()
qplot(CTD, cor_val, data=chum_data)


?group_by
quartz()
hist(chum_r2_prod$cor_val)

## early and late

expanded_df_prod_early <- filter(expanded_df_prod, Year < 1985)
expanded_df_prod_late <- filter(expanded_df_prod, Year > 1984)

## early
chum_r2_prod_early <-
  expanded_df_prod_early%>%
  group_by(Population.x, Population.y) %>%
  do(get_cor(.$survival.x, .$survival.y, use = 'complete.obs')) %>%
  filter(Population.x != Population.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(Population.x, Population.y)), 
         max_pop = max(c(Population.x, Population.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)


chum_r2_prod_late <-
  expanded_df_prod_late%>%
  group_by(Population.x, Population.y) %>%
  do(get_cor(.$survival.x, .$survival.y, use = 'complete.obs')) %>%
  filter(Population.x != Population.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(Population.x, Population.y)), 
         max_pop = max(c(Population.x, Population.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)


#
quartz()
hist(chum_r2_prod_early$cor_val, xlim=c(-1,1), main = "Chum", xlab="Correlation Coefficient", ylim=c(0,25), col=rgb(1,0,0,alpha=0.3), breaks=12)
#abline(v=mean(chum_r2_early$cor_val), lwd=5)
hist(chum_r2_prod_late$cor_val, xlim=c(-1,1), main = "Chum 1985-2014", xlab="Correlation Coefficient", add=T, col=rgb(0,0,1,alpha=0.3), breaks=12)

###coho
coho_df_prod <-
  left_join(sp_list$CO, select(sp_list$CO, Population, Year, survival), by = "Year" ) %>%
  arrange(Population.x, Population.y) %>% 
  as_data_frame()


get_cor <- function(x, y, use = 'complete.obs') {
  cor_val <- cor(x, y, use)
  return(data_frame(cor_val = cor_val))
}

coho_r2_prod <-
  coho_df_prod%>%
  group_by(Population.x, Population.y) %>%
  do(get_cor(.$survival.x, .$survival.y, use = 'complete.obs')) %>%
  filter(Population.x != Population.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(Population.x, Population.y)), 
         max_pop = max(c(Population.x, Population.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)


## early and late

coho_df_prod_early <- filter(coho_df_prod, Year < 1985)
coho_df_prod_late <- filter(coho_df_prod, Year > 1984)

## early
coho_r2_prod_early <-
  coho_df_prod_early%>%
  group_by(Population.x, Population.y) %>%
  do(get_cor(.$survival.x, .$survival.y, use = 'complete.obs')) %>%
  filter(Population.x != Population.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(Population.x, Population.y)), 
         max_pop = max(c(Population.x, Population.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)


coho_r2_prod_late <-
  coho_df_prod_late%>%
  group_by(Population.x, Population.y) %>%
  do(get_cor(.$survival.x, .$survival.y, use = 'complete.obs')) %>%
  filter(Population.x != Population.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(Population.x, Population.y)), 
         max_pop = max(c(Population.x, Population.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)

## 
quartz()
hist(coho_r2_prod_early$cor_val, xlim=c(-1,1), main = "Coho", xlab="Correlation Coefficient", ylim=c(0,25), col=rgb(1,0,0,alpha=0.3), breaks=12)
#abline(v=mean(chum_r2_early$cor_val), lwd=5)
hist(coho_r2_prod_late$cor_val, xlim=c(-1,1), main = "Chum 1985-2014", xlab="Correlation Coefficient", add=T, col=rgb(0,0,1,alpha=0.3), breaks=12)


### sockeye
##
sx_df_prod <-
  left_join(sp_list$SX, select(sp_list$SX, Population, Year, survival), by = "Year" ) %>%
  arrange(Population.x, Population.y) %>% 
  as_data_frame()


get_cor <- function(x, y, use = 'complete.obs') {
  cor_val <- cor(x, y, use)
  return(data_frame(cor_val = cor_val))
}

sx_r2_prod <-
  sx_df_prod%>%
  group_by(Population.x, Population.y) %>%
  do(get_cor(.$survival.x, .$survival.y, use = 'complete.obs')) %>%
  filter(Population.x != Population.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(Population.x, Population.y)), 
         max_pop = max(c(Population.x, Population.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)


## early and late

sx_df_prod_early <- filter(sx_df_prod, Year < 1985)
sx_df_prod_late <- filter(sx_df_prod, Year > 1984)

## early
sx_r2_prod_early <-
  sx_df_prod_early%>%
  group_by(Population.x, Population.y) %>%
  do(get_cor(.$survival.x, .$survival.y, use = 'complete.obs')) %>%
  filter(Population.x != Population.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(Population.x, Population.y)), 
         max_pop = max(c(Population.x, Population.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)


sx_r2_prod_late <-
  sx_df_prod_late%>%
  group_by(Population.x, Population.y) %>%
  do(get_cor(.$survival.x, .$survival.y, use = 'complete.obs')) %>%
  filter(Population.x != Population.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(Population.x, Population.y)), 
         max_pop = max(c(Population.x, Population.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)

## 
quartz()
hist(sx_r2_prod_early$cor_val, xlim=c(-1,1), main = "Sockeye", xlab="Correlation Coefficient", ylim=c(0,150), col=rgb(1,0,0,alpha=0.3), breaks=10)
#abline(v=mean(chum_r2_early$cor_val), lwd=5)
hist(sx_r2_prod_late$cor_val, xlim=c(-1,1), main = "Chum 1985-2014", xlab="Correlation Coefficient", add=T, col=rgb(0,0,1,alpha=0.3), breaks=10)


### sockeye
##
pke_df_prod <-
  left_join(sp_list$PKe, select(sp_list$PKe, Population, Year, survival), by = "Year" ) %>%
  arrange(Population.x, Population.y) %>% 
  as_data_frame()


get_cor <- function(x, y, use = 'complete.obs') {
  cor_val <- cor(x, y, use)
  return(data_frame(cor_val = cor_val))
}

pke_r2_prod <-
  pke_df_prod%>%
  group_by(Population.x, Population.y) %>%
  do(get_cor(.$survival.x, .$survival.y, use = 'complete.obs')) %>%
  filter(Population.x != Population.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(Population.x, Population.y)), 
         max_pop = max(c(Population.x, Population.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)


## early and late

pke_df_prod_early <- filter(pke_df_prod, Year < 1985)
pke_df_prod_late <- filter(pke_df_prod, Year > 1984)

## early
pke_r2_prod_early <-
  pke_df_prod_early%>%
  group_by(Population.x, Population.y) %>%
  do(get_cor(.$survival.x, .$survival.y, use = 'complete.obs')) %>%
  filter(Population.x != Population.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(Population.x, Population.y)), 
         max_pop = max(c(Population.x, Population.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)


pke_r2_prod_late <-
  pke_df_prod_late%>%
  group_by(Population.x, Population.y) %>%
  do(get_cor(.$survival.x, .$survival.y, use = 'complete.obs')) %>%
  filter(Population.x != Population.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(Population.x, Population.y)), 
         max_pop = max(c(Population.x, Population.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)

## 
quartz()
hist(pke_r2_prod_early$cor_val, xlim=c(-1,1), main = "Sockeye", xlab="Correlation Coefficient", ylim=c(0,10), col=rgb(1,0,0,alpha=0.3), breaks=10)
#abline(v=mean(chum_r2_early$cor_val), lwd=5)
hist(pke_r2_prod_late$cor_val, xlim=c(-1,1), main = "Chum 1985-2014", xlab="Correlation Coefficient", add=T, col=rgb(0,0,1,alpha=0.3), breaks=10)



### sockeye
##
pko_df_prod <-
  left_join(sp_list$PKo, select(sp_list$PKo, Population, Year, survival), by = "Year" ) %>%
  arrange(Population.x, Population.y) %>% 
  as_data_frame()


get_cor <- function(x, y, use = 'complete.obs') {
  cor_val <- cor(x, y, use)
  return(data_frame(cor_val = cor_val))
}

pko_r2_prod <-
  pke_df_prod%>%
  group_by(Population.x, Population.y) %>%
  do(get_cor(.$survival.x, .$survival.y, use = 'complete.obs')) %>%
  filter(Population.x != Population.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(Population.x, Population.y)), 
         max_pop = max(c(Population.x, Population.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)


## early and late

pko_df_prod_early <- filter(pko_df_prod, Year < 1985)
pko_df_prod_late <- filter(pko_df_prod, Year > 1984)

## early
pko_r2_prod_early <-
  pko_df_prod_early%>%
  group_by(Population.x, Population.y) %>%
  do(get_cor(.$survival.x, .$survival.y, use = 'complete.obs')) %>%
  filter(Population.x != Population.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(Population.x, Population.y)), 
         max_pop = max(c(Population.x, Population.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)


pko_r2_prod_late <-
  pko_df_prod_late%>%
  group_by(Population.x, Population.y) %>%
  do(get_cor(.$survival.x, .$survival.y, use = 'complete.obs')) %>%
  filter(Population.x != Population.y)%>%
  ungroup()%>%
  rowwise()%>%
  mutate(min_pop = min(c(Population.x, Population.y)), 
         max_pop = max(c(Population.x, Population.y)))%>%
  distinct(min_pop, max_pop, .keep_all = TRUE)

## 
quartz()
hist(pko_r2_prod_early$cor_val, xlim=c(-1,1), main = "Sockeye", xlab="Correlation Coefficient", ylim=c(0,10), col=rgb(1,0,0,alpha=0.3), breaks=10)
#abline(v=mean(chum_r2_early$cor_val), lwd=5)
hist(pko_r2_prod_late$cor_val, xlim=c(-1,1), main = "Chum 1985-2014", xlab="Correlation Coefficient", add=T, col=rgb(0,0,1,alpha=0.3), breaks=10)




## plot all on one plot

quartz(width=8, height=8)
par(mfrow=c(2,3))

hist(chum_r2_prod_early$cor_val, xlim=c(-1,1), main = "Chum", xlab="Correlation Coefficient", ylim=c(0,50), col=rgb(1,0,0,alpha=0.3), breaks=6)
#abline(v=mean(chum_r2_early$cor_val), lwd=5)
hist(chum_r2_prod_late$cor_val, xlim=c(-1,1), main = "", xlab="Correlation Coefficient", add=T, col=rgb(0,0,1,alpha=0.3), breaks=8)


hist(coho_r2_prod_early$cor_val, xlim=c(-1,1), main = "Coho", xlab="Correlation Coefficient", ylim=c(0,50), col=rgb(1,0,0,alpha=0.3), breaks=10)
#abline(v=mean(chum_r2_early$cor_val), lwd=5)
hist(coho_r2_prod_late$cor_val, xlim=c(-1,1), main = "", xlab="Correlation Coefficient", add=T, col=rgb(0,0,1,alpha=0.3), breaks=10)


hist(sx_r2_prod_early$cor_val, xlim=c(-1,1), main = "Sockeye", xlab="Correlation Coefficient", ylim=c(0,150), col=rgb(1,0,0,alpha=0.3), breaks=10)
#abline(v=mean(chum_r2_early$cor_val), lwd=5)
hist(sx_r2_prod_late$cor_val, xlim=c(-1,1), main = "", xlab="Correlation Coefficient", add=T, col=rgb(0,0,1,alpha=0.3), breaks=10)


hist(pke_r2_prod_early$cor_val, xlim=c(-1,1), main = "Pink Even", xlab="Correlation Coefficient", ylim=c(0,10), col=rgb(1,0,0,alpha=0.3), breaks=6)
#abline(v=mean(chum_r2_early$cor_val), lwd=5)
hist(pke_r2_prod_late$cor_val, xlim=c(-1,1), main = "", xlab="Correlation Coefficient", add=T, col=rgb(0,0,1,alpha=0.3), breaks=6)


hist(pko_r2_prod_early$cor_val, xlim=c(-1,1), main = "Pink Odd", xlab="Correlation Coefficient", ylim=c(0,12), col=rgb(1,0,0,alpha=0.3), breaks=8)
#abline(v=mean(chum_r2_early$cor_val), lwd=5)
hist(pko_r2_prod_late$cor_val, xlim=c(-1,1), main = "", xlab="Correlation Coefficient", add=T, col=rgb(0,0,1,alpha=0.3), breaks=8)

mean(chum_r2_prod_early$cor_val)
mean(chum_r2_prod_late$cor_val)

mean(coho_r2_prod_early$cor_val)
mean(coho_r2_prod_late$cor_val)

mean(sx_r2_prod_early$cor_val)
mean(sx_r2_prod_late$cor_val)

mean(pko_r2_prod_early$cor_val)
mean(pko_r2_prod_late$cor_val)

mean(pke_r2_prod_early$cor_val)
mean(pke_r2_prod_late$cor_val)

# write csvs of data so that I can collate in excel
write.csv(chum_r2_early, file="chum_r2_early.csv")
write.csv(chum_r2_late, file="chum_r2_late.csv")

write.csv(ck_r2, file="ck_r2.csv")

write.csv(coho_r2_early, file="coho_r2_early.csv")
write.csv(coho_r2_late, file="coho_r2_late.csv")

write.csv(pke_r2_early, file="pke_r2_early.csv")
write.csv(pke_r2_late, file="pke_r2_late.csv")

write.csv(pko_r2_early, file="pko_r2_early.csv")
write.csv(pko_r2_late, file="pko_r2_late.csv")

write.csv(sockeye_r2_early, file="sockeye_r2_early.csv")
write.csv(sockeye_r2_late, file="sockeye_r2_late.csv")

### read in collated data

corr_file <- read.csv("collated_corr_data.csv", header = T)

## violin plot
corr_file <-
corr_file%>%
  filter(Species != 'Chinook')
  
p <-  ggplot(corr_file, aes(x=Species, y=cor_val, fill=Temporal)) +
  geom_violin(trim = FALSE)+
  labs(title="Correlation among CUs by species and time period",
       x="Species", y = "Pearson Correlation Coefficient")+
  theme_bw()+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())+
  stat_summary(fun.y=median, geom="point", shape=16, size=2,
                position=position_dodge(1))+
  scale_fill_discrete(name="cor_val",
                      breaks=c("Early", "Late"),
                      labels=c("1954-1984", "1985-2014"))+
   theme(legend.title = element_blank())

quartz()
p


# calculate S-R residuals by CU
productivity_file <- productivity_file[complete.cases(productivity_file),]
# calculate S-R residuals
Data <- ddply(productivity_file,c("CU_Name"),function(x){
  res <- resid(lm(x$survival~x$Escape))	
  data.frame(res)
})
res<-Data$res
#combine SR residuals into dataframe
productivity_file <- cbind(productivity_file,res)


#### scales abundance by each CU
cu_list <- split(abund_file, list(abund_file$SpeciesId, abund_file$CU_Name), drop = TRUE)

scaled_list <-
  lapply(cu_list, function(cu_df){
    scaled_abund <- mutate(.data = cu_df, scaled_abund = scale(Total.Run))
    }
    )
#scaled_list

scaled_df <- bind_rows(scaled_list)
head(scaled_df)

#all species together
quartz()
scaled_df %>%
  ggplot(data = ., aes(x = Year, y = scaled_abund))+
  geom_line(aes(colour = CU))+
  geom_smooth(aes(colour = CU))+
  theme_bw()+
  legend(F)+
  facet_wrap(~SpeciesId)
?legend


scaled_df %>%
  ggplot(data = ., aes(x = Year, y = scaled_abund))+
  geom_smooth(aes(colour = Region))+
  theme_bw()+
  facet_wrap(~SpeciesId)



## sockeye plots
## scaled abundance by CU on one plot
quartz(width=6, height=3)
par(mar=c(0,1.5,1.5,1)+.1,mfrow=c(1,1))
p1 <- scaled_df %>%
  filter(SpeciesId == "SX") %>%
  ggplot(data = ., aes(x = Year, y = scaled_abund))+
  ylab("Abundance")+
  ggtitle("Sockeye")+
  geom_line(aes(colour = CU_Name), size=0.5)+
  scale_colour_grey()+
  geom_smooth()+
  theme_bw()+
  theme(legend.position = "none")+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())


## scaled abundance by CU on one plot
quartz(width=6, height=3)
par(mar=c(0,1.5,1.5,1)+.1,mfrow=c(1,1))
p2<- scaled_df %>%
  filter(SpeciesId == "CO") %>%
  ggplot(data = ., aes(x = Year, y = scaled_abund))+
  ylab("Abundance")+
  ggtitle("Coho")+
  geom_line(aes(colour = CU_Name))+
  scale_colour_grey()+
  geom_smooth()+
  theme_bw()+
  theme(legend.position = "none")+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())

## scaled abundance by CU on one plot
quartz(width=6, height=3)
par(mar=c(0,1.5,1.5,1)+.1,mfrow=c(1,1))
p3<- scaled_df %>%
  filter(SpeciesId == "CN") %>%
  ggplot(data = ., aes(x = Year, y = scaled_abund))+
  expand_limits(x = 1954)+
  ylab("Abundance")+
  ggtitle("Chinook")+
  geom_line(aes(colour = CU_Name))+
  scale_colour_grey()+
  geom_smooth()+
  theme_bw()+
  theme(legend.position = "none")+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())

cc <- scaled_df %>%
  filter(SpeciesId == "CN")
## scaled abundance by CU on one plot
quartz(width=6, height=3)
par(mar=c(0,1.5,1.5,1)+.1,mfrow=c(1,1))
p4 <- scaled_df %>%
  filter(SpeciesId == "CM") %>%
  ggplot(data = ., aes(x = Year, y = scaled_abund))+
  ylab("Abundance")+
  ggtitle("Chum")+
  geom_line(aes(colour = CU_Name))+
  scale_colour_grey()+
  geom_smooth()+
  theme_bw()+
  theme(legend.position = "none")+  
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())



## scaled abundance by CU on one plot
quartz(width=6, height=3)
par(mar=c(0,1.5,1.5,1)+.1,mfrow=c(1,1))
p5<- scaled_df %>%
  filter(SpeciesId  %in% c( "PKe","PKo")) %>%
  ggplot(data = ., aes(x = Year, y = scaled_abund))+
  ylab("Abundance")+
  ggtitle("Pink")+
  geom_line(aes(colour = CU_Name))+
  scale_colour_grey()+
  geom_smooth()+
  theme_bw()+
  theme(legend.position = "none")+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())

quartz(width=7,height=7)
grid.arrange(p1,p2,p3,p4,p5)

## scaled abundance by CU on one plot
quartz()
scaled_df %>%
  filter(SpeciesId  %in% c( "PKe","PKo")) %>%
  ggplot(data = ., aes(x = Year, y = scaled_abund))+
  geom_line(aes(colour = CU_Name))+
  scale_colour_grey()+
  geom_smooth()+
  theme_bw()+
  theme(legend.position = "none")+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())

?ggplot
## each CU on own plot
scaled_df %>%
  filter(SpeciesId == "SX") %>%
  ggplot(data = ., aes(x = Year, y = scaled_abund))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()+
  facet_wrap(~CU_Name)

theme_set(theme_bw(base_size = 25))
## by region
scaled_df %>%
  filter(SpeciesId == "SX") %>%
  ggplot(data = ., aes(x = Year, y = scaled_abund))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw(base_size = 18)+
  facet_wrap(~Region)+
  ylab("Scaled Abundance")+
  geom_hline(yintercept=0, size=1, linetype=2)+
  theme(legend.position="none")

target <- c("Long", "Owikeno","Northern Coastal Fjords")

scaled_df %>%
  filter(SpeciesId == "SX") %>%
  filter(CU_Name %in% target)%>%
  ggplot(data = ., aes(x = Year, y = scaled_abund))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw(base_size = 18)+
  facet_wrap(~Region)+
  ylab("Scaled Abundance")+
  geom_hline(yintercept=0, size=1, linetype=2)+
  scale_colour_discrete(breaks=c("Long", "Owikeno","Northern Coastal Fjords"),
                      labels=c("Long", "Owikeno", "Atnarko"))

  theme(legend.position="none")

## chum plots
## scaled abundance by CU on one plot
windows()
scaled_df %>%
  filter(SpeciesId == "CM") %>%
  ggplot(data = ., aes(x = Year, y = scaled_abund))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()

## each CU on own plot
scaled_df %>%
  filter(SpeciesId == "CM") %>%
  ggplot(data = ., aes(x = Year, y = scaled_abund))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()+
  facet_wrap(~CU_Name)

## by region
scaled_df %>%
  filter(SpeciesId == "CM") %>%
  ggplot(data = ., aes(x = Year, y = scaled_abund))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw(base_size = 18)+
  facet_wrap(~Region)+
  ylab("Scaled Abundance")+
  geom_hline(yintercept=0, size=1, linetype=2)+
  theme(legend.position="none")

## coho plots
## scaled abundance by CU on one plot
windows()
scaled_df %>%
  filter(SpeciesId == "CO") %>%
  ggplot(data = ., aes(x = Year, y = scaled_abund))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()

## each CU on own plot
scaled_df %>%
  filter(SpeciesId == "CO") %>%
  ggplot(data = ., aes(x = Year, y = scaled_abund))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()+
  facet_wrap(~CU_Name)

## by region
scaled_df %>%
  filter(SpeciesId == "CO") %>%
  ggplot(data = ., aes(x = Year, y = scaled_abund))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw(base_size = 18)+
  facet_wrap(~Region)+
  ylab("Scaled Abundance")+
  geom_hline(yintercept=0, size=1, linetype=2)+
  theme(legend.position="none")

## pink even plots
## scaled abundance by CU on one plot
windows()
scaled_df %>%
  filter(SpeciesId == "PKe") %>%
  ggplot(data = ., aes(x = Year, y = scaled_abund))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()

## each CU on own plot
scaled_df %>%
  filter(SpeciesId == "PKe") %>%
  ggplot(data = ., aes(x = Year, y = scaled_abund))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()+
  facet_wrap(~CU_Name)

## each Region on own plot
scaled_df %>%
  filter(SpeciesId == "PKe") %>%
  ggplot(data = ., aes(x = Year, y = scaled_abund))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw(base_size = 18)+
  facet_wrap(~Region)+
  ylab("Scaled Abundance")+
  geom_hline(yintercept=0, size=1, linetype=2)+
  theme(legend.position="none")

## pink odd plots
## scaled abundance by CU on one plot
windows()
scaled_df %>%
  filter(SpeciesId == "PKo") %>%
  ggplot(data = ., aes(x = Year, y = scaled_abund))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()

## each CU on own plot
scaled_df %>%
  filter(SpeciesId == "PKo") %>%
  ggplot(data = ., aes(x = Year, y = scaled_abund))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()+
  facet_wrap(~CU_Name)

## each CU on own plot
scaled_df %>%
  filter(SpeciesId == "PKo") %>%
  ggplot(data = ., aes(x = Year, y = scaled_abund))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw(base_size = 18)+
  facet_wrap(~Region)+
  ylab("Scaled Abundance")+
  geom_hline(yintercept=0, size=1, linetype=2)+
  theme(legend.position="none")


## chinook plots
## scaled abundance by CU on one plot
windows()
scaled_df %>%
  filter(SpeciesId == "CN") %>%
  ggplot(data = ., aes(x = Year, y = scaled_abund))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()

## each CU on own plot
scaled_df %>%
  filter(SpeciesId == "CN") %>%
  ggplot(data = ., aes(x = Year, y = scaled_abund))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()+
  facet_wrap(~CU_Name)


## each region on own plot
scaled_df %>%
  filter(SpeciesId == "CN") %>%
  ggplot(data = ., aes(x = Year, y = scaled_abund))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw(base_size = 18)+
  facet_wrap(~Region)+
  ylab("Scaled Abundance")+
  geom_hline(yintercept=0, size=1, linetype=2)+
  theme(legend.position="none")




### plot residulas from S-R relationship as function of year
## sockeye first
## all on one ##
windows()
productivity_file %>%
  filter(SpeciesId == "SX") %>%
  ggplot(data = ., aes(x = BroodYear, y = res))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()

productivity_file %>%
  filter(SpeciesId == "SX") %>%
  ggplot(data = ., aes(x = BroodYear, y = res))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()+
  facet_wrap(~CU_Name)

productivity_file %>%
  filter(SpeciesId == "SX") %>%
  ggplot(data = ., aes(x = BroodYear, y = res))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()+
  facet_wrap(~Region)


## chum 
## all on one ##
windows()
productivity_file %>%
  filter(SpeciesId == "CM") %>%
  ggplot(data = ., aes(x = BroodYear, y = survival))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()

productivity_file %>%
  filter(SpeciesId == "CM") %>%
  ggplot(data = ., aes(x = BroodYear, y = survival))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()+
  facet_wrap(~CU_Name)

productivity_file %>%
  filter(SpeciesId == "CM") %>%
  ggplot(data = ., aes(x = BroodYear, y = survival))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()+
  facet_wrap(~Region)

## coho
## all on one ##
windows()
productivity_file %>%
  filter(SpeciesId == "CO") %>%
  ggplot(data = ., aes(x = BroodYear, y = res))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()

productivity_file %>%
  filter(SpeciesId == "CO") %>%
  ggplot(data = ., aes(x = BroodYear, y = res))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()+
  facet_wrap(~CU_Name)

productivity_file %>%
  filter(SpeciesId == "CO") %>%
  ggplot(data = ., aes(x = BroodYear, y = res))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()+
  facet_wrap(~Region)

## pink even
## all on one ##
windows()
productivity_file %>%
  filter(SpeciesId == "PKe") %>%
  ggplot(data = ., aes(x = BroodYear, y = res))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()

productivity_file %>%
  filter(SpeciesId == "PKe") %>%
  ggplot(data = ., aes(x = BroodYear, y = res))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()+
  facet_wrap(~CU_Name)

productivity_file %>%
  filter(SpeciesId == "PKe") %>%
  ggplot(data = ., aes(x = BroodYear, y = res))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()+
  facet_wrap(~Region)

## pink odd
## all on one ##
windows()
productivity_file %>%
  filter(SpeciesId == "PKo") %>%
  ggplot(data = ., aes(x = BroodYear, y = res))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()

productivity_file %>%
  filter(SpeciesId == "PKo") %>%
  ggplot(data = ., aes(x = BroodYear, y = res))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()+
  facet_wrap(~CU_Name)

productivity_file %>%
  filter(SpeciesId == "PKo") %>%
  ggplot(data = ., aes(x = BroodYear, y = res))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()+
  facet_wrap(~Region)


## chinook
## all on one ##
windows()
productivity_file %>%
  filter(SpeciesId == "CN") %>%
  ggplot(data = ., aes(x = BroodYear, y = res))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()

productivity_file %>%
  filter(SpeciesId == "CN") %>%
  ggplot(data = ., aes(x = BroodYear, y = res))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()+
  facet_wrap(~CU_Name)

productivity_file %>%
  filter(SpeciesId == "CN") %>%
  ggplot(data = ., aes(x = BroodYear, y = res))+
  geom_line(aes(colour = CU_Name))+
  geom_smooth()+
  theme_bw()+
  facet_wrap(~Region)



###

head(prod_file)

pink_odd_hecate_low <- subset(prod_file, CU_Name =="Hecate Strait-Lowlands" & SpeciesId=="PKo")
pink_odd_hecate_fjord <- subset(prod_file, CU_Name =="Hecate Strait-Fjords"& SpeciesId =="PKo")
pink_odd_homathko <- subset(prod_file, CU_Name ==""& SpeciesId =="PKo")

pink_odd_nass_skeena_estuary <- subset(prod_file, CU_Name =="Nass-Skeena Estuary"& SpeciesId =="PKo")
pink_odd_lower_skeena <- subset(prod_file, CU_Name =="Lower Skeena"& SpeciesId =="PKo")
pink_odd_middle_upper_skeena <- subset(prod_file, CU_Name =="Middle & Upper Skeena"& SpeciesId =="PKo")
pink_odd_nass_portland_observatory <- subset(prod_file, CU_Name =="Nass-Portland-Observatory"& SpeciesId =="PKo")

pink_odd_east_haida_gwaii <- subset(prod_file, CU_Name =="East Haida Gwaii"& SpeciesId =="PKo")
# west haida gwaii??

pink_even_hecate_low <- subset(prod_file, CU_Name =="Hecate Lowlands" & SpeciesId =="PKe")
pink_even_hecate_fjord <- subset(prod_file, CU_Name =="Hecate Strait-Fjords"& SpeciesId =="PKe")

pink_even_east_haida_gwaii <- subset(prod_file, CU_Name =="East Haida Gwaii"& SpeciesId =="PKe")
pink_even_west_haida_gwaii <- subset(prod_file, CU_Name =="West Haida Gwaii"& SpeciesId =="PKe")
pink_even_nass_skeena_estuary <- subset(prod_file, CU_Name =="Nass-Skeena Estuary"& SpeciesId =="PKe")
pink_even_middle_upper_skeena <- subset(prod_file, CU_Name =="Middle & Upper Skeena"& SpeciesId =="PKe")
pink_even_north_haida_gwaii <- subset(prod_file, CU_Name =="North Haida Gwaii"& SpeciesId =="PKe")


chum_bc_dean <- subset(prod_file, CU_Name =="Bella Colla-Dean Rivers"& SpeciesId =="CM")
chum_bc_late <- subset(prod_file, CU_Name =="Bella Coola River-Late"& SpeciesId =="CM")
chum_spiller_fh_burke <- subset(prod_file, CU_Name =="Spiller-Fitz-Hugh-Burke"& SpeciesId=="CM")
chum_smith_inlet <- subset(prod_file, CU_Name =="Smith Inlet"& SpeciesId=="CM")
chum_rivers_inlet <- subset(prod_file, CU_Name =="Rivers Inlet"& SpeciesId=="CM")
chum_hecate_low <- subset(prod_file, CU_Name =="Hecate Lowlands"& SpeciesId=="CM")
chum_mussel_kynock <- subset(prod_file, CU_Name =="Mussel-Kynock"& SpeciesId=="CM")
chum_douglas_gardner <- subset(prod_file, CU_Name =="Douglas-Gardner"& SpeciesId=="CM")

chum_east_haida_gwaii <- subset(prod_file, CU_Name =="East HG"& SpeciesId=="CM")
chum_skidegate <- subset(prod_file, CU_Name =="Skidegate"& SpeciesId=="CM")
chum_west_haida_gwaii <- subset(prod_file, CU_Name =="West Haida Gwaii"& SpeciesId=="CM")
chum_north_haida_gwaii <- subset(prod_file, CU_Name =="North Haida Gwaii"& SpeciesId=="CM")
chum_west_haida_gwaii_stanley <- subset(prod_file, CU_Name =="North Haida Gwaii-Stanley Creek"& SpeciesId=="CM")
chum_skeena_estuary <- subset(prod_file, CU_Name =="Skeena Estuary"& SpeciesId=="CM")
chum_lower_skeena <- subset(prod_file, CU_Name =="Lower Skeena"& SpeciesId=="CM")
chum_middle_skeena <- subset(prod_file, CU_Name =="Middle Skeena"& SpeciesId=="CM")
chum_portland_inlet <- subset(prod_file, CU_Name =="Portland Inlet"& SpeciesId=="CM")
chum_lower_nass <- subset(prod_file, CU_Name =="Lower Nass"& SpeciesId=="CM")
chum_portland_canal_obs <- subset(prod_file, CU_Name =="Portland Canal-Observatory"& SpeciesId=="CM")

coho_hg_east <- subset(prod_file, CU_Name =="Haida Gwaii-East"& SpeciesId =="CO")
coho_hg_west <- subset(prod_file, CU_Name =="Haida Gwaii-West"& SpeciesId =="CO")
coho_hg_graham <- subset(prod_file, CU_Name =="Haida Gwaii-Graham Island Lowlands"& SpeciesId =="CO")
coho_skeena_estuary <- subset(prod_file, CU_Name =="Skeena Estuary"& SpeciesId =="CO")
coho_lower_skeena <- subset(prod_file, CU_Name =="Lower Skeena"& SpeciesId =="CO")
coho_middle_skeena <- subset(prod_file, CU_Name =="Middle Skeena"& SpeciesId =="CO")
coho_upper_skeena <- subset(prod_file, CU_Name =="Middle Skeena"& SpeciesId =="CO")
coho_lower_nass <- subset(prod_file, CU_Name =="Lower Nass"& SpeciesId =="CO")
coho_upper_nass <- subset(prod_file, CU_Name =="Upper Nass"& SpeciesId =="CO")
coho_portland_sound <- subset(prod_file, CU_Name =="Portland Sound-Observatory Inlet-Portland Canal"& SpeciesId =="CO")

coho_bc_dean <- subset(prod_file, CU_Name =="Bella Coola-Dean Rivers"& SpeciesId =="CO")
coho_n_stream <- subset(prod_file, CU_Name =="Northern Coastal Streams"& SpeciesId =="CO")
coho_smith <- subset(prod_file, CU_Name =="Smith Inlet"& SpeciesId =="CO")
coho_rivers <- subset(prod_file, CU_Name =="Rivers Inlet"& SpeciesId =="CO")
coho_mussel_kynoch <- subset(prod_file, CU_Name =="Mussel-Kynoch"& SpeciesId =="CO")
coho_hecate_strait_mainland <- subset(prod_file, CU_Name =="Hecate Strait Mainland"& SpeciesId =="CO")
coho_brim_whaoo <- subset(prod_file, CU_Name =="Brim-Wahoo"& SpeciesId =="CO")
coho_douglas_channel <- subset(prod_file, CU_Name =="Douglas Channel-Kitimat Arm"& SpeciesId =="CO")


chinook_hg_north <- subset(prod_file, CU_Name =="Haida Gwaii-North"& SpeciesId =="CN")
chinook_ecstall <- subset(prod_file, CU_Name =="Ecstall"& SpeciesId =="CN")
chinook_kalum_early <- subset(prod_file, CU_Name =="Kalum-early timing"& SpeciesId =="CN")
chinook_kalum_late <- subset(prod_file, CU_Name =="Kalum-late timing"& SpeciesId =="CN")
chinook_lower_skeena <- subset(prod_file, CU_Name =="Lower Skeena"& SpeciesId =="CN")
chinook_middle_skeeka_l_lakes <- subset(prod_file, CU_Name =="Middle Skeena-large lakes"& SpeciesId =="CN")
chinook_middle_skeeka_m_trib <- subset(prod_file, CU_Name =="Middle Skeena-mainstem tributaries"& SpeciesId =="CN")
chinook_portland_sound <- subset(prod_file, CU_Name =="Portland Sound-Observatory Inlet-Lower Nass"& SpeciesId =="CN")
chinook_upper_bulkley <- subset(prod_file, CU_Name =="Upper Bulkley River"& SpeciesId =="CN")
chinook_upper_nass <- subset(prod_file, CU_Name =="Upper Nass"& SpeciesId =="CN")
#chinook_zymoetz <- subset(prod_file, CU_Name =="Zymoetz"& SpeciesId =="CN")
chinook_bc_bentick <- subset(prod_file, CU_Name =="Bella Coola-Bentinck"& SpeciesId =="CN")
chinook_dean <- subset(prod_file, CU_Name =="Dean River"& SpeciesId =="CN")
chinook_docee <- subset(prod_file, CU_Name =="Docee"& SpeciesId =="CN")
chinook_ncc_early <- subset(prod_file, CU_Name =="North & Central Coast-early timing"& SpeciesId =="CN")
chinook_rivers_inlet <- subset(prod_file, CU_Name =="Rivers Inlet"& SpeciesId =="CN")
chinook_wannock <- subset(prod_file, CU_Name =="Wannock"& SpeciesId =="CN")
chinook_ncc_late <- subset(prod_file, CU_Name =="North & Central Coast-late timing"& SpeciesId =="CN")

# central coast
sx_atnarko<-read.delim(file="Atnarko_production_file.Oct22015.txt",header=TRUE)
sx_port_john <- subset(prod_file, CU_Name =="Port John")
sx_namu <- subset(prod_file, CU_Name =="Namu")
sx_koeye <- subset(prod_file, CU_Name =="Koeye")
sx_kadjusdis <- subset(prod_file, CU_Name =="Kadjusdis River")
sx_kainet <- subset(prod_file, CU_Name =="Kainet Creek")
sx_mary_cove <- subset(prod_file, CU_Name =="Mary Cove Creek")
sx_roderick <- subset(prod_file, CU_Name =="Roderick")
sx_tankeenah <- subset(prod_file, CU_Name =="Tankeeah River")
sx_yeo <- subset(prod_file, CU_Name =="Yeo")
sx_owikeno <- subset(prod_file, CU_Name =="Owikeno")
sx_long <- subset(prod_file, CU_Name =="Long")

sx_bloomfield <- subset(prod_file, CU_Name =="Bloomfield")
sx_curtis <- subset(prod_file, CU_Name =="Curtis Inlet")
sx_devon <- subset(prod_file, CU_Name =="Devon")
sx_freeda <- subset(prod_file, CU_Name =="Freeda")
sx_hartley <- subset(prod_file, CU_Name =="Hartley Bay")
sx_keecha <- subset(prod_file, CU_Name =="Keecha")
sx_kooryet <- subset(prod_file, CU_Name =="Kooryet")
sx_kwakwa <- subset(prod_file, CU_Name =="Kwakwa Creek")
sx_lowe_simpson_weir <- subset(prod_file, CU_Name =="Lowe/Simpson/Weir")
sx_mikado <- subset(prod_file, CU_Name =="Mikado")
sx_prudhomme <- subset(prod_file, CU_Name =="Prudhomme")
sx_shawatlan <- subset(prod_file, CU_Name =="Shawatlan")
sx_tsimtack_moore_roger <- subset(prod_file, CU_Name =="Tsimtack/Moore/Roger")



# haida gwaii
sx_awun <- subset(prod_file, CU_Name =="Awun")
sx_marian <- subset(prod_file, CU_Name =="Marian")
sx_mathers <- subset(prod_file, CU_Name =="Mathers")
sx_mercer <- subset(prod_file, CU_Name =="Mercer")
sx_skidegate <- subset(prod_file, CU_Name =="Skidegate"& SpeciesId =="SX")
sx_yakoun <- subset(prod_file, CU_Name =="Yakoun")

# skeena/nass
sx_backland <- subset(prod_file, CU_Name =="Backland")
sx_canoona <- subset(prod_file, CU_Name =="Canoona")
sx_evelyn <- subset(prod_file, CU_Name =="Evelyn")
sx_kainet <- subset(prod_file, CU_Name =="Kainet Creek")
sx_kitlope <- subset(prod_file, CU_Name =="Kitlope")
sx_alastair <- subset(prod_file, CU_Name =="Alastair")
sx_johnston <- subset(prod_file, CU_Name =="Johnston")
sx_kitsumkalum <- subset(prod_file, CU_Name =="Kitsumkalum")
sx_lakelse <- subset(prod_file, CU_Name =="Lakelse")
sx_mcdonell <- subset(prod_file, CU_Name =="Mcdonell")
sx_babine_early_wild <- subset(prod_file, CU_Name =="Babine-Early-Wild")
sx_babine_fulton <- subset(prod_file, CU_Name =="Babine-Fulton")
sx_babine_late_wild <- subset(prod_file, CU_Name =="Babine-Late-Wild")
sx_babine_mid_wild <- subset(prod_file, CU_Name =="Babine-Mid-Wild")
sx_babine_pinkut <- subset(prod_file, CU_Name =="Babine-Pinkut")
sx_kitwancool <- subset(prod_file, CU_Name =="Kitwancool")
sx_morice <- subset(prod_file, CU_Name =="Morice")
sx_stephens <- subset(prod_file, CU_Name =="Stephens")
sx_swan <- subset(prod_file, CU_Name =="Swan")
sx_tahlo_morrison <- subset(prod_file, CU_Name =="Tahlo/Morrison")
sx_asitika <- subset(prod_file, CU_Name =="Asitika")
sx_azuklotz <- subset(prod_file, CU_Name =="Azuklotz")
sx_bear <- subset(prod_file, CU_Name =="Bear")
sx_damshilgwit <- subset(prod_file, CU_Name =="Damshilgwit")
sx_motase <- subset(prod_file, CU_Name =="Motase")
sx_damdochax <- subset(prod_file, CU_Name =="Damdochax")
sx_fred_wright <- subset(prod_file, CU_Name =="Fred Wright")
sx_meziadin <- subset(prod_file, CU_Name =="Meziadin")
sx_north_coast_fj <- subset(prod_file, CU_Name =="Northern Coastal Fjords")
sx_skeena_high_int <- subset(prod_file, CU_Name =="Skeena River-high interior")
sx_lower_nass_portland <- subset(prod_file, CU_Name =="Lower Nass-Portland")
sx_upper_nass <- subset(prod_file, CU_Name =="Upper Nass River")



###select CU
CU_data <- coho_bc_dean
#bind harvest and catch data 
counts <- rbind(CU_data$TE, CU_data$Total.Harvest,CU_data$Year)

####Run size plot###################################################################

	dev.new(width=6.5, height=3.5,new=FALSE)
	par(mar=c(3.5,3.5,0.5,3.5))

		barplot(counts[1:2,]/1000,xlab="",ylab="",col=c(grey(0.25),grey(1)), space=0.25,bty="n",yaxt="n")
		axis(2,line=-0.7,las=1)
		
		par(mar=c(2.9,3.5,2.25,4),new=TRUE)
		plot(counts[3,],seq(0,1,length.out=length(counts[3,])),xaxt="n",yaxt="n",xlab="",ylab="",type="n",bty="n")
		axis(1,at=c(1955,1965,1975,1985,1995,2005,2014),line=-0.5)

		mtext("Number of fish (000s)",side=2,line=1.8,las=3,adj=0.5,cex=1)
		mtext("Year",side=1,line=2.0,las=1.3,cex=1)
		legend(2002,1,legend=c("Spawners","Catch"),fill=c(grey(0.25),grey(1)) ,border=c("black","black"), bty="n",ncol=1,cex=0.9)



#####Atnarko only
counts <- rbind(sx_atnarko $spaw, sx_atnarko $fsc_catch, sx_atnarko $com_catch)
sx_atnarko $er[1:3]<-NA

####Run size plot###################################################################

	dev.new(width=6.5, height=3.5,new=FALSE)
	par(mar=c(3.5,3.5,0.5,3.5))

		barplot(counts[,4:43]/1000,xlab="",ylab="",col=c(grey(0.25),grey(0.75),grey(1)), space=0.25,bty="n",yaxt="n",ylim=c(0,120))
		axis(2,at=c(0,10,20,30,40,50,60,70,80,90,100,110),line=-0.7,las=1)
		
		par(mar=c(2.9,3.5,2.25,4),new=TRUE)
		plot(c(1972:2015), sx_atnarko $er,col="red",xaxt="n",yaxt="n",xlab="",ylab="",ylim=c(0,1),lwd=3,type="n",bty="n")
		axis(1,at=c(1975,1980,1985,1990,1995,2000,2005,2010,2015),line=-0.5)

		mtext("Number of fish (000s)",side=2,line=1.8,las=3,adj=0.5,cex=1)
		mtext("Year",side=1,line=2.0,las=1.3,cex=1)
		legend(1998,1,legend=c("Escapement","FSC catch","Commercial catch"),fill=c(grey(0.25),grey(0.75),grey(1)) ,border=c("black","black","black"), bty="n",ncol=1,cex=0.9)
		
#####scaled time series		

windows(width=8, height=6)
par(mfrow=c(2,1),mar=c(2.5,2,1,0.5),oma=c(3.5,3.5,1,1))

#pink odd and even together, regions colour coded
quartz()
plot(pink_odd_hecate_low$Year, scale(pink_odd_hecate_low $Total),col="maroon2",xaxt="n",yaxt="n",xlab="",ylab="",type="l",lwd=2,bty="n",ylim=c(-2,5),main="Pink (odd years)",xlim=c(1955,2015))
axis(2,las=2)
axis(1)		
points(pink_odd_hecate_fjord $Year, scale(pink_odd_hecate_fjord $Total),col="navy",type="l",lwd=2)
points(pink_odd_homathko $Year, scale(pink_odd_homathko $Total),col="olivedrab3",type="l",lwd=2)
box()
legend(1965,5,legend=c("Hecate Strait Low.", "Hecate Strait Fjords", "Homathko"),lwd=c(2,2) ,col=c("maroon2", "navy","olivedrab3"), bty="n",ncol=1,cex=0.9)		

mtext("Relative abundance",2,outer=T,line=1)
mtext("Return year",1,outer=T,line=1)

#pink even
plot(pink_even_hecate_low $Year, scale(pink_even_hecate_low $Total.Run),col="maroon2",xaxt="n",yaxt="n",xlab="",ylab="",type="l",lwd=2,bty="n",ylim=c(-2,5),main="Pink (even years)",xlim=c(1955,2015))
axis(2,las=2)
axis(1)		
points(pink_even_hecate_fjord $Year, scale(pink_even_hecate_fjord $Total.Run),col="olivedrab",type="l",lwd=2)
box()
legend(1965,5,legend=c("Hecate Strait Low.", "Hecate Strait Fjords"),lwd=c(2,2) ,col=c("maroon2", "olivedrab"), bty="n",ncol=1,cex=0.9)		

mtext("Relative abundance",2,outer=T,line=1)
mtext("Return year",1,outer=T,line=1)

####
## chum - has 18 CUs, too messy on a sinle plot seperated by colour
### three plots, side by side: Haida Gwaii, Skeena/Nass, Central Coast
windows(width=8, height=6)
par(mfrow=c(2,1),mar=c(2.5,2,1,0.5),oma=c(3.5,3.5,1,1))


#chum
#Central Coast
plot(c(1955:2015), scale(chum_hecate_low$Total.Run),col="tomato1",xaxt="n",yaxt="n",xlab="",ylab="",type="l",lwd=2,bty="n",ylim=c(-2,5),main="Chum-Central Coast")
axis(2,las=2)
axis(1)		
points(c(1955:2015), scale(chum_spiller_fh_burke $Total.Run),col="olivedrab4",type="l",lwd=2)
points(c(1955:2015), scale(chum_mussel_kynock $Total.Run),col="tomato4",type="l",lwd=2)
points(c(1955:2015), scale(chum_douglas_gardner $Total.Run),col="olivedrab3",type="l",lwd=2)
box()
legend(1975,5.45,legend=c("Hecate Low", "Spiller-FH", "Mussel-Kynock", "Douglas Gardner"),lwd=c(2,2) ,col=c("tomato1","olivedrab4","tomato4", "olivedrab3"), bty="n",ncol=1,cex=0.9)		




plot(c(1955:2015), scale(chum_bc_dean$Total.Run),col="maroon2",xaxt="n",yaxt="n",xlab="",ylab="",type="l",lwd=2,bty="n",ylim=c(-2,5))
axis(2,las=2)
axis(1)		
points(c(1955:2015), scale(chum_bc_late $Total.Run),col="skyblue",type="l",lwd=2)
points(c(1955:2015), scale(chum_rivers_inlet $Total.Run),col="springgreen4",type="l",lwd=2)
points(c(1955:2015), scale(chum_smith_inlet $Total.Run),col="midnightblue",type="l",lwd=2)
box()
legend(1960,5.45,legend=c("BC-Dean", "BC Late", "Rivers", "Smith"),lwd=c(2,2) ,col=c("maroon2","skyblue","springgreen4","midnightblue"), bty="n",ncol=1,cex=0.9)		

mtext("Relative abundance",2,outer=T,line=1)
mtext("Return year",1,outer=T,line=1)


#coho

# Central coast
windows(width=8, height=6)
par(mfrow=c(2,1),mar=c(2.5,2,1,0.5),oma=c(3.5,3.5,1,1))

plot(c(1955:2015), scale(coho_hecate_strait_mainland $Total.Run),col="yellow2",xaxt="n",yaxt="n",xlab="",ylab="",type="l",lwd=2,bty="n",ylim=c(-2,5),main="Coho-Central Coast")
axis(2,las=2)
axis(1)		
points(c(1955:2015), scale(coho_douglas_channel $Total.Run),col="magenta4",type="l",lwd=2)
points(c(1955:2015), scale(coho_brim_whaoo $Total.Run),col="magenta1",type="l",lwd=2)
points(c(1955:2015), scale(coho_mussel_kynoch $Total.Run),col="orange2",type="l",lwd=2)
legend(1988,5.45,legend=c("Hecate Strait Main","Douglas Channel", "Brim-Wahoo","Mussel-Kynoch"),lwd=c(2,2) ,
       col=c("yellow2","magenta4","magenta1", "orange2"), bty="n",ncol=1,cex=0.9)		
box()

plot(c(1955:2015), scale(coho_n_stream $Total.Run),col="skyblue4",xaxt="n",yaxt="n",xlab="",ylab="",type="l",lwd=2,bty="n",ylim=c(-2,5),main="Coho-Central Coast")
axis(2,las=2)
axis(1)		
points(c(1955:2015), scale(coho_bc_dean $Total.Run),col="sienna3",type="l",lwd=2)
points(c(1955:2015), scale(coho_smith $Total.Run),col="navy",type="l",lwd=2)
points(c(1955:2015), scale(coho_rivers $Total.Run),col="springgreen4",type="l",lwd=2)
box()

legend(1988,5.45,legend=c("BC Dean", "Northern Streams",  "Rivers", "Smith"),lwd=c(2,2) ,
       col=c("skyblue4","sienna3","springgreen4","navy"), bty="n",ncol=1,cex=0.9)		

mtext("Relative abundance",2,outer=T,line=1)
mtext("Return year",1,outer=T,line=1)



#chinook
# Central coast
quartz(width=8, height=7)
par(mfrow=c(2,1),mar=c(2.5,2,1,0.5),oma=c(3.5,3.5,1,1))


plot(chinook_ncc_early $Year, scale(chinook_ncc_early $Total),col="purple4",xaxt="n",yaxt="n",xlab="",ylab="",type="l",lwd=2,bty="n",ylim=c(-2,5),main="Chinook-Central Coast")
axis(2,las=2)
axis(1)		
points(chinook_ncc_late $Year, scale(chinook_ncc_late $Total),col="lightpink",type="l",lwd=2)
points(chinook_ncc_early $Year, scale(chinook_dean $Total),col="purple",type="l",lwd=2)
abline(h=0, lty=2, lwd=2)
box()
legend(1960,5.45,legend=c("NCC Early","NCC Late", "Dean"),lwd=c(2,2) ,
       col=c("purple4","purple","lightpink"), bty="n",ncol=1,cex=0.9)		



plot(chinook_ncc_early $Year, scale(chinook_bc_bentick $Total),col="violetred4",xaxt="n",yaxt="n",xlab="",ylab="",type="l",lwd=2,bty="n",ylim=c(-2,5),main="Chinook-Central Coast")
axis(2,las=2)
axis(1)		
points(chinook_ncc_early $Year, scale(chinook_rivers_inlet $Total),col="pink3",type="l",lwd=2)
points(chinook_ncc_early $Year, scale(chinook_wannock $Total),col="magenta",type="l",lwd=2)
points(chinook_ncc_early $Year, scale(chinook_docee $Total),col="grey30",type="l",lwd=2)
abline(h=0, lty=2, lwd=2)
box()
# legends next



legend(1960,5.45,legend=c("Bella Coola-Bentick","Rivers Inlet", "Wannock",  "Docee"),lwd=c(2,2) ,
       col=c("violetred4","pink3", "magenta","grey30"), bty="n",ncol=1,cex=0.9)		

mtext("Relative abundance",2,outer=T,line=1)
mtext("Return year",1,outer=T,line=1)


# Central coast pink
quartz(width=8, height=7)
par(mfrow=c(2,1),mar=c(2.5,2,1,0.5),oma=c(3.5,3.5,1,1))


plot(pink_odd_hecate_low $Year, scale(pink_odd_hecate_low $Total),col="purple4",xaxt="n",yaxt="n",xlab="",ylab="",type="l",lwd=2,bty="n",ylim=c(-2,5),main="Pink odd-Central Coast")
axis(2,las=2)
axis(1)		
points(pink_odd_hecate_fjord $Year, scale(pink_odd_hecate_fjord $Total),col="lightpink",type="l",lwd=2)
points(pink_odd_homathko $Year, scale(pink_odd_homathko $Total),col="purple",type="l",lwd=2)
abline(h=0, lty=2, lwd=2)
box()
legend(1960,5.45,legend=c("Hecate low","Hecate fjords", "Homathko"),lwd=c(2,2) ,
       col=c("purple4","purple","lightpink"), bty="n",ncol=1,cex=0.9)		



plot(pink_even_hecate_low $Year, scale(pink_even_hecate_low $Total),col="violetred4",xaxt="n",yaxt="n",xlab="",ylab="",type="l",lwd=2,bty="n",ylim=c(-2,5),main="Pink even-Central Coast")
axis(2,las=2)
axis(1)		
points(pink_even_hecate_fjord $Year, scale(pink_even_hecate_fjord $Total),col="pink3",type="l",lwd=2)
abline(h=0, lty=2, lwd=2)
box()
# legends next



legend(1960,5.45,legend=c("Hecate low","Hecate fjords"),lwd=c(2,2) ,
       col=c("violetred4","pink3"), bty="n",ncol=1,cex=0.9)		

mtext("Relative abundance",2,outer=T,line=1)
mtext("Return year",1,outer=T,line=1)




#sockeye
windows(width=8, height=10)
par(mfrow=c(3,1),mar=c(2.5,2,1,0.5),oma=c(3.5,3.5,1,1))

plot(c(1955:2015), scale(sx_kainet $Total.Run),col="magenta",xaxt="n",yaxt="n",xlab="",ylab="",type="l",lwd=2,bty="n",ylim=c(-2,5), ,main="Sockeye")
axis(2,las=2)
axis(1)		
points(c(1955:2015), scale(sx_mary_cove $Total.Run),col="yellow",type="l",lwd=2)
points(c(1955:2015), scale(sx_roderick $Total.Run),col="gray",type="l",lwd=2)
points(c(1955:2015), scale(sx_yeo $Total.Run),col="orange",type="l",lwd=2)
box()
legend(1995,5,legend=c("Kainet","Mary Cove","Roderick", "Yeo"),lwd=c(2,2,2,2) 
       ,col=c("magenta","yellow","gray","orange"), bty="n",ncol=1,cex=0.9)		



plot(c(1955:2015), scale(sx_tankeenah $Total.Run),col="green",xaxt="n",yaxt="n",xlab="",ylab="",type="l",lwd=2,bty="n",ylim=c(-2,5))
axis(2,las=2)
axis(1)		
points(c(1955:2015), scale(sx_kadjusdis $Total.Run),col="cyan",type="l",lwd=2)
points(c(1955:2015), scale(sx_port_john $Total.Run),col="black",type="l",lwd=2)
points(c(1955:2015), scale(sx_namu $Total.Run),col="darkgreen",type="l",lwd=2)
box()


legend(1995,5,legend=c("Tankeeah","Kadjusdis","Port John", "Namu"),lwd=c(2,2,2,2) 
       ,col=c("green","cyan","black","darkgreen"), bty="n",ncol=1,cex=0.9)		


head(sx_atnarko)
plot(c(1955:2015), scale(sx_koeye $Total.Run),col="blue",xaxt="n",yaxt="n",xlab="",ylab="",type="l",lwd=2,bty="n",ylim=c(-2,5))
axis(2,las=2)
axis(1)		
points(c(1972:2015), scale(sx_atnarko $run),col="red",type="l",lwd=2)
points(c(1955:2015), scale(sx_owikeno $Total.Run),col="gray30",type="l",lwd=2)
points(c(1955:2015), scale(sx_long $Total.Run),col="purple",type="l",lwd=2)
box()
legend(1995,5,legend=c("Koeye","Atnarko", "Owikeno","Long"),lwd=c(2,2,2,2) 
       ,col=c("blue","red","gray30","purple"), bty="n",ncol=1,cex=0.9)		

mtext("Relative abundance",2,outer=T,line=1)
mtext("Return year",1,outer=T,line=1)


#####
#rolling 10 year windows for chum productivity


start <- min(expanded_df$Year)
end <- max(expanded_df$Year)
# Make an empty list to store stuff in from the loop
cor_df_list <- list()

#check results
dat1 <- filter(expanded_df_prod, Year < 1964)
cor(dat1$survival.x, dat1$survival.y, use = "complete.obs")

for (i in 0:(end - start - 9)) {
  years <- (start + i):(start + i + 9)
  df <- filter(expanded_df_prod, Year %in% years)
  # Get useful values that you might want to reference later
  # You can add more here
  corr <- cor(df$survival.x, df$survival.y, use = "complete.obs")
  min_year <- min(df$Year)
  max_year <- max(df$Year)
  # Dump stuff in the list
  cor_df_list[[i + 1]] <- data_frame(min_year = min_year, max_year =
                                       max_year,
                                     corr = corr)
}


dat_cm <- bind_rows(cor_df_list)

quartz()
qplot(min_year, corr, data = dat_cm, main = "Chum")+
  geom_point()+
  geom_smooth()


#rolling 10 year windows for coho productivity


start <- min(expanded_df$Year)
end <- max(expanded_df$Year)
# Make an empty list to store stuff in from the loop
cor_df_list <- list()

#check results
dat1 <- filter(coho_df_prod, Year < 1964)
cor(dat1$survival.x, dat1$survival.y, use = "complete.obs")

for (i in 0:(end - start - 9)) {
  years <- (start + i):(start + i + 9)
  df <- filter(coho_df_prod, Year %in% years)
  # Get useful values that you might want to reference later
  # You can add more here
  corr <- cor(df$survival.x, df$survival.y, use = "complete.obs")
  min_year <- min(df$Year)
  max_year <- max(df$Year)
  # Dump stuff in the list
  cor_df_list[[i + 1]] <- data_frame(min_year = min_year, max_year =
                                       max_year,
                                     corr = corr)
}


dat_co <- bind_rows(cor_df_list)

quartz()
qplot(min_year, corr, data = dat_co, main = "Coho")+
  geom_point()+
  geom_smooth()


#rolling 10 year windows for sockeye productivity


start <- min(expanded_df$Year)
end <- max(expanded_df$Year)
# Make an empty list to store stuff in from the loop
cor_df_list <- list()

#check results
dat1 <- filter(sx_df_prod, Year < 1964)
cor(dat1$survival.x, dat1$survival.y, use = "complete.obs")

for (i in 0:(end - start - 9)) {
  years <- (start + i):(start + i + 9)
  df <- filter(sx_df_prod, Year %in% years)
  # Get useful values that you might want to reference later
  # You can add more here
  corr <- cor(df$survival.x, df$survival.y, use = "complete.obs")
  min_year <- min(df$Year)
  max_year <- max(df$Year)
  # Dump stuff in the list
  cor_df_list[[i + 1]] <- data_frame(min_year = min_year, max_year =
                                       max_year,
                                     corr = corr)
}


dat_sx <- bind_rows(cor_df_list)

quartz()
qplot(min_year, corr, data = dat_sx, main = "Sockeye")+
  geom_point()+
  geom_smooth()


#rolling 10 year windows for pink even productivity


start <- min(expanded_df$Year)
end <- max(expanded_df$Year)
# Make an empty list to store stuff in from the loop
cor_df_list <- list()

#check results
dat1 <- filter(pke_df_prod, Year < 1964)
cor(dat1$survival.x, dat1$survival.y, use = "complete.obs")

for (i in 0:(end - start - 9)) {
  years <- (start + i):(start + i + 9)
  df <- filter(pke_df_prod, Year %in% years)
  # Get useful values that you might want to reference later
  # You can add more here
  corr <- cor(df$survival.x, df$survival.y, use = "complete.obs")
  min_year <- min(df$Year)
  max_year <- max(df$Year)
  # Dump stuff in the list
  cor_df_list[[i + 1]] <- data_frame(min_year = min_year, max_year =
                                       max_year,
                                     corr = corr)
}


dat_pke <- bind_rows(cor_df_list)

quartz()
qplot(min_year, corr, data = dat_pke, main = "Pink Even")+
  geom_point()+
  geom_smooth()



#rolling 10 year windows for pink odd productivity


start <- min(expanded_df$Year)
end <- max(expanded_df$Year)
# Make an empty list to store stuff in from the loop
cor_df_list <- list()

#check results
dat1 <- filter(pko_df_prod, Year < 1964)
cor(dat1$survival.x, dat1$survival.y, use = "complete.obs")

for (i in 0:(end - start - 9)) {
  years <- (start + i):(start + i + 9)
  df <- filter(pko_df_prod, Year %in% years)
  # Get useful values that you might want to reference later
  # You can add more here
  corr <- cor(df$survival.x, df$survival.y, use = "complete.obs")
  min_year <- min(df$Year)
  max_year <- max(df$Year)
  # Dump stuff in the list
  cor_df_list[[i + 1]] <- data_frame(min_year = min_year, max_year =
                                       max_year,
                                     corr = corr)
}


dat_pko <- bind_rows(cor_df_list)

quartz()
qplot(min_year, corr, data = dat_pko, main = "Pink Odd")+
  geom_point()+
  geom_smooth()


#plot them all

quartz(width=8, height=10)

plot1 <- qplot(min_year, corr, data = dat, xlab="Correlation coefficent",ylab="Year",main = "Chum")+
  geom_point()+
  geom_smooth()

plot2 <- qplot(min_year, corr, data = dat_co, xlab="Correlation coefficent",ylab="Year",main = "Coho")+
  geom_point()+
  geom_smooth()

plot3 <- qplot(min_year, corr, data = dat_sx, xlab="Correlation coefficent",ylab="Year",main = "Sockeye")+
  geom_point()+
  geom_smooth()

plot4 <- qplot(min_year, corr, data = dat_pke, xlab="Correlation coefficent",ylab="Year",main = "Pink even")+
  geom_point()+
  geom_smooth()

plot5 <- qplot(min_year, corr, data = dat_pko, xlab="Correlation coefficent",ylab="Year",main = "Pink odd")+
  geom_point()+
  geom_smooth()

require(gridExtra)
grid.arrange(plot1, plot2, plot3, plot4, plot5, ncol=2, nrow=3)













#####
#rolling 10 year windows for chum


start <- min(expanded_df$Year)
end <- max(expanded_df$Year)
# Make an empty list to store stuff in from the loop
cor_df_list <- list()

#check results
dat1 <- filter(expanded_df, Year < 1964)
cor(dat1$Total.Run.x, dat1$Total.Run.y, use = "complete.obs")

for (i in 0:(end - start - 9)) {
  years <- (start + i):(start + i + 9)
  df <- filter(expanded_df, Year %in% years)
  # Get useful values that you might want to reference later
  # You can add more here
  corr <- cor(df$Total.Run.x, df$Total.Run.y, use = "complete.obs")
  min_year <- min(df$Year)
  max_year <- max(df$Year)
  # Dump stuff in the list
  cor_df_list[[i + 1]] <- data_frame(min_year = min_year, max_year =
                                       max_year,
                                     corr = corr)
}


dat <- bind_rows(cor_df_list)

quartz()
qplot(min_year, corr, data = dat, main = "Chum")+
  geom_point()+
  geom_smooth()



#####
#rolling 10 year windows for coho


start <- min(expanded_df_co$Year)
end <- max(expanded_df_co$Year)
# Make an empty list to store stuff in from the loop
cor_df_list <- list()

#check results
dat1 <- filter(expanded_df_co, Year < 1964)
cor(dat1$Total.Run.x, dat1$Total.Run.y, use = "complete.obs")

for (i in 0:(end - start - 9)) {
  years <- (start + i):(start + i + 9)
  df <- filter(expanded_df_co, Year %in% years)
  # Get useful values that you might want to reference later
  # You can add more here
  corr <- cor(df$Total.Run.x, df$Total.Run.y, use = "complete.obs")
  min_year <- min(df$Year)
  max_year <- max(df$Year)
  # Dump stuff in the list
  cor_df_list[[i + 1]] <- data_frame(min_year = min_year, max_year =
                                       max_year,
                                     corr = corr)
}


dat_co <- bind_rows(cor_df_list)

quartz()
qplot(min_year, corr, data = dat_co, main = "Coho")+
  geom_point()+
  geom_smooth()


#####
#rolling 10 year windows for sockeye


start <- min(expanded_df_sx$Year)
end <- max(expanded_df_sx$Year)
# Make an empty list to store stuff in from the loop
cor_df_list <- list()

#check results
dat2 <- filter(expanded_df_sx, Year < 1964)
cor(dat2$Total.Run.x, dat2$Total.Run.y, use = "complete.obs")

for (i in 0:(end - start - 9)) {
  years <- (start + i):(start + i + 9)
  df <- filter(expanded_df_sx, Year %in% years)
  # Get useful values that you might want to reference later
  # You can add more here
  corr <- cor(df$Total.Run.x, df$Total.Run.y, use = "complete.obs")
  min_year <- min(df$Year)
  max_year <- max(df$Year)
  # Dump stuff in the list
  cor_df_list[[i + 1]] <- data_frame(min_year = min_year, max_year =
                                       max_year,
                                     corr = corr)
}


dat_sx <- bind_rows(cor_df_list)

quartz()
qplot(min_year, corr, data = dat_sx, main = "Sockeye")+
  geom_point()+
  geom_smooth()


#rolling 10 year windows for pke


start <- min(expanded_df_pke$Year)
end <- max(expanded_df_pke$Year)
# Make an empty list to store stuff in from the loop
cor_df_list <- list()

#check results
dat2 <- filter(expanded_df_pke, Year < 1964)
cor(dat2$Total.Run.x, dat2$Total.Run.y, use = "complete.obs")

for (i in 0:(end - start - 9)) {
  years <- (start + i):(start + i + 9)
  df <- filter(expanded_df_pke, Year %in% years)
  # Get useful values that you might want to reference later
  # You can add more here
  corr <- cor(df$Total.Run.x, df$Total.Run.y, use = "na.or.complete")
  min_year <- min(df$Year)
  max_year <- max(df$Year)
  # Dump stuff in the list
  cor_df_list[[i + 1]] <- data_frame(min_year = min_year, max_year =
                                       max_year,
                                     corr = corr)
}


dat_pke <- bind_rows(cor_df_list)

windows()
qplot(min_year, corr, data = dat_pke, main = "Pink (even)")+
  geom_point()+
  geom_smooth()


#rolling 10 year windows for pko


start <- min(expanded_df_pko$Year)
end <- max(expanded_df_pko$Year)
# Make an empty list to store stuff in from the loop
cor_df_list <- list()

#check results
dat2 <- filter(expanded_df_pko, Year < 1964)
cor(dat2$Total.Run.x, dat2$Total.Run.y, use = "complete.obs")

for (i in 0:(end - start - 9)) {
  years <- (start + i):(start + i + 9)
  df <- filter(expanded_df_pko, Year %in% years)
  # Get useful values that you might want to reference later
  # You can add more here
  corr <- cor(df$Total.Run.x, df$Total.Run.y, use = "na.or.complete")
  min_year <- min(df$Year)
  max_year <- max(df$Year)
  # Dump stuff in the list
  cor_df_list[[i + 1]] <- data_frame(min_year = min_year, max_year =
                                       max_year,
                                     corr = corr)
}


dat_pko <- bind_rows(cor_df_list)

windows()
qplot(min_year, corr, data = dat_pko, main = "Pink (odd)")+
  geom_point()+
  geom_smooth()



####  #####
#rolling 10 year windows for chum productivity
spp_list_prod <- split(productivity_file, productivity_file$SpeciesId)

expanded_df_prod <-
  left_join(spp_list_prod$CM, select(spp_list_prod$CM, CU_Name, BroodYear, logRS), by = "BroodYear" ) %>%
  arrange(CU_Name.x, CU_Name.y)




start <- min(expanded_df_prod$BroodYear)
end <- max(expanded_df_prod$BroodYear)
# Make an empty list to store stuff in from the loop
cor_df_list <- list()

#check results
dat1 <- filter(expanded_df_prod, BroodYear < 1964)
cor(dat1$logRS.x, dat1$logRS.y, use = "complete.obs")

for (i in 0:(end - start - 9)) {
  years <- (start + i):(start + i + 9)
  df <- filter(expanded_df_prod, BroodYear %in% years)
  # Get useful values that you might want to reference later
  # You can add more here
  corr <- cor(df$logRS.x, df$logRS.y, use = "complete.obs")
  min_year <- min(df$BroodYear)
  max_year <- max(df$BroodYear)
  # Dump stuff in the list
  cor_df_list[[i + 1]] <- data_frame(min_year = min_year, max_year =
                                       max_year,
                                     corr = corr)
}


dat_cm_prod <- bind_rows(cor_df_list)

quartz()
qplot(min_year, corr, data = dat_cm_prod, main = "Chum")+
  geom_point()+
  geom_smooth()

#ck
Dat = matrix(c(5/7*100,1/7*100,0,1/7*100), nrow=4, ncol=1)

quartz(width=2.5, height=3.5)
barplot(Dat, col=c("green4","yellow2","red","grey"), axes=F)
axis(2, las=1)

#cm
Dat = matrix(c(2/9*100,3/9*100,2/9*100,2/9*100), nrow=4, ncol=1)

quartz(width=2.5, height=3.5)
barplot(Dat, col=c("green4","yellow2","red","grey"), axes=F)
axis(2, las=1)


#co
Dat = matrix(c(4/8*100,3/8*100,0/8*100,1/8*100), nrow=4, ncol=1)

quartz(width=2.5, height=3.5)
barplot(Dat, col=c("green4","yellow2","red","grey"), axes=F)
axis(2, las=1)


#pk
Dat = matrix(c(3/5*100,0/8*100,2/5*100,0/8*100), nrow=4, ncol=1)

quartz(width=2.5, height=3.5)
barplot(Dat, col=c("green4","yellow2","red","grey"), axes=F)
axis(2, las=1)

#sx
Dat = matrix(c(16/85*100,0/8*100,9/85*100,60/85*100), nrow=4, ncol=1)

quartz(width=2.5, height=3.5)
barplot(Dat, col=c("green4","yellow2","red","grey"), axes=F)
axis(2, las=1)



mfr_file <- read.csv("MFR_spring2.csv", header = T)

quartz(width=10, height=5)
par(mar=c(0,1.5,1.5,1)+.1,mfrow=c(1,1))

mfr_file %>%
  ggplot(data = ., aes(x = Year, y = Data))+
  ylab("Spawners")+
  geom_line(aes(colour = Group))+
  geom_point(aes(colour = Group))+
  scale_color_manual(values=c("#00CCFF", "#0066FF", "#56B4E9", "#E69F00"))+
  theme_bw()+
  theme(panel.grid.major = element_blank())+
  theme(panel.grid.minor = element_blank())


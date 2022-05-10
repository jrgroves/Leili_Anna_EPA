#Modified Leili and Anna Paper Script[Original on OneDrive]
#Census API Download to check validity of census data


rm(list=ls())

library(tidyverse)
library(sf)
library(readxl)

core<-read.csv("./Data/paper1_data.csv")

cendat<-read.csv("./Data/nhgis0015_ts_nominal_tract.csv", as.is=TRUE)

cendat<-cendat %>%
  select(!AV0AA125)%>%
  rename_with( ~gsub("125", "2010", .x, fixed=TRUE)) %>%
  select(!contains("195"),
         !ends_with("M"))

load("./Data/sites.RData")  
  sites<-rbind(sites80, sites90, sites00, sites10)
  
  sites <- sites %>%
    rename(GJOIN = GISJOIN) %>%
    distinct()
  
for(i in seq(1980, 2010, 10)){
  temp1<-cendat %>%
    select(ends_with(as.character(i))) %>%
    rename_with( ~ gsub(as.character(i),"",.x, fixed=TRUE)) %>%
    select(!NAME) %>%
    mutate(year = i)
  
  temp2<-left_join(sites, temp1, by=c("GJOIN", "year"))  
  
  temp3<-temp2 %>%
    select(!year) %>%
    mutate(across(AV0AA:B37AB, ~.x*share),
           year = i) %>%
    select(!GJOIN) 
  
  temp4<-aggregate(.~site_id+year, temp3, FUN=sum ) 
  
  temp4<-temp4%>%
    filter(AV0AA>0)%>%
    mutate(pop = AV0AA,
           perwht = B18AA/pop,
           perblk = B18AB/pop,
           peroth = ifelse(i <= 1999 , (B18AC+B18AD)/pop, (B18AC+B18AD+B18AE)/pop),
           permin = 1 - perwht,
           peru18 = (B57AA+B57AB+B57AC+B57AD)/pop,
           pero65 = (B57AP+B57AQ+B57AR)/pop,
           perocc = B37AA/pop,
           perrnt = B37AB/pop,
           #perurb = A57AA/pop,
           #perrrl = A57AD/pop,
           pernhs = B69AA/pop,
           perhsd = B69AB/pop,
           percld = B69AC/pop,
           per25k = (B70AA+B70AB+B70AC+B70AD)/pop,
           per50k = (B70AE+B70AF+B70AG+B70AH)/pop,
           per50p = (B70AI+B70AJ)/pop,
           lnminc   = log(B79AA),
           lnminc2 = asinh(B79AA)) %>%
    select(site_id, year, pop, starts_with("per"), lnminc, lnminc2) %>%
    filter(!is.na(pop))
  
    ifelse(i==1980, cendat2<-temp4, cendat2<-rbind(cendat2, temp4))
    rm(temp1, temp2, temp3, temp4)
}
  
cendat2$perwht[which(cendat2$perwht>1)]<-1.0  #fixed 5 rounding errors
cendat2$permin[which(cendat2$permin<0)]<-0.0  #fixed 5 rounding errors


save(cendat2, file="./Data/Census.RData")

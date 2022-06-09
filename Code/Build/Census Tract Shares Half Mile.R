#Modified Leili and Anna Paper Script[Original on OneDrive]
#Faster Census Tract intersect to creates shares for buffers and weights
#Updated: June 9, 2022

rm(list=ls())

library(tidyverse)
library(sf)
library(readxl)

core<-read.csv("./Data/paper1_data.csv")

core2<-core %>%
  mutate(Start_YR=format(as.Date(final_date), "%Y"),
         year = round(as.numeric(Start_YR) / 10) * 10,
         LONGITUDE = as.numeric(LONGITUDE),
         LATITUDE = as.numeric(LATITUDE)) %>%
  select(site_id, LATITUDE, LONGITUDE, Start_YR, year) 

sites<-st_as_sf(core2, coords=c("LONGITUDE","LATITUDE"), crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
#sites2<-st_buffer(sites, dist = 1609.34) 
sites2<-st_buffer(sites, dist = 804.972) 

sites2$area<-st_area(sites2)

#Pulls out the 2000 start dates to link to 2000 census tracts####

tract<-st_read("./Data/2000/US_tract_2000_tl10.shp")
tr<-st_transform(tract, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
tr<-st_make_valid(tr)

sp<-sites2 %>%
  filter(year==2000)

sp2<-st_intersects(sp, tr)

for(i in seq(1,length(sp2))){
  
  temp <- slice(tr,sp2[[i]])
  temp2<-st_intersection(sp[i,], temp)
  temp2<-temp2 %>%
    mutate(aa = st_area(temp2),
           share = as.numeric(aa/Shape_Area),
           #weight = as.numeric(aa/((1609.34^2)*pi)),
           weight = as.numeric(aa/((804.672^2)*pi)),
           weight = case_when(weight > 1.0 ~ 1.0,
                              TRUE ~ weight)) %>%
    select(GISJOIN, site_id, share, weight, year) %>%
    distinct()
  
  ifelse(i==1, sp3<-temp2, sp3<-rbind(sp3,temp2))

}

sites00<-as.data.frame(sp3)
sites00$geometry<-NULL

#Pulls out the 1990 start dates to link to 1990 census tracts#### 

tract90<-st_read("./Data/1990/US_tract_1990_conflated.shp")
  tr<-st_transform(tract90, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  tr<-st_make_valid(tr)
  
  sp<-sites2 %>%
    filter(year==1990)
  
  sp2<-st_intersects(sp, tr)
  
  for(i in seq(1,length(sp2))){
    
    temp <- slice(tr,sp2[[i]])
    temp2<-st_intersection(sp[i,], temp)
    temp2<-temp2 %>%
      mutate(aa = st_area(temp2),
             share = as.numeric(aa/Shape_Area),
             #weight = as.numeric(aa/((1609.34^2)*pi)),
             weight = as.numeric(aa/((804.672^2)*pi)),
             weight = case_when(weight > 1.0 ~ 1.0,
                                TRUE ~ weight)) %>%
      select(GISJOIN, site_id, share, weight, year) %>%
      distinct()
    
    ifelse(i==1, sp3<-temp2, sp3<-rbind(sp3,temp2))
    
  }
  
  sites90<-as.data.frame(sp3)
  sites90$geometry<-NULL
  
#Pulls out the 1980 start dates to link to 1980 census tracts####  
  
  tract80<-st_read("./Data/1980/US_tract_1980_conflated.shp")
  tr<-st_transform(tract80, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  tr<-st_make_valid(tr)
  
  sp<-sites2 %>%
    filter(year==1980,
           !row_number() %in% c(34, 112))  #Two sites have no tracts within 1/2 mile
  
  sp2<-st_intersects(sp, tr)
  
  for(i in seq(1,length(sp2))){
    
    temp <- slice(tr,sp2[[i]])
    temp2<-st_intersection(sp[i,], temp)
    temp2<-temp2 %>%
      mutate(aa = st_area(temp2),
             share = as.numeric(aa/Shape_Area),
             #weight = as.numeric(aa/((1609.34^2)*pi)),
             weight = as.numeric(aa/((804.672^2)*pi)),
             weight = case_when(weight > 1.0 ~ 1.0,
                                TRUE ~ weight)) %>%
      select(GISJOIN, site_id, share, weight, year) %>%
      distinct()
    
    ifelse(i==1, sp3<-temp2, sp3<-rbind(sp3,temp2))
    
  }
  
  sites80<-as.data.frame(sp3)
  sites80$geometry<-NULL
  
#Pulls out the 2010 start dates to link to 2010 census tracts####  
  
  tract10<-st_read("./Data/2010/US_tract_2010.shp")
  tr<-st_transform(tract10, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  tr<-st_make_valid(tr)
  
  sp<-sites2 %>%
    filter(year==2010)
  
  sp2<-st_intersects(sp, tr)
  
  for(i in seq(1,length(sp2))){
    
    temp <- slice(tr,sp2[[i]])
    temp2<-st_intersection(sp[i,], temp)
    temp2<-temp2 %>%
      mutate(aa = st_area(temp2),
             share = as.numeric(aa/Shape_area),
             #weight = as.numeric(aa/((1609.34^2)*pi)),
             weight = as.numeric(aa/((804.672^2)*pi)),
             weight = case_when(weight > 1.0 ~ 1.0,
                                TRUE ~ weight)) %>%
      select(GISJOIN, site_id, share, weight, year) %>%
      distinct()
    
    ifelse(i==1, sp3<-temp2, sp3<-rbind(sp3,temp2))
    
  }
  
  sites10<-as.data.frame(sp3)
  sites10$geometry<-NULL
  
#Saves Data for Future Use####  
  
  save(sites80, sites90, sites00, sites10, file="./Data/sites05.RData")
  
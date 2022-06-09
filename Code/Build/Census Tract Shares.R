#Modified Leili and Anna Paper Script[Original on OneDrive]
#Census API Download to check validity of census data


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
  sites2<-st_buffer(sites, dist = 1609.34) 
  sites2$area<-st_area(sites2)

#Pulls out the 2000 start dates to link to 2000 census tracts####
  
tract<-st_read("./Data/2000/US_tract_2000_tl10.shp")
  tr<-st_transform(tract, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  tr<-st_make_valid(tr)

sp<-sites2 %>%
  filter(year==2000)

sites3 <- st_join(tr, sp, join = st_intersects, left=FALSE) #Joins the buffers with the census tract maps
sites4<-st_intersection(sp, st_difference(sites3)) #Intersects with the smaller set of tracts to get shares

sites5 <- sites4 %>%
  mutate(aa = st_area(sites4),
         share = as.numeric(aa/Shape_Area),
         weight = as.numeric(aa/((1609.34^2)*pi)),
         weight = case_when(weight > 1.0 ~ 1.0,
                            TRUE ~ weight))%>%
  select(GISJOIN, site_id, share, weight, year) %>%
  distinct()

sites00<-as.data.frame(sites5)
  sites00$geometry<-NULL
  
#Pulls out the 1990 start dates to link to 1990 census tracts#### 
  
tract90<-st_read("./Data/1990/US_tract_1990_conflated.shp")
  tr90<-st_transform(tract90, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  tr90<- tr90 %>%
    select(GISJOIN, Shape_Area, geometry)
  rm(tract90)
  
  tr90<-st_make_valid(tr90)
  
  sp<-sites2 %>%
    filter(year==1990)
  
  sites3 <- st_join(tr90, sp, join = st_intersects, left=FALSE) #Joins the buffers with the census tract maps
    rm(tr90)
  
  sites4<-st_intersection(sp, st_difference(sites3)) #Intersects with the smaller set of tracts to get shares
  
  sites5 <- sites4 %>%
    mutate(aa = st_area(sites4),
           share = as.numeric(aa/Shape_Area),
           weight = as.numeric(aa/((1609.34^2)*pi)),
           weight = case_when(weight > 1.0 ~ 1.0,
                              TRUE ~ weight))%>%
    select(GISJOIN, site_id, share, weight, year) %>%
    distinct()
  
  sites90<-as.data.frame(sites5)
  sites90$geometry<-NULL

#Pulls out the 1980 start dates to link to 1980 census tracts####  
  
  tract80<-st_read("./Data/1980/US_tract_1980_conflated.shp")
  tr80<-st_transform(tract80, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  tr80<-st_make_valid(tr80)
  
  sp<-sites2 %>%
    filter(year==1980)
  
  sites3 <- st_join(tr80, sp, join = st_intersects, left=FALSE) #Joins the buffers with the census tract maps
  
  sites4<-st_intersection(sp, sites3) #Intersects with the smaller set of tracts to get shares
  
  sites5 <- sites4 %>%
    mutate(aa = st_area(sites4),
           share = as.numeric(aa/Shape_Area),
           weight = as.numeric(aa/((1609.34^2)*pi)),
           weight = case_when(weight > 1.0 ~ 1.0,
                              TRUE ~ weight))%>%
    select(GISJOIN, site_id, share, weight, year) %>%
    distinct()
  
  sites80<-as.data.frame(sites5)
  sites80$geometry<-NULL

#Pulls out the 2010 start dates to link to 2010 census tracts####  
 
  tract10<-st_read("./Data/2010/US_tract_2010.shp")
  tr10<-st_transform(tract10, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  tr10<-st_make_valid(tr10)
  
  sp<-sites2 %>%
    filter(year==2010)
  
  sites3 <- st_join(tr10, sp, join = st_intersects, left=FALSE) #Joins the buffers with the census tract maps
  
  sites4<-st_intersection(sp, st_difference(sites3)) #Intersects with the smaller set of tracts to get shares
  
  sites5 <- sites4 %>%
    mutate(aa = st_area(sites4),
           share = as.numeric(aa/Shape_Area),
           weight = as.numeric(aa/((1609.34^2)*pi)),
           weight = case_when(weight > 1.0 ~ 1.0,
                              TRUE ~ weight))%>%
    select(GISJOIN, site_id, share, weight, year) %>%
    distinct()
  
  sites10<-as.data.frame(sites5)
  sites10$geometry<-NULL

#Saves Data for Future Use####  

save(sites80, sites90, sites00, sites10, file="./Data/sites.RData")

#Modified Leili and Anna Paper Script[Original on OneDrive]
#Census API Download to check validity of census data


rm(list=ls())

library(tidycensus)
library(tidyverse)
library(sf)
library(readxl)

core<-read.csv("./Data/paper1_data.csv")

cendat<-read.csv("./Data/nhgis0012_ts_nominal_county.csv", as.is=TRUE)

temp80<-cendat %>%
  select(STATE, STATEFP, COUNTY, COUNTYFP, ends_with(c("1980"))) %>%
  filter(!is.na(COUNTYFP)) %>%
  mutate(st_code = str_pad(as.character(STATEFP), 3, pad="0", side="left"),
         cn_code = str_pad(as.character(COUNTYFP), 3, pad="0",  side="left"))
  

c<-core %>%
  mutate(Start_YR=format(as.Date(core$final_date), "%Y"),
         Cen_YR = round(as.numeric(Start_YR) / 10) * 10) %>%
  select(site_id, year_x, COUNTY, STATE, LATITUDE, LONGITUDE, final_date,
         Start_YR, Cen_YR)


fips<-read_excel("~/Data/Census FIPS Codes.xlsx")

#Correct County Errors#####
cendat<-c %>%
  mutate(Name = gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(COUNTY), perl=TRUE),
         Name = gsub(" Boroug", "", Name),
         Name = gsub(" Parish", "", Name),
         Name = gsub("St.", "St", Name),
         Name = gsub(" Pari", "", Name),
         Name = gsub("'S", "s", Name),
         Name = gsub("Stafford", "Strafford", Name),
         Name = gsub("Starns", "Stearns", Name),
         Name = gsub("Stllwater", "Stillwater", Name),
         Name = gsub("Stnislaus", "Stanislaus", Name),
         Name = gsub("Strk", "Stark", Name),
         Name = gsub("Stvens", "Stevens", Name),
         Name = gsub("Lewis And Clark", "Lewis and Clark", Name),
         Name = gsub("Fairbanks North", "Fairbanks North Star", Name),
         Name = gsub("Aiken, Barnwell,", "Aiken", Name),
         Name = gsub("Aleutians West C", "Aleutians West", Name),
         Name = gsub("Anderson, Roane", "Anderson", Name),
         Name = gsub("Butte,Clark,Jeff", "Butte", Name),
         Name = gsub("Cascade, Judith", "Judith Basin", Name),
         Name = gsub("Davis, Weber", "Weber", Name),
         Name = gsub("Dupage", "Du Page", Name),
         Name = gsub("East Feliciana P", "East Feliciana", Name),
         Name = gsub("Livingstons", "Livingston", Name),
         Name = gsub("Mcclain", "McClain", Name),
         Name = gsub("Mccormick", "McCormick", Name),
         Name = gsub("Mccracken", "McCracken", Name),
         Name = gsub("Mchenry", "McHenry", Name),
         Name = gsub("Mckinley", "McKinley", Name),
         Name = gsub("Mclean", "McLean", Name),
         Name = gsub("Newport News Cit", "Newport News City", Name),
         Name = gsub("Virginia Beach C", "Virginia Beach City", Name),
         Name = gsub("Meade, Penningto", "Meade", Name),
         Name = gsub("Outer Ketchikan", "Outer Ketchikan", Name),
         
         Name = gsub("Fond Du Lac", "Fond du Lac", Name),
         Name = gsub("Greene, Montgome", "Greene", Name),
         Name = gsub("Hamilton, Butler", "Hamilton", Name),
         Name = gsub("Kings, Queens", "Kings", Name),
         Name = gsub("Morgan, Limeston", "Morgan", Name),
         Name = gsub("Ocean/Monmouth", "Ocean", Name),
         
         Name = gsub("Power, Bannock", "Power", Name),
         Name = gsub("Worcester, Middl", "Worcester", Name),
         Name = gsub("District Of Colu", "District of Columbia", Name)) %>%
  rename(State = STATE)

#Get FIPS Codes####

cendat<-merge(cendat, fips, by=c("Name", "State"), all.x=T)

temp<-cendat %>%
  select(Cen_YR, FIPS) %>%
  arrange(Cen_YR, FIPS) %>%
  distinct() %>%
  mutate(state = substr(FIPS,1,2),
         county = substr(FIPS,3,5))


#Modified Leili and Anna Paper Script[Original on OneDrive]
#Census API Download to check validity of census data


rm(list=ls())

library(tidycensus)
library(tidyverse)
library(sf)
library(readxl)

core<-read.csv("./Data/paper1_data.csv")

cendat<-read.csv("./Data/nhgis0013_ts_nominal_county.csv", as.is=TRUE)
  cendat<-cendat %>%
    rename_with( ~gsub("125", "2010", .x, fixed=TRUE)) %>%
    select(!ends_with("M"),
           !ends_with("195"))

for(i in seq(1980,2010,10)){
  
temp<-cendat %>%
  select(STATE, STATEFP, COUNTY, COUNTYFP, ends_with(as.character(i))) %>%
  filter(!is.na(COUNTYFP)) %>%
  rename_with( ~ gsub(as.character(i),"",.x, fixed=TRUE)) %>%
  mutate(st_code = str_pad(as.character(STATEFP), 3, pad="0", side="left"),
         cn_code = str_pad(as.character(COUNTYFP), 3, pad="0",  side="left"),
         pop = A00AA,
         perwht = B18AA/A00AA,
         perblk = B18AB/A00AA,
         peroth = ifelse(i <= 1999 , (B18AC+B18AD)/A00AA, (B18AC+B18AD+B18AE)/A00AA),
         peru18 = (B57AA+B57AB+B57AC+B57AD)/A00AA,
         pero65 = (B57AP+B57AQ+B57AR)/A00AA,
         perocc = B37AA/A00AA,
         perrnt = B37AB/A00AA,
         perurb = A57AA/A00AA,
         perrrl = A57AD/A00AA,
         pernhs = B69AA/A00AA,
         perhsd = B69AB/A00AA,
         percld = B69AC/A00AA,
         per25k = (B70AA+B70AB+B70AC+B70AD)/A00AA,
         per50k = (B70AE+B70AF+B70AG+B70AH)/A00AA,
         per50p = (B70AI+B70AJ)/A00AA,
         minc   = (B79AA),
         year = as.numeric(i)) %>%
  select(st_code, cn_code, year, pop, starts_with("per"), minc)%>%
  mutate(join = paste0(st_code, cn_code, year))%>%
  filter(!is.na(pop))

  ifelse(i==1980, cendat2<-temp, cendat2<-rbind(cendat2, temp))
}  
  

core2<-core %>%
  mutate(Start_YR=format(as.Date(core$final_date), "%Y"),
         year = round(as.numeric(Start_YR) / 10) * 10) %>%
  select(site_id, year_x, COUNTY, STATE, LATITUDE, LONGITUDE, final_date,
         Start_YR, year)

fips<-read_excel("~/Data/Census FIPS Codes.xlsx")

#Correct County Errors#####
core2<-core2 %>%
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

core2<-merge(core2, fips, by=c("Name", "State"), all.x=T)

core2<-core2 %>%
  distinct() %>%
  mutate(st_code = str_pad(substr(FIPS,1,2), 3, pad="0", side="left"),
         cn_code = str_pad(substr(FIPS,3,5), 3, pad="0", side="left"),
         join = as.character(paste0(st_code,cn_code,year)),
         join = replace(join, join == "0022012010", "0021302010"),
         join = replace(join, join == "0120861980", "0122051980"),
         join = replace(join, join == "0120861990", "0122051990"))

core2<-merge(core2, cendat2, by="join", all.x=TRUE)

core2 <- core2 %>%
  filter(!is.na(pop)) %>%
  select(site_id, Start_YR, pop, starts_with("per"), minc)

save(core2, file="./Data/core2.RData")



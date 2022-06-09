#Modified Leili and Anna Paper Script[Original on OneDrive]

rm(list=ls())

options(scipen=999)

library(survival)
library(tidyverse)
library(coxme)
library(survminer)
library(stargazer)


coxme_table <- function (mod){
  beta <- mod$coefficients
  var<-names(beta)
  e.beta <- exp(beta)
  nvar <- length(beta)
  nfrail <- nrow(mod$var) - nvar
  se <- sqrt(diag(mod$var)[nfrail + 1:nvar])
  z<- round(beta/se, 2)
  p<- round(signif(1 - pchisq((beta/se)^2, 1), 2), 4)
  table=data.frame(cbind(var,beta,e.beta,se,z,p))
  return(table)
}


core<-read.csv("./Data/paper1_data.csv")
load(file="./Data/Census.RData")

core1<-core %>%
  mutate(unemp = (f_maleunem + f_femunem)/2) %>%
  select(!starts_with("f_")) %>%
  rename(year = final_date_year) %>%
  mutate(CAG = case_when(CAG == 1 ~ 1,
                         CAG == "CAG" ~ 1,
                         CAG == 0 ~ 0,
                         TRUE ~ 0),
          party.pres = case_when(president == "reagan1" ~ 0,
                           president == "reagan1" ~ 0,
                           president == "bush_father" ~ 0,
                           president == "bush_son1" ~ 0,
                           president == "bush_son2" ~ 0,
                           TRUE ~ 1),
         Start_YR = year,
         year = case_when(Start_YR <1986 ~ 1980,
                          Start_YR > 1984 & Start_YR < 1996 ~ 1990,
                          Start_YR > 1994 & Start_YR < 2006 ~ 2000,
                          Start_YR > 2004 ~ 2010,
                          TRUE ~ 0),
         REGION = factor(REGION),
         FEDERAL = factor(FEDERAL),
         q_vote = case_when(vote <= 50.90 ~ "vote_1",
                            vote > 50.90 & vote <= 54.80 ~ "vote_2",
                            vote > 54.80 & vote <= 58.10 ~ "vote_3",
                            vote > 58.10 ~ "vote_4",
                            TRUE ~ "other2"),
         q_vote = factor(q_vote, levels = c("vote_1","vote_2","vote_3","vote_4")),
         q_sites = case_when(SITE_SCORE <= 37.93 ~ "v_low_haz",
                             SITE_SCORE >37.93 & SITE_SCORE <=41.00 ~ "low_haz",
                             SITE_SCORE > 41.00 & SITE_SCORE <= 50.00 ~ "hg_haz",
                             SITE_SCORE > 50.00 ~ "v_hg_haz",
                             TRUE ~ "other3"),
         q_sites = factor(q_sites, levels=c("v_low_haz","low_haz","hg_haz", "v_hg_haz")),
         vote = vote/100,
         npv = npv/100000,
         npv2 = asinh(npv),
         year.end = as.numeric(substr(completion_date, 1, 4))) %>%
  filter(unemp < 0.30,
         unemp > 0.001,
         duration > 0) %>%
  select(site_id, year, duration, event, Start_YR, REGION, FEDERAL, CAG, party, unemp, vote, 
         npv2, npv, q_vote, q_sites, vote, SITE_SCORE, party.pres, year.end) %>%
  distinct()

core2<-merge(core1, cendat2, by=c("site_id", "year"))
save(core2, file="./Analysis/Output/core.RData")

### Table One ####

sum.stat<-core2 %>%
  mutate(federal = case_when(FEDERAL == "F" ~ 1,
                             FEDERAL == "G" ~ 0,
                             TRUE ~ 0),
         v_low_haz = case_when(q_sites == "v_low_haz" ~ 1,
                               TRUE ~0),
         low_haz = case_when(q_sites == "low_haz" ~ 1,
                               TRUE ~0),
         hg_haz = case_when(q_sites == "hg_haz" ~ 1,
                               TRUE ~0),
         v_hg_haz = case_when(q_sites == "v_hg_haz" ~ 1,
                               TRUE ~0),
         region1 = case_when(REGION == 1 ~ 1,
                             TRUE ~ 0),
         region2 = case_when(REGION == 2 ~ 1,
                             TRUE ~ 0),
         region3 = case_when(REGION == 3 ~ 1,
                             TRUE ~ 0),
         region4 = case_when(REGION == 4 ~ 1,
                             TRUE ~ 0),
         region5 = case_when(REGION == 5 ~ 1,
                             TRUE ~ 0),
         region6 = case_when(REGION == 6 ~ 1,
                             TRUE ~ 0),
         region7 = case_when(REGION == 7 ~ 1,
                             TRUE ~ 0),
         region8 = case_when(REGION == 8 ~ 1,
                             TRUE ~ 0),
         region9 = case_when(REGION == 9 ~ 1,
                    TRUE ~ 0),
         region10 = case_when(REGION == 10 ~ 1,
                    TRUE ~ 0)) %>%
  select(event, duration, npv, npv2, SITE_SCORE, federal, CAG, pop, permin, pero65, unemp, pernhs, perhsd,
         percld, minc,  lnminc2, vote, party, perocc, starts_with("region"), Start_YR) %>%
  select(!REGION)

  sum.m<-signif(sapply(sum.stat, mean), 4)
  sum.st<-signif(sapply(sum.stat, sd), 4)
  sum.min<-signif(sapply(sum.stat, min), 4)
  sum.max<-signif(sapply(sum.stat, max), 4)
  
  descrip.stat=data.frame(cbind(sum.m, sum.st, sum.min, sum.max))
  
  #Additional Data for Discussion on Table One
  
  clean80<-sum.stat %>%
    filter(Start_YR < 1990)
  
  clean90<-sum.stat %>%
    filter(Start_YR > 1989,
           Start_YR < 2000)
  
  clean00<-sum.stat %>%
    filter(Start_YR > 1999,
           Start_YR < 2010)
  
  clean10<-sum.stat %>%
    filter(Start_YR > 2009)

#Regression Models####

mod1a <- coxph(Surv(duration, event)~permin+pero65+unemp+perhsd+percld+lnminc2+perocc, data=core2)

mod1b <- coxme(Surv(duration, event)~permin+pero65+unemp+perhsd+percld+lnminc2+perocc+(1|Start_YR), data=core2)

mod2a <- coxph(Surv(duration, event)~permin+pero65+unemp+perhsd+percld+lnminc2+perocc+
                 vote+CAG+npv2+SITE_SCORE, data=core2)

mod2b <- coxme(Surv(duration, event)~permin+pero65+unemp+perhsd+percld+lnminc2+perocc+
                 vote+CAG+npv2+SITE_SCORE+(1|Start_YR), data=core2)

mod2c <- coxph(Surv(duration, event)~permin+pero65+unemp+perhsd+percld+lnminc2+perocc+
                 vote+CAG+npv2+q_sites, data=core2)

mod2d <- coxme(Surv(duration, event)~permin+pero65+unemp+perhsd+percld+lnminc2+perocc+
                 vote+CAG+npv2+q_sites+(1|Start_YR), data=core2)


mod3a <- coxph(Surv(duration, event)~permin+pero65+unemp+perhsd+percld+lnminc2+perocc+
                 vote+CAG+npv2+q_sites+party+FEDERAL+REGION, data=core2)
mod3b <- coxme(Surv(duration, event)~permin+pero65+unemp+perhsd+percld+lnminc2+perocc+
                 vote+CAG+npv2+q_sites+party+FEDERAL+REGION+(1|Start_YR), data=core2)

mod4a <- coxph(Surv(duration, event)~permin+pero65+unemp+perhsd+percld+lnminc2+perocc+
                 vote+CAG+npv2+q_sites+party+FEDERAL+REGION+
                 permin*vote+permin*party, data=core2)
mod4b <- coxme(Surv(duration, event)~permin+pero65+unemp+perhsd+percld+lnminc2+perocc+
                 vote+CAG+npv2+q_sites+party+FEDERAL+REGION+
                 permin*vote+permin*party+(1|Start_YR), data=core2)

mod5a <- coxph(Surv(duration, event)~permin+pero65+unemp+perhsd+percld+lnminc2+perocc+
                 vote+CAG+npv2+q_sites+party+FEDERAL+REGION+
                 permin*CAG+perhsd*party+percld*party+perhsd*CAG+percld*CAG, data=core2)
mod5b <- coxme(Surv(duration, event)~permin+pero65+unemp+perhsd+percld+lnminc2+perocc+
                 vote+CAG+npv2+q_sites+party+FEDERAL+REGION+
                 permin*CAG+perhsd*party+percld*party+perhsd*CAG+percld*CAG+(1|Start_YR), data=core2)

mod6a <- coxph(Surv(duration, event)~permin+pero65+unemp+perhsd+percld+lnminc2+perocc+
                 vote+CAG+npv2+q_sites+party+FEDERAL+REGION+
                 q_sites*CAG, data=core2)
mod6b <- coxme(Surv(duration, event)~permin+pero65+unemp+perhsd+percld+lnminc2+perocc+
                 vote+CAG+npv2+q_sites+party+FEDERAL+REGION+
                 q_sites*CAG+(1|Start_YR), data=core2)
               
            
mod1b.o<-coxme_table(mod1b)
mod2b.o<-coxme_table(mod2b)
mod2d.o<-coxme_table(mod2d)
mod3b.o<-coxme_table(mod3b)
mod4b.o<-coxme_table(mod4b)
mod5b.o<-coxme_table(mod5b)
mod6b.o<-coxme_table(mod6b)

#Code to join all coxme tables for output
#put all data frames into list
df_list <- list(mod1b.o, mod2b.o, mod2d.o, mod3b.o, mod4b.o, mod5b.o, mod6b.o)

#merge all data frames in list
coxme.out <- df_list %>% reduce(full_join, by='var')
  write.csv(coxme.out, file="./Analysis/Output/ME_Tables_robust1.csv")
stargazer(mod1a, mod2a, mod2c, mod3a, mod4a, mod5a, mod6a,
          type="html",
          out = "./Analysis/Output/PH_Table_robust1.html")

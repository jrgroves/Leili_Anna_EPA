#Modified Leili and Anna Paper Script[Original on OneDrive]

rm(list=ls())

library(survival)
library(tidyverse)
library(coxme)
library(survminer)
library(stargazer)

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
         year = round(as.numeric(Start_YR) / 10) * 10,
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
         vote = vote/100) %>%
  filter(unemp < 0.30) %>%
  select(site_id, year, duration, event, Start_YR, REGION, FEDERAL, CAG, party, unemp, vote, 
         lognpv, q_vote, q_sites, vote, SITE_SCORE, party.pres) %>%
  distinct()

core2<-merge(core1, cendat2, by=c("site_id", "year"))
save(core2, file="./Analysis/Output/core.RData")

#Models

mod1a <- coxph(Surv(duration, event)~permin+unemp+peru18+pero65+perhsd+percld+lnminc2+
                 perocc+perrnt, data=core2)
mod1b <- coxme(Surv(duration, event)~permin+unemp+peru18+pero65+perhsd+percld+lnminc2+
                 perocc+perrnt+(1|Start_YR), data=core2)

mod2a <- coxph(Surv(duration, event)~permin+unemp+peru18+pero65+perhsd+percld+lnminc2+
                 perocc+perrnt+vote+CAG+lognpv+q_sites, data=core2)
mod2b <- coxme(Surv(duration, event)~permin+unemp+peru18+pero65+perhsd+percld+lnminc2+
                 perocc+perrnt+vote+CAG+lognpv+q_sites+(1|Start_YR), data=core2)

mod3a <- coxph(Surv(duration, event)~permin+unemp+peru18+pero65+perhsd+percld+lnminc2+
                 perocc+perrnt+q_vote+CAG+lognpv+q_sites+party+FEDERAL+REGION, data=core2)
mod3b <- coxme(Surv(duration, event)~permin+unemp+peru18+pero65+perhsd+percld+lnminc2+
                 perocc+perrnt+q_vote+CAG+lognpv+q_sites+party+FEDERAL+REGION+(1|Start_YR), data=core2)

mod4a <- coxph(Surv(duration, event)~permin+unemp+peru18+pero65+perhsd+percld+lnminc2+
               perocc+perrnt+q_vote+CAG+lognpv+q_sites+party+FEDERAL+REGION+permin*q_vote+permin*party,
               data=core2)
mod4b <- coxme(Surv(duration, event)~permin+unemp+peru18+pero65+perhsd+percld+lnminc2+
                 perocc+perrnt+q_vote+CAG+lognpv+q_sites+party+FEDERAL+REGION+permin*q_vote+permin*party+
                 (1|Start_YR), data=core2)

mod5a <- coxph(Surv(duration, event)~permin+unemp+peru18+pero65+perhsd+percld+lnminc2+
                 perocc+perrnt+vote+CAG+lognpv+q_sites+party+FEDERAL+REGION+permin*CAG+
                 perhsd*party+percld*party+perhsd*CAG+percld*CAG,
               data=core2)
mod5b <- coxme(Surv(duration, event)~permin+unemp+peru18+pero65+perhsd+percld+lnminc2+
                 perocc+perrnt+vote+CAG+lognpv+q_sites+party+FEDERAL+REGION+permin*CAG+
                 perhsd*party+percld*party+perhsd*CAG+percld*CAG+(1|Start_YR), data=core2)

mod6a <- coxph(Surv(duration, event)~permin+unemp+peru18+pero65+perhsd+percld+lnminc2+
                 perocc+perrnt+q_vote+CAG+lognpv+q_sites+party+FEDERAL+REGION+q_sites*CAG
                 ,
               data=core2)
mod6b <- coxme(Surv(duration, event)~permin+unemp+peru18+pero65+perhsd+percld+lnminc2+
                 perocc+perrnt+q_vote+CAG+lognpv+q_sites+party+FEDERAL+REGION+q_sites*CAG+
                 (1|Start_YR), data=core2)

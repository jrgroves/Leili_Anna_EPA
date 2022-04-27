#Modified Leili and Anna Paper Script[Original on OneDrive]

rm(list=ls())

library(survival)
library(tidyverse)
library(coxme)
library(survminer)
library(stargazer)

core<-read.csv("./Data/paper1_data.csv")
load(file="./Data/core2.RData")

core<-core %>%
  mutate(unemp = f_maleunem + f_femunem) %>%
  select(!starts_with("f_")) %>%
  mutate(CAG = case_when(CAG == 1 ~ 1,
                         CAG == "CAG" ~ 1,
                         CAG == 0 ~ 0,
                         TRUE ~ 0)) %>%
  mutate(party = case_when(president == "reagan1" ~ 0,
                           president == "reagan1" ~ 0,
                           president == "bush_father" ~ 0,
                           president == "bush_son1" ~ 0,
                           president == "bush_son2" ~ 0,
                           TRUE ~ 1))


core<-merge(core, core2, by="site_id")

c<-core %>%
  mutate(Start_YR=format(as.Date(core$final_date), "%Y"),
         REGION = factor(REGION),
         FEDERAL = factor(FEDERAL),
         duration_w = duration/7,
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
         lnminc = log(minc),
         pop = pop/1000,
         vote = vote/100) %>%
  select(duration, event, Start_YR, REGION, FEDERAL, CAG, starts_with("per"), pop, minc, party,
         unemp, vote, lnminc, lognpv, q_vote, q_sites, vote)

stargazer(c, type="html", out="./Analysis/Output/sumstat.html")


#fit <- survfit(Surv(duration, event) ~ factor(q_income), data = c)
#ggsurvplot(fit, 
#           data=c, 
#           conf.int = FALSE,
#           conf.int.style = "step",
#           break.time.by = 2,
#           surv.median.line = "hv",
#            title = "",
#           xlab = "")


mod1.a<-coxph(Surv(duration,event)~perblk+peroth+pero65+unemp+perhsd+percld+
              perurb+lnminc+perocc, data=c)
mod1.b<-coxme(Surv(duration,event)~perblk+peroth+pero65+unemp+perhsd+percld+
              perurb+lnminc+perocc+(1|Start_YR), data=c)              

mod2.a<-coxph(Surv(duration,event)~perblk+peroth+pero65+unemp+perhsd+percld+
                perurb+lnminc+perocc+q_vote+CAG+lognpv+q_sites, data=c)
mod2.b<-coxme(Surv(duration,event)~perblk+peroth+pero65+unemp+perhsd+percld+
                perurb+lnminc+perocc+q_vote+CAG+lognpv+q_sites+(1|Start_YR), data=c)              


mod3.a<-coxph(Surv(duration,event)~perblk+peroth+pero65+peru18+unemp+perhsd+percld+
                perurb+lnminc+perocc+q_vote+CAG+lognpv+q_sites+
                REGION+FEDERAL+party+per25k+per50k+pop, data=c)
mod3.b<-coxme(Surv(duration,event)~perblk+peroth+pero65+peru18+unemp+perhsd+percld+
                perurb+lnminc+perocc+q_vote+CAG+lognpv+q_sites+
                REGION+FEDERAL+party+per25k+per50k+pop+(1|Start_YR), data=c)

mod4.a<-coxph(Surv(duration,event)~perblk+peroth+pero65+peru18+unemp+perhsd+percld+
                perurb+lnminc+perocc+q_vote+CAG+lognpv+q_sites+
                REGION+FEDERAL+party+per25k+per50k+pop+vote*perblk, data=c)
mod4.b<-coxme(Surv(duration,event)~perblk+peroth+pero65+peru18+unemp+perhsd+percld+
                perurb+lnminc+perocc+q_vote+CAG+lognpv+q_sites+
                REGION+FEDERAL+party+per25k+per50k+pop+vote*perblk+(1|Start_YR), data=c)

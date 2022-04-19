#Modified Leili and Anna Paper Script[Original on OneDrive]

rm(list=ls())

library(survival)
library(tidyverse)
library(coxme)
library(survminer)

core<-read.csv("./Data/paper1_data.csv")

c<-core %>%
  mutate(Start_YR=format(as.Date(core$final_date), "%Y"),
         REGION = factor(REGION),
         FEDERAL = factor(FEDERAL),
         CAG = factor(CAG),
         duration_w = duration/7,
         f_income = log(income),
         q_income = case_when(income < 18636  ~"first",
                              income >18636 & income <29216 ~"second",
                              income >29216 & income <37384 ~"thrid",
                              income > 37384 ~ "fourth",
                              TRUE ~ "other"),
         q_vote = case_when(vote <= 50.90 ~ "vote_f",
                            vote >50.90 & vote <=54.80 ~ "vote_s",
                            vote > 54.80 & vote<=58.10 ~ "vote_t",
                            vote >58.10 ~ "vote_fth",
                            TRUE ~ "other2"),
         q_sites = case_when(SITE_SCORE <= 37.93 ~ "v_low_haz",
                             SITE_SCORE >37.93 & SITE_SCORE <=41.00 ~ "low_haz",
                             SITE_SCORE > 41.00 & SITE_SCORE <= 50.00 ~ "hg_haz",
                             SITE_SCORE > 50.00 ~ "v_hg_haz",
                             TRUE ~ "other3")) %>%
  select(duration, event, Start_YR, REGION, FEDERAL, CAG, starts_with("f_"),
         f_income, q_income, lognpv, q_vote, q_sites)


fit <- survfit(Surv(duration, event) ~ factor(q_income), data = c)
ggsurvplot(fit, 
           data=c, 
           conf.int = FALSE,
           conf.int.style = "step",
           break.time.by = 2,
           surv.median.line = "hv",
           title = "",
           xlab = "")


mod1<-coxph(Surv(duration,event)~f_black+f_unemployment+f_educated+f_urban+f_pop65+
              f_owner+q_income+q_vote+lognpv+q_sites, data=c)
mod2<-coxme(Surv(duration,event)~f_black+f_unemployment+f_educated+f_urban+f_pop65+
              f_owner+q_income+q_vote+lognpv+(1|Start_YR), data=subset(c, q_sites=="low_haz"))

summary(mod1)
summary(mod2)

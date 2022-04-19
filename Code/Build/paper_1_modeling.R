#options(scipen = 999)

rm(list=ls())

library(ivprobit)
library(aod)
library(ggplot2)
library(survival)

paper1_data<-read.csv("./Data/paper1_data.csv")


attach(paper1_data)
time <- duration 
event <- event 
args(coxph)

# model 7 
coxph1_1 <- coxph(Surv(duration,event)~f_black+f_unemployment+f_educated+
                    +f_urban+log(income) +f_pop65 + f_owner
                  
                  ,data=paper1_data)
summary(coxph1_1) 

#model 8 


coxph1_1 <- coxph(Surv(duration,event)~f_black+f_unemployment+f_educated+
                    +f_urban+log(income) +f_pop65 + f_owner + vote+party+CAG+log(npv)+SITE_SCORE
                  
                  ,data=paper1_data)
summary(coxph1_1)

#model 9 
coxph72 <- coxph(Surv(duration,event)~f_black+f_unemployment+ f_educated
                 +f_urban+log(income)+f_pop65+f_owner+vote+party+CAG+log(npv)+
                   SITE_SCORE+federal_status+region1+ region2 +region3 +region4+ region5 +region6 +region7 +region8+
                   region9 +region10
                 
                 ,data=paper1_data)
summary(coxph72)

#model 10 
coxph72 <- coxph(Surv(duration,event)~f_black+f_unemployment+ f_educated
                 +f_urban+log(income)+f_pop65+f_owner+vote+party+CAG+log(npv)+
                   SITE_SCORE+federal_status+region1+ region2 +region3 +region4+ region5 +region6 +region7 +region8+
                   region9 +region10+f_black*vote+f_black*party
                 
                 ,data=paper1_data)
summary(coxph72)

#model 11 

coxph73 <- coxph(Surv(duration,event)~f_black+f_unemployment+ f_educated
                 +f_urban+log(income)+f_pop65+f_owner+vote+party+CAG+log(npv)+
                   SITE_SCORE+federal_status+region1+ region2 +region3 +region4+ region5 +region6 +region7 +region8+
                   region9 +region10+f_black*CAG+ f_educated*party+ f_educated*CAG
                 
                 ,data=paper1_data)
summary(coxph73)

#model 12 

paper1_data$minority=paper1_data$f_black+paper1_data$f_hispanic

# ---Also, with your summary statistics, you might also break the data down into four categories of minority status 
# (0-25, 26-50, 51-75, 76-100% minority tracts) and see if any of means differ between the groups (no test needed).


paper1_data$minority_cat = 'null'

paper1_data <- within(paper1_data,minority_cat[minority>=0 & minority< 0.25] <- 1)  #54% 
paper1_data <- within(paper1_data,minority_cat[minority>=0.25 & minority< 0.5] <- 2)  #15% 
paper1_data <- within(paper1_data,minority_cat[minority>=0.5 & minority< 0.75] <- 3)  # 8% 
paper1_data <- within(paper1_data,minority_cat[minority>=0.75 & minority< 1] <- 4)    # 4%
paper1_data <- within(paper1_data,minority_cat[minority>=1] <- 5)     # 19% 


paper1_data$minority_cat <- factor(paper1_data$minority_cat)
paper1_data$minority_cat <- relevel(paper1_data$minority_cat, 5 )


mod1<- coxph(Surv(duration,event)~minority_cat+f_unemployment+ f_educated
              +f_urban+log(income)+vote+CAG+log(npv)+federal_status+site_score+region1+ region2 +region3 
              +region4+ region5 +region6 +region7 +region8+
                region9 +region10+f_educated*party+party,data=paper1_data)
summary(mod1)

#model 13 
mod2<- coxph(Surv(duration,event)~minority_cat+f_unemployment+ f_educated
              +f_urban+log(income)+vote+CAG+log(npv)+federal_status+site_score+region1+ region2 +region3 
              +region4+ region5 +region6 +region7 +region8+
                region9 +region10+minority_cat*vote+party,data=paper1_data)
summary(mod2)

#model 14
mod3<- coxph(Surv(duration,event)~minority_cat+f_unemployment+ f_educated
              +f_urban+log(income)+vote+CAG+log(npv)+federal_status+site_score+region1+ region2 +region3 
              +region4+ region5 +region6 +region7 +region8+
                region9 +region10+minority_cat*party+party,data=paper1_data)
summary(mod3)

#model 9e 
#model 10e
#model 11e


######## half mile analysis 
#model 9h

coxph9 <- coxph(Surv(duration,event)~f_black+f_unemployment+ f_educated
                +f_urban+log(income)+vote+party+CAG+log(npv)+
                  SITE_SCORE+federal_status+region1+ region2 +region3 +region4+ region5 +region6 +region7 +region8+
                  region9 +region10
                
                ,data=paper1_data_halfmile)
summary(coxph9)


#model 10h 
coxph10 <- coxph(Surv(duration,event)~f_black+f_unemployment+ f_educated
                 +f_urban+log(income)+vote+party+CAG+log(npv)+
                   SITE_SCORE+federal_status+region1+ region2 +region3 +region4+ region5 +region6 +region7 +region8+
                   region9 +region10+f_black*vote+f_black*party
                 
                 ,data=paper1_data_halfmile)
summary(coxph10)


#model 11h 
coxph11 <- coxph(Surv(duration,event)~f_black+f_unemployment+ f_educated
                 +f_urban+log(income)+vote+party+CAG+log(npv)+
                   SITE_SCORE+federal_status+region1+ region2 +region3 +region4+ region5 +region6 +region7 +region8+
                   region9 +region10+f_black*CAG+ f_educated*party+ f_educated*CAG,
                 data=paper1_data_halfmile)
summary(coxph11)


  







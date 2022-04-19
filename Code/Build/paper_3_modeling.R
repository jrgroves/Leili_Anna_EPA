

## These are logistic regressions in Tables 2 and 3 

options(scipen = 999)
library(ivprobit)
library(aod)
library(ggplot2)
require(foreign)


paper3_data$REGION_x<-factor(paper3_data$REGION_x)

paper3_data$REGION_x<-relevel(paper3_data$REGION_x, '10')

paper3_data$minority=paper3_data$black_c+paper3_data$hispanic_c
paper3_data$educ=paper3_data$higherthan+paper3_data$graduate

# mm=glm(negotiation~minority+education_new+urban_c+unemployment+income_c+f_pop65+vote+party,data=paper3_data, family=binomial(link="logit"),x=TRUE)
# summary(mm)



## Table 2 


##with federal indicator 

# model 1 

mm=glm(negotiation_new~black_c+f_unemployment+education_new+urban_c+income_c+vote+party+grant_new
       +lognpv+SITE_SCORE_x+REGION_x+PARTY_NUMBER+federal_status_x,data=paper3_data, family=binomial(link="logit"),x=TRUE)
summary(mm)  ## AIC 495.79



##with nature of sites 

# model2 

mm=glm(negotiation_new~minority+f_unemployment+education_new+urban_c+income_c+vote+party+grant_new
       +lognpv+SITE_SCORE_x+REGION_x+PARTY_NUMBER+
         army+defense+navy+manufacturing+airforce+energy,data=paper3_data, family=binomial(link="logit"),x=TRUE)
summary(mm)   ### AIC 484.68


mm=glm(negotiation_new~minority+f_unemployment+education_new+urban_c+income_c+vote+party+grant_new
       +lognpv+SITE_SCORE_x+REGION_x+PARTY_NUMBER+federal_status_x+
         federal_status_x*army+defense+navy+manufacturing+airforce+energy,data=paper3_data, family=binomial(link="logit"),x=TRUE)
summary(mm)




##with minority as cat

#model 3 

paper3_data$minority_cat = 'null'

paper3_data <- within(paper3_data,minority_cat[minority>=0 & minority< 0.25] <- 1)  #54% 
paper3_data <- within(paper3_data,minority_cat[minority>=0.25 & minority< 0.5] <- 2)  #15% 
paper3_data <- within(paper3_data,minority_cat[minority>=0.5 & minority< 0.75] <- 3)  # 8% 
paper3_data <- within(paper3_data,minority_cat[minority>=0.75 & minority< 1] <- 4)    # 4%
paper3_data <- within(paper3_data,minority_cat[minority>=1] <- 5)     # 19% 


mm=glm(negotiation_new~minority_cat+f_unemployment+education_new+urban_c+income_c+vote+party+grant_new
       +lognpv+SITE_SCORE_x+REGION_x+PARTY_NUMBER+
         army+defense+navy+manufacturing+airforce+energy,data=paper3_data, family=binomial(link="logit"),x=TRUE)
summary(mm)  ###AIC 489.7

####################################################################################################################################
####################################################################################################################################
####################################################################################################################################

## Table 3 


##interaction between minority and education

# model 4 

mm_test1=glm(negotiation_new~minority+f_unemployment+education_new+urban_c+income_c+vote+party+grant_new
             +lognpv+SITE_SCORE_x+REGION_x+PARTY_NUMBER+
               army+defense+navy+manufacturing+airforce+energy+hispanic_c*education_new,data=paper3_data, family=binomial(link="logit"),x=TRUE)
summary(mm_test1)   ##AIC 486.78



#minority and EJ organization 
# model 5 

mm_test2=glm(negotiation_new~minority+f_unemployment+education_new+urban_c+income_c+vote+party+grant_new
             +lognpv+SITE_SCORE_x+REGION_x+PARTY_NUMBER+
               army+defense+navy+manufacturing+airforce+energy+minority*grant_new,data=paper3_data, family=binomial(link="logit"),x=TRUE)
summary(mm_test2)  ##AIC 486.47


#interaction between education and grant (EJ organization) 
# model 6 

typeof(paper3_data$grad_new)

mm_test3=glm(negotiation_new~minority+f_unemployment+education_new+urban_c+income_c+vote+party+grant_new
             +lognpv+SITE_SCORE_x+REGION_x+PARTY_NUMBER+
               army+defense+navy+manufacturing+airforce+energy+education_new*grant_new,data=paper3_data, family=binomial(link="logit"),x=TRUE)
summary(mm_test3)  ## AIC 486




#### Preamble ####
# Purpose: clean raw data to clean data for the paper
# Author: Anna Li
# Data: February 26, 2022
# Contact:annadl.li@mail.utoronto.ca

##workplace setup
library(tidyverse)
library(ggplot2)
library(here)
library(knitr)

library(janitor) # Helps clean datasets
library(lubridate) #helps with date cleaning
library(haven)
library(kableExtra)

##read in raw data
main_exp1<-read_dta("inputs/data/raw_data/01_main_exp_raw1.dta")
main_exp2<-read_dta("inputs/data/raw_data/01_main_exp_raw2.dta")
followup1<-read_dta("inputs/data/raw_data/02_follow_up_raw1.dta")
followup2<-read_dta("inputs/data/raw_data/02_follow_up_raw2.dta")


## clean main exp

#rename 
names(main_exp1)[names(main_exp1) == 'q45_1_p2'] <- 'labor_demand_guess'
#combine to get one labor demand guess column
main_exp1$labor_demand_guess<-ifelse(is.na(main_exp1$labor_demand_guess),main_exp1$q45_4_p2,main_exp1$labor_demand_guess)
main_exp1$labor_demand_guess<-ifelse(is.na(main_exp1$labor_demand_guess),main_exp1$q46_1_p2,main_exp1$labor_demand_guess)
main_exp1$labor_demand_guess<-ifelse(is.na(main_exp1$labor_demand_guess),main_exp1$q43_1_p2,main_exp1$labor_demand_guess)

#generate signup variable with wife's phone number
main_exp1 <- main_exp1 %>% mutate(
  signed_up_number = signed_up)
main_exp1$signed_up_number[which(main_exp1$no_wife_number ==1)]<-0

#moving no condition ppl to control and include
main_exp1 <- main_exp1 %>% mutate(
  condition2 = condition)
main_exp1$condition2[which(is.na(main_exp1$condition))]<-0

#replacing undesired ages
##generate new variables
main_exp1<- main_exp1 %>% mutate(married = (marital == 2))
main_exp1<- main_exp1 %>% mutate(secondary = as.integer((education >= 3)))

#change from absolute numbers to percentage
main_exp1<- main_exp1 %>% mutate(
  num_knows_per = num_know/30,
  num_mfs_per = num_mutual_friends/30)

#create tag for session (first occurrence of unique value)
main_exp1$stag<-main_exp1$session
main_exp1$stag[which(duplicated(main_exp1$stag))]<-0

#create wedges
#outside, semiseg, mwage
#guess
main_exp1<- main_exp1 %>% mutate(
  outside_guess = outside_others+outside_self, 
  semiseg_guess = semiseg_others+semiseg_self,  
  mwage_guess = mwage_others+mwage_self)
#objective, need to find true mean

session_count<-main_exp1 %>% group_by(session) %>% count()
#same value, just extracted for easier repetition
session_count<- c(30,34,29,30,29,30,29,30,27,29,32,29,28,30,29,27,28)     

outside_mean<-main_exp1 %>% group_by(session) %>% summarise(mean(outside_self))
semiseg_mean<-main_exp1 %>% group_by(session) %>% summarise(mean(semiseg_self))
mwage_mean<-main_exp1 %>% group_by(session) %>% summarise(mean(mwage_self))
means<-cbind(outside_mean[,2],semiseg_mean[,2],mwage_mean[,2])
means<-means*30
main_exp1<- main_exp1 %>% mutate(
  outside_objective = rep((means[,1]),session_count), 
  semiseg_objective =rep((means[,2]),session_count), 
  mwage_objective = rep((means[,3]),session_count))

#wedge, difference between guess and objective
main_exp1<- main_exp1 %>% mutate(
  outside_wedge = outside_guess-outside_objective, 
  semiseg_wedge = semiseg_guess-semiseg_objective, 
  mwage_wedge = mwage_guess-mwage_objective)

main_exp1<- main_exp1 %>% mutate(
  outside_wedge_pos = outside_wedge>0,
  interaction = condition2*outside_wedge_pos)
main_exp1<- main_exp1 %>% subset(select = -c(condition,marital,employed_ever,
                                             num_know,num_mutual_friends,
                                             q45_4_p2,q46_1_p2,q43_1_p2,
                                             count,condition_txt,glowork_choice,
                                             second_order_total,signed_up,no_wife_number))
##save cleaned results
write_csv(main_exp1,file = "inputs/data/main_exp_clean_paper.csv")



## clean followup

#clean duplicates
n<-nrow(main_exp2)
dup_index<-seq(1001,1000+n)
main_exp2<-cbind(main_exp2,dup_index)
for (i in 1:n) {
  if(main_exp2$session[i] == 1 & main_exp2$digits[i] ==136) {
    main_exp2$session[i] = main_exp2$dup_index[i]}
  if(main_exp2$session[i] == 5 & main_exp2$digits[i] ==430) {
    main_exp2$session[i] = dup_index[i]}
  if(main_exp2$session[i] == 6 & main_exp2$digits[i] ==651) {
    main_exp2$session[i] = main_exp2$dup_index[i]}
  if(main_exp2$session[i] == 6 & main_exp2$digits[i] ==922) {
    main_exp2$session[i] = main_exp2$dup_index[i]}
  if(main_exp2$session[i] == 7 & main_exp2$digits[i] ==646) {
    main_exp2$session[i] = main_exp2$dup_index[i]}
  if(main_exp2$session[i] == 9 & main_exp2$digits[i] ==16) {
    main_exp2$session[i] = main_exp2$dup_index[i]}
  if(main_exp2$session[i] == 9 & main_exp2$digits[i] ==449) {
    main_exp2$session[i] = main_exp2$dup_index[i]}
  if(main_exp2$session[i] == 12 & main_exp2$digits[i] ==282) {
    main_exp2$session[i] = main_exp2$dup_index[i]}
  if(main_exp2$session[i] == 13 & main_exp2$digits[i] ==880) {
    main_exp2$session[i] = main_exp2$dup_index[i]}
  if(main_exp2$session[i] == 15 & main_exp2$digits[i] ==84) {
    main_exp2$session[i] = main_exp2$dup_index[i]}
  if(main_exp2$session[i] == 17 & main_exp2$digits[i] ==341) {
    main_exp2$session[i] = main_exp2$dup_index[i]}
  if(main_exp2$session[i] == 17 & main_exp2$digits[i] ==966) {
    main_exp2$session[i] = main_exp2$dup_index[i]}
}

#initial merge
merge1<-inner_join(main_exp2,followup1, by=c('digits', 'session'),copy = TRUE)
merge2<-anti_join(main_exp2,followup1, by=c('digits', 'session'), copy = TRUE)
merged<-plyr::rbind.fill(merge1,merge2)
#generate matched indication
tot_match<-merge1$dup_index
merged<-merged %>% mutate(
  matched = rep(0,nrow(merged)))
merged$matched[which(tot_match %in% merged$dup_index)]<-1

#merge with followup2 dataset
merge1<-inner_join(merged,followup2, by=c('digits', 'session'))
merge2<-anti_join(merged,followup2, by=c('digits', 'session'))
followup<-plyr::rbind.fill(merge1,merge2)
#generate matched indication
tot_match<-merge1$dup_index
followup$matched[which(tot_match %in% followup$dup_index)]<-1

#new variables
followup$work_outside_1_fl<-ifelse(is.na(followup$work_outside_1_fl),followup$work_outside_1,followup$work_outside_1_fl)
followup$work_semiseg_1_fl<-ifelse(is.na(followup$work_semiseg_1_fl),
                                   followup$work_semiseg_1,followup$work_semiseg_1_fl)
followup<- followup %>% subset(select = -c(work_outside_1, work_semiseg_1))
#matched replace
followup$matched[which(followup$session>0 &is.na(followup$session))]<-0
followup$matched[which(is.na(followup$work_semiseg_1_fl))]<-0

#rename 
names(followup)[names(followup) == 'q45_1_p2'] <- 'labor_demand_guess'

#generate signup variable with wife's phone number
followup <- followup %>% mutate(
  signed_up_number = signed_up)
followup$signed_up_number[which(followup$no_wife_number ==1)]<-0

#moving no condition ppl to control and include
followup <- followup %>% mutate(
  condition2 = condition)
followup$condition2[which(is.na(followup$condition))]<-0

##generate new variables
followup<- followup %>% mutate(married = (marital == 2))
followup<- followup %>% mutate(secondary = as.integer((education >= 3)))

#change from absolute numbers to percentage
followup<- followup %>% mutate(
  num_knows_per = num_know/30,
  num_mfs_per = num_mutual_friends/30)

#create tag for session (first occurrence of unique value)
followup$stag<-followup$session
followup$stag[which(duplicated(followup$stag))]<-0

#create wedges
#outside, semiseg, mwage
#guess
followup<- followup %>% mutate(
  outside_guess = outside_others+outside_self, 
  semiseg_guess = semiseg_others+semiseg_self,  
  mwage_guess = mwage_others+mwage_self)
#objective, need to find true mean

session_count<-followup %>% group_by(session) %>% count()
#same value, just extracted for easier repetition
session_count<- c(28,34,29,30,27,26,27,30,23,29,32,27,26,30,27,27,24,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)  

outside_mean<-followup %>% group_by(session) %>% summarise(mean(outside_self))
semiseg_mean<-followup %>% group_by(session) %>% summarise(mean(semiseg_self))
mwage_mean<-followup %>% group_by(session) %>% summarise(mean(mwage_self))
means<-cbind(outside_mean[,2],semiseg_mean[,2],mwage_mean[,2])
means<-means*30
followup<- followup %>% mutate(
  outside_objective = rep((means[,1]),session_count), 
  semiseg_objective =rep((means[,2]),session_count), 
  mwage_objective = rep((means[,3]),session_count))

#wedge, difference between guess and objective
followup<- followup %>% mutate(
  outside_wedge = outside_guess-outside_objective, 
  semiseg_wedge = semiseg_guess-semiseg_objective, 
  mwage_wedge = mwage_guess-mwage_objective)

## more replacing 
followup$driving_fl[which(followup$driving_fl ==2)]<-0
followup$employed_3mos_out_fl[which(followup$employed_3mos_out_fl ==2)]<-0
followup$employed_3mos_fl[which(followup$employed_3mos_fl ==2)]<-0
followup$applied_out_fl[which(followup$applied_out_fl ==2)]<-0
followup$interviewed_out_fl[which(followup$interviewed_out_fl ==2)]<-0
followup$interview_sched_out_fl[which(followup$interview_sched_out_fl ==2)]<-0
followup$employed_now_out_fl[which(followup$employed_now_out_fl ==2)]<-0


# renaming
names(followup)[names(followup) == 'work_outside_1_fl'] <- 'outside_others_fl'
# new variables
followup<-followup %>%  mutate(
  outside_others_fl2 = outside_others_fl/30,
  outside_others2 = outside_others/30,
  min_wage_fl_per = min_wage_1_fl/30
)

#keep relevant variables
followup<-followup %>% select(haafez_self,vjobs_self,
                              applied_out_fl,driving_fl,interviewed_out_fl,employed_now_out_fl,
                              outside_objective,outside_guess,outside_others_fl,min_wage_fl_per,
                              children,outside_others_fl2,interview_sched_out_fl,matched,session,
                              outside_self, semiseg_self, mwage_self,
                              outside_others, semiseg_others, mwage_others,
                              employed_wife,employed_now,education,age,secondary,
                              num_knows_per,num_mfs_per,signed_up_number,outside_wedge,
                              condition2,outside_confidence,employed_3mos_out_fl)
##save cleaned results
write_csv(followup,file = "inputs/data/followup_clean_paper.csv")

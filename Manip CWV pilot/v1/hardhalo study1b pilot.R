---
title: "hardhalo study1b pilot-V1"
author: "Christine Nguyen "
Date: 
RStudio_Version:
---

setwd('C:/Users/c1/OneDrive/Desktop/Fall 2021/Hard halos F21/Manip CWV pilot/v1')

library(dplyr)
library(readxl)
library(psych)

# PREPROCESSING ########################################

# read in data ####
study1b_data <- read.csv("hardhalo study1b pilot_num_012622.csv")
study1b_data <- study1b_data %>%
  filter(Status == 0)  #remove non-data rows (variable info + test rows)

# rename ####
varnames <- read_xlsx("../../var names.xlsx", sheet = "study1b")
oldnames <- varnames[!is.na(varnames$old), ]$old  #removing empty rows
newnames <- varnames[!is.na(varnames$new), ]$new  #removing empty rows
study1b_data <- study1b_data %>%
  rename_at(vars(all_of(oldnames)), ~all_of(newnames))
# remove randomization order variables
study1b_data <- study1b_data %>%
  select(-ends_with("_DO", ignore.case = F))

# make numeric vars numeric ####
check_numeric <- function(x) {
  return(!is.na(as.numeric(x[1])))
}
study1b_data <- study1b_data %>%
  mutate_if(check_numeric, as.numeric)

# combine open-ended columns for two conditions
study1b_data <- study1b_data %>%
  tidyr::unite(CWVopen, highCWVopen, lowCWVopen, sep = "") %>%
  tidyr::unite(CWVactopen, highCWVactopen, lowCWVactopen, sep = "")

# drop incomplete cases ####
# sum(study1b_data$Finished!=1) #0
# study1b_data <- study1b_data %>% filter(Finished==1)

# drop cases that fail the attention check ####
study1b_data <- study1b_data %>%
  filter(entered_mTurkID == workerId & attencheck_1 == 2)  #Reordered the structures to make the code more clear  
# nrow(study1b_data) #145

# calculate mean CWV ####
# R=2,5,7,9,10
# alpha(select(study1b_data, c(CWV_1:CWV_10)),
#      check.keys=T)
study1b_data <- study1b_data %>% 
  rowwise() %>% #Reordered the structures to make the code more clear 
  mutate(CWV = mean(c(CWV_1,CWV_6,8-CWV_10,8-CWV_5))) %>%
  ungroup()

# categorical CWV ####

# factorizing random condition ####
study1b_data <- study1b_data %>%
  mutate(CWVmanip = ifelse(CWVmanip == "CWVmaniplow", 0, 1), targetgender = ifelse(target ==
                                                                                     "Jennifer", 2, 1))

# check randomization ###
# study1b_data %>%
#   group_by(CWVmanip, targetgender) %>%
#   summarize(n=n())

# constructing eval ####
# alpha(select(study1b_data, c(competent, intelligent)),
#       check.keys=T) #0.
# alpha(select(study1b_data, c(leader, negotiator, solver)),
#       check.keys=T) #0.
# alpha(select(study1b_data, c(competent, intelligent,
#                             leader, negotiator, solver)),
#       check.keys=T) #0.


study1b_data <- study1b_data %>%
  rowwise() %>%
  mutate(gen_competent = mean(c(competent, intelligent)), org_competent = mean(c(leader,
                                                                                 negotiator, solver))) %>%
  ungroup()

# other constructs ####
# alpha(select(study1b_data, c(open, dependable, extravert, warm, calm)),
#       check.keys=T) #0.9, 0.92 w/o extravert
# alpha(select(study1b_data, c(pos_genimpression, neg_genimpression)),
#       keys='neg_genimpression') #0.96
# alpha(select(study1b_data, c(outcome, motivation, backfire, performance)),
#       check.keys=T) #0.95
# alpha(select(study1b_data, c(forceful, dominant, assertive, cold, warm)),
#       keys='warm') #0.87
# alpha(select(study1b_data, c(cold, warm)),
#       keys='warm') #0.9



study1b_data <- study1b_data %>%
  rowwise() %>%
  mutate(posTIPI = mean(c(open, dependable, extravert, warm, calm)),
         genimpression = mean(c(pos_genimpression, 6 - neg_genimpression)),
         adapt = mean(c(outcome, motivation, backfire, performance)), self_bell = mean(c(self_dominant,
                                                                                         self_forceful, self_cold, 6 - self_warm))) %>%
  ungroup()

# categorical self_bell ####
lowselfbell <- mean(study1b_data$self_bell) - sd(study1b_data$self_bell)  #2.56
highselfbell <- mean(study1b_data$self_bell) + sd(study1b_data$self_bell)  #3.96
study1b_data <- study1b_data %>%
  mutate(cat_selfbell = factor(ifelse(self_bell < lowselfbell, "low",
                                      ifelse(self_bell >= highselfbell, "high", "med")), levels = c("low",
                                                                                                    "med", "high")))


# write.csv(study1b_data, file='study1b_processed.csv', row.names=F)
study1b_data <- read.csv("study1b_processed.csv") %>%
  mutate(cat_selfbell = factor(cat_selfbell, levels = c("low", "med",
                                                        "high")))

study1b_onlymed <- study1b_data %>%
  filter(cat_selfbell == "med")

study1b_onlyqual <- read_xlsx("study1b_processed.xlsx") %>%
  filter(screenopen == 0)  # screen garbage manipulation open responses


# nrow(study1b_onlyqual) #76
# nrow(study1b_onlyqual %>% filter(cat_selfbell == 'med'))

# standardized ####

# MAIN ANALYSES ########################################

# demographics ####
proportions(table(study1b_data$race))
proportions(table(study1b_data$gender))
proportions(table(study1b_data$education))
mean(study1b_data$age[study1b_data$age < 150])
sd(study1b_data$age[study1b_data$age < 150])
proportions(table(study1b_data$working))


# manipulation check ####
summary(lm(CWV ~ CWVmanip, study1b_data))
summary(lm(CWV ~ CWVmanip, study1b_onlymed))
summary(lm(CWV ~ self_bell, study1b_data))
summary(lm(CWV ~ CWVmanip, study1b_onlyqual))

# linear regression ####
summary(lm(gen_competent ~ CWVmanip, study1b_data))
summary(lm(org_competent ~ CWVmanip, study1b_data))
summary(lm(gen_competent ~ CWV, study1b_data))
summary(lm(org_competent ~ CWV, study1b_data))

summary(lm(gen_competent ~ CWVmanip, study1b_onlymed))
summary(lm(org_competent ~ CWVmanip, study1b_onlymed))

summary(lm(gen_competent ~ CWVmanip, study1b_onlyqual))
summary(lm(org_competent ~ CWVmanip, study1b_onlyqual))

# mediators
summary(lm(adapt ~ CWVmanip, study1b_data))
summary(lm(posTIPI ~ CWVmanip, study1b_data))
summary(lm(genimpression ~ CWVmanip, study1b_data))
summary(lm(adapt ~ CWV, study1b_data))
summary(lm(posTIPI ~ CWV, study1b_data))
summary(lm(genimpression ~ CWV, study1b_data))

summary(lm(adapt ~ CWVmanip, study1b_onlymed))
summary(lm(posTIPI ~ CWVmanip, study1b_onlymed))
summary(lm(genimpression ~ CWVmanip, study1b_onlymed))

# self_bell as CWV proxy
summary(lm(self_bell ~ CWVmanip, study1b_data))
summary(lm(adapt ~ self_bell, study1b_data))
summary(lm(posTIPI ~ self_bell, study1b_data))
summary(lm(genimpression ~ self_bell, study1b_data))

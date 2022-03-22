setwd('C:/Users/c1/OneDrive/Desktop/Research/Hard halos F21/Manip CWV pilot/v4')

library(dplyr)
library(readxl)
library(psych)

# PREPROCESSING ########################################

# read in data ####
manipCWV.4_data <- read.csv('hardhalo manipCWV.4_num_031622.csv')
manipCWV.4_data <- manipCWV.4_data %>% filter(Status==0) #remove non-data rows (variable info + test rows)

# rename ####
varnames <- read_xlsx('../../var names.xlsx', sheet='manipCWV.4')
oldnames <- varnames[!is.na(varnames$old),]$old #removing empty rows
newnames <- varnames[!is.na(varnames$new),]$new #removing empty rows
manipCWV.4_data <- manipCWV.4_data %>% 
  rename_at(vars(all_of(oldnames)), ~all_of(newnames))

# drop incomplete cases ####
# sum(manipCWV.4_data$Finished!=1)
# manipCWV.4_data <- manipCWV.4_data %>% filter(Finished==1)
# nrow(manipCWV.4_data) #300

# make numeric vars numeric ####
check_numeric <- function(x) { return(!is.na(as.numeric(x[1]))) }
manipCWV.4_data <- manipCWV.4_data %>%
  mutate_if(check_numeric, as.numeric)

# combine open-ended columns for two conditions ####
manipCWV.4_data <- manipCWV.4_data %>%
  tidyr::unite(CWVopen, highCWVopen, lowCWVopen, sep='')

# drop cases that fail the attention checks ####
manipCWV.4_data <- manipCWV.4_data %>%
  filter(entered_mTurkID==workerId &
           attencheck_1==2)
# nrow(manipCWV.4_data) #300
manipCWV.4_data <- manipCWV.4_data %>%
  filter((condition=='competitive' & howact>3) |
           (condition=='cooperative' & howact<3)) %>%
  filter((condition=='competitive' & whatwrite==1) |
           (condition=='cooperative' & whatwrite==2))
# nrow(manipCWV.4_data) #251

# calculate mean CWV ####
# R=2,5,7,9,10
# alpha(select(manipCWV.4_data, c(CWV_1:CWV_10)),
#      check.keys=T) #0.7
manipCWV.4_data <- manipCWV.4_data %>% rowwise() %>%
  mutate(CWV = mean(c(CWV_1,CWV_6,8-CWV_10,8-CWV_5))) %>%
  ungroup()

# factorizing random condition ####
manipCWV.4_data <- manipCWV.4_data %>%
  mutate(condition = ifelse(condition=='cooperative',0,1))

# constructing eval ####
# alpha(select(manipCWV.4_data, c(competent, intelligent)),
#       check.keys=T)
# alpha(select(manipCWV.4_data, c(leader, negotiator, solver)),
#       check.keys=T)
manipCWV.4_data <- manipCWV.4_data %>% rowwise() %>%
  mutate(gen_competent = mean(c(competent, intelligent)),
         org_competent = mean(c(leader, negotiator, solver))) %>%
  ungroup()

# other constructs ####

# write.csv(manipCWV.4_data, file='manipCWV.4_processed.csv', row.names=F)
# manipCWV.4_data <- read.csv('manipCWV.4_processed.csv') %>%
#   mutate(cat_selfbell = factor(cat_selfbell, levels=c('low','med','high')))

# standardized ####

# MAIN ANALYSES ########################################

# demographics ####
proportions(table(manipCWV.4_data$race))
proportions(table(manipCWV.4_data$gender))
proportions(table(manipCWV.4_data$education))
mean(manipCWV.4_data$age[manipCWV.4_data$age<150]); sd(manipCWV.4_data$age[manipCWV.4_data$age<150])
proportions(table(manipCWV.4_data$working))

# manipulation check ####
summary(lm(CWV ~ condition, manipCWV.4_data))

# linear regression ####
summary(lm(gen_competent ~ condition, manipCWV.4_data))
summary(lm(org_competent ~ condition, manipCWV.4_data))

manipCWV.4_data %>% filter(howrecent<=3) %>% nrow() #204
summary(lm(gen_competent ~ condition, manipCWV.4_data %>% filter(howrecent<=3)))
summary(lm(org_competent ~ condition, manipCWV.4_data %>% filter(howrecent<=3)))
summary(lm(gen_competent ~ condition, manipCWV.4_data %>% filter(howrecent>3)))
summary(lm(org_competent ~ condition, manipCWV.4_data %>% filter(howrecent>3)))
manipCWV.4_data %>% filter(howrecent<=5) %>% nrow() #204
summary(lm(gen_competent ~ condition, manipCWV.4_data %>% filter(howrecent<=2)))
summary(lm(org_competent ~ condition, manipCWV.4_data %>% filter(howrecent<=2)))
summary(lm(gen_competent ~ condition, manipCWV.4_data %>% filter(howrecent>3)))
summary(lm(org_competent ~ condition, manipCWV.4_data %>% filter(howrecent>3)))

summary(lm(gen_competent ~ condition * howrecent, manipCWV.4_data %>% filter(howrecent>3)))
summary(lm(org_competent ~ condition * howrecent, manipCWV.4_data %>% filter(howrecent>3)))
summary(lm(gen_competent ~ condition * howrecent, manipCWV.4_data %>% filter(howrecent>3)))
summary(lm(org_competent ~ condition * howrecent, manipCWV.4_data %>% filter(howrecent>3)))


summary(lm(gen_competent ~ CWV, manipCWV.4_data))
summary(lm(org_competent ~ CWV, manipCWV.4_data))

summary(lm(gen_competent ~ condition + CWV, manipCWV.4_data))
summary(lm(org_competent ~ condition + CWV, manipCWV.4_data))



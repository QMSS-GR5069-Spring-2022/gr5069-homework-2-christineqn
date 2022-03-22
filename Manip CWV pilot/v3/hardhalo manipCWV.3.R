setwd('C:/Users/c1/OneDrive/Desktop/Research/Hard halos F21/Manip CWV pilot/v3')

library(dplyr)
library(readxl)
library(psych)

# PREPROCESSING ########################################

# read in data ####
manipCWV.3_data <- read.csv('hardhalo manipCWV.3_num_030822.csv')
manipCWV.3_data <- manipCWV.3_data %>% filter(Status==0) #remove non-data rows (variable info + test rows)

# rename ####
varnames <- read_xlsx('../../var names.xlsx', sheet='manipCWV.3')
oldnames <- varnames[!is.na(varnames$old),]$old #removing empty rows
newnames <- varnames[!is.na(varnames$new),]$new #removing empty rows
manipCWV.3_data <- manipCWV.3_data %>% 
  rename_at(vars(all_of(oldnames)), ~all_of(newnames))

# drop incomplete cases ####
# sum(manipCWV.3_data$Finished!=1)
# manipCWV.3_data <- manipCWV.3_data %>% filter(Finished==1)
# nrow(manipCWV.3_data)

# make numeric vars numeric ####
check_numeric <- function(x) { return(!is.na(as.numeric(x[1]))) }
manipCWV.3_data <- manipCWV.3_data %>%
  mutate_if(check_numeric, as.numeric)

# combine open-ended columns for two conditions ####
manipCWV.3_data <- manipCWV.3_data %>%
  tidyr::unite(CWVopen, highCWVopen, lowCWVopen, sep='')

# drop cases that fail the attention checks ####
manipCWV.3_data <- manipCWV.3_data %>%
  filter(entered_mTurkID==workerId &
           attencheck_1==2 & attencheck_2==2)
# nrow(manipCWV.3_data) #203
manipCWV.3_data <- manipCWV.3_data %>%
  filter((condition=='competitive' & howact>3) |
           (condition=='cooperative' & howact<3)) %>%
  filter((condition=='competitive' & whatwrite==1) |
           (condition=='cooperative' & whatwrite==2))
# nrow(manipCWV.3_data) #151

# calculate mean CWV ####
# R=2,5,7,9,10
# alpha(select(manipCWV.3_data, c(CWV_1:CWV_10)),
#      check.keys=T) #0.7
manipCWV.3_data <- manipCWV.3_data %>% rowwise() %>%
  mutate(CWV = mean(c(CWV_1,CWV_6,8-CWV_10,8-CWV_5))) %>%
  ungroup()

# factorizing random condition ####
manipCWV.3_data <- manipCWV.3_data %>%
  mutate(condition = ifelse(condition=='cooperative',0,1))

# constructing eval ####
# alpha(select(manipCWV.3_data, c(competent, intelligent)),
#       check.keys=T)
# alpha(select(manipCWV.3_data, c(leader, negotiator, solver)),
#       check.keys=T)
manipCWV.3_data <- manipCWV.3_data %>% rowwise() %>%
  mutate(gen_competent = mean(c(competent, intelligent)),
         org_competent = mean(c(leader, negotiator, solver))) %>%
  ungroup()

# other constructs ####
# alpha(select(manipCWV.3_data, c(open, dependable, extravert, warm, calm)),
#       check.keys=T)
# alpha(select(manipCWV.3_data, c(pos_genimpression, neg_genimpression)),
#       keys='neg_genimpression')
# alpha(select(manipCWV.3_data, c(outcome, motivation, backfire, performance)),
#       check.keys=T)
manipCWV.3_data <- manipCWV.3_data %>% rowwise() %>%
  mutate(posTIPI = mean(c(open, dependable, extravert, warm, calm)),
         genimpression = mean(c(pos_genimpression, 6-neg_genimpression)),
         adapt = mean(c(outcome, motivation, backfire, performance)),
         self_bell = mean(c(self_dominant, self_forceful, self_cold, 6-self_warm))) %>%
  ungroup()

# categorical self_bell ####
lowselfbell <- mean(manipCWV.3_data$self_bell)-sd(manipCWV.3_data$self_bell) #2.56
highselfbell <- mean(manipCWV.3_data$self_bell)+sd(manipCWV.3_data$self_bell) #3.96
manipCWV.3_data <- manipCWV.3_data %>%
  mutate(cat_selfbell = factor(ifelse(self_bell<lowselfbell,'low',
                                      ifelse(self_bell>=highselfbell,'high','med')),
                               levels=c('low','med','high')))

# write.csv(manipCWV.3_data, file='study1b_processed.csv', row.names=F)
# manipCWV.3_data <- read.csv('study1b_processed.csv') %>%
#   mutate(cat_selfbell = factor(cat_selfbell, levels=c('low','med','high')))

manipCWV.3_onlymed <- manipCWV.3_data %>% filter(cat_selfbell=='med')
# nrow(manipCWV.3_onlymed) #107
manipCWV.3_onlyeasy <- manipCWV.3_data %>% filter(questioneasy>2 & thinkeasy>1)
# nrow(manipCWV.3_onlyeasy) #142

# standardized ####

# MAIN ANALYSES ########################################

# demographics ####
proportions(table(manipCWV.3_data$race))
proportions(table(manipCWV.3_data$gender))
proportions(table(manipCWV.3_data$education))
mean(manipCWV.3_data$age[manipCWV.3_data$age<150]); sd(manipCWV.3_data$age[manipCWV.3_data$age<150])
proportions(table(manipCWV.3_data$working))

# manipulation check ####
summary(lm(CWV ~ condition, manipCWV.3_data))
summary(lm(CWV ~ condition, manipCWV.3_onlymed))

# linear regression ####
summary(lm(gen_competent ~ condition, manipCWV.3_data))
summary(lm(org_competent ~ condition, manipCWV.3_data))

summary(lm(gen_competent ~ condition, manipCWV.3_onlymed))
summary(lm(org_competent ~ condition, manipCWV.3_onlymed))

summary(lm(gen_competent ~ condition, manipCWV.3_onlyeasy))
summary(lm(org_competent ~ condition, manipCWV.3_onlyeasy))
# manipCWV.3_data %>% filter(questioneasy>2) %>% nrow() #150
summary(lm(gen_competent ~ condition, manipCWV.3_data %>% filter(questioneasy>2)))
summary(lm(org_competent ~ condition, manipCWV.3_data %>% filter(questioneasy>2)))
# manipCWV.3_data %>% filter(thinkeasy>1) %>% nrow() #143
summary(lm(gen_competent ~ condition, manipCWV.3_data %>% filter(thinkeasy>1)))
summary(lm(org_competent ~ condition, manipCWV.3_data %>% filter(thinkeasy>1)))

summary(lm(gen_competent ~ CWV, manipCWV.3_data))
summary(lm(org_competent ~ CWV, manipCWV.3_data))

summary(lm(gen_competent ~ condition + CWV, manipCWV.3_data))
summary(lm(org_competent ~ condition + CWV, manipCWV.3_data))


# mediators
summary(lm(adapt ~ condition, manipCWV.3_data))
summary(lm(posTIPI ~ condition, manipCWV.3_data))
summary(lm(genimpression ~ condition, manipCWV.3_data))
summary(lm(adapt ~ CWV, manipCWV.3_data))
summary(lm(posTIPI ~ CWV, manipCWV.3_data))
summary(lm(genimpression ~ CWV, manipCWV.3_data))



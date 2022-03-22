setwd('C:/Users/c1/OneDrive/Desktop/Research/Hard halos F21/Manip CWV pilot/v2')

library(dplyr)
library(readxl)
library(psych)

# PREPROCESSING ########################################

# read in data ####
manipCWVpilot.2_data <- read.csv('hardhalo manipCWV pilot.2_num_021322.csv')
manipCWVpilot.2_data <- manipCWVpilot.2_data %>% filter(Status==0) #remove non-data rows (variable info + test rows)

# rename ####
varnames <- read_xlsx('../../var names.xlsx', sheet='manipCWV pilot.2')
oldnames <- varnames[!is.na(varnames$old),]$old #removing empty rows
newnames <- varnames[!is.na(varnames$new),]$new #removing empty rows
manipCWVpilot.2_data <- manipCWVpilot.2_data %>% 
  rename_at(vars(all_of(oldnames)), ~all_of(newnames))

# drop incomplete cases ####
# sum(manipCWVpilot.2_data$Finished!=1) #16
manipCWVpilot.2_data <- manipCWVpilot.2_data %>% filter(Finished==1)
# nrow(manipCWVpilot.2_data) #199

# make numeric vars numeric ####
check_numeric <- function(x) { return(!is.na(as.numeric(x[1]))) }
manipCWVpilot.2_data <- manipCWVpilot.2_data %>%
  mutate_if(check_numeric, as.numeric)

# recode miscoded questions ####
manipCWVpilot.2_data <- manipCWVpilot.2_data %>%
  mutate(across(c(harshlanguage:showconcern), ~ . - 10))

# combine open-ended columns for two conditions ####
manipCWVpilot.2_data <- manipCWVpilot.2_data %>%
  tidyr::unite(CWVopen, highCWVopen, lowCWVopen, sep='')

# drop cases that fail the attention checks ####
manipCWVpilot.2_data <- manipCWVpilot.2_data %>%
  filter(entered_mTurkID==workerId &
           attencheck_1==5)
# nrow(manipCWVpilot.2_data) #197
manipCWVpilot.2_data <- manipCWVpilot.2_data %>%
  filter((condition=='competitive' & howact>3) |
           (condition=='cooperative' & howact<3)) %>%
  filter((condition=='competitive' & whatwrite==1) |
           (condition=='cooperative' & whatwrite==4))
nrow(manipCWVpilot.2_data) #163

# calculate mean CWV ####
# R=2,5,7,9,10
# alpha(select(manipCWVpilot.2_data, c(CWV_1:CWV_10)),
#      check.keys=T) #0.7
manipCWVpilot.2_data <- manipCWVpilot.2_data %>% rowwise() %>%
  mutate(CWV = mean(c(CWV_1,CWV_6,8-CWV_10,8-CWV_5))) %>%
  ungroup()

# categorical CWV ####

# factorizing random condition ####
manipCWVpilot.2_data <- manipCWVpilot.2_data %>%
  mutate(condition = ifelse(condition=='cooperative',0,1))

# check randomization ####
# manipCWVpilot.2_data %>%
#   group_by(condition) %>%
#   summarize(n=n())

# constructing bell ####
# alpha(select(manipCWVpilot.2_data, c(harshlanguage:actintimidate, readyupset:blame)),
#       check.keys=T) #0.9
# alpha(select(manipCWVpilot.2_data, c(harshlanguage:actnice, readyupset:showconcern)),
#       check.keys=T) #0.91
alpha(select(manipCWVpilot.2_data, c(apologize, actnice, polite, showconcern)),
      check.keys=T) #0.78
manipCWVpilot.2_data <- manipCWVpilot.2_data %>% rowwise() %>%
  mutate(bell_behav = mean(c(harshlanguage, showdisgust, aggressivegestures,
                             actintimidate, readyupset, actcold, threats, blame)),
         warm_behav = mean(c(apologize, actnice, polite, showconcern))) %>%
  ungroup

# other constructs ####
manipCWVpilot.2_data <- manipCWVpilot.2_data %>% rowwise() %>%
  mutate(self_bell = mean(c(self_dominant, self_forceful, self_cold, 6-self_warm)),
         comprehension = ifelse(questioneasy>=3 & thinkeasy>=2, 1, 0)) %>%
  ungroup()

# categorical self_bell ####
lowselfbell <- mean(manipCWVpilot.2_data$self_bell)-sd(manipCWVpilot.2_data$self_bell) #2.56
highselfbell <- mean(manipCWVpilot.2_data$self_bell)+sd(manipCWVpilot.2_data$self_bell) #3.96
manipCWVpilot.2_data <- manipCWVpilot.2_data %>%
  mutate(cat_selfbell = factor(ifelse(self_bell<lowselfbell,'low',
                                 ifelse(self_bell>=highselfbell,'high','med')),
                          levels=c('low','med','high')))

# write.csv(manipCWVpilot.2_data, file='manipCWVpilot.2_processed.csv', row.names=F)
manipCWVpilot.2_data <- read.csv('manipCWVpilot.2_processed.csv') %>%
  mutate(cat_selfbell = factor(cat_selfbell, levels=c('low','med','high')))

manipCWVpilot.2_onlymed <- manipCWVpilot.2_data %>% filter(cat_selfbell=='med')

# standardized ####

# MAIN ANALYSES ########################################

# demographics ####
proportions(table(manipCWVpilot.2_data$race))
proportions(table(manipCWVpilot.2_data$gender))
proportions(table(manipCWVpilot.2_data$education))
mean(manipCWVpilot.2_data$age[manipCWVpilot.2_data$age<150]); sd(manipCWVpilot.2_data$age[manipCWVpilot.2_data$age<150])
proportions(table(manipCWVpilot.2_data$working))

# manipulation check ####
summary(lm(CWV ~ condition, manipCWVpilot.2_data))
summary(lm(CWV ~ condition, manipCWVpilot.2_onlymed))
summary(lm(CWV ~ self_bell, manipCWVpilot.2_data))
summary(lm(CWV ~ condition, manipCWVpilot.2_data %>% filter(comprehension==1)))

# linear regression ####
behaviors <- manipCWVpilot.2_data %>%
  select(c(bell_behav, harshlanguage:actintimidate, readyupset:blame,
           warm_behav, apologize, actnice, polite, showconcern)) %>%
  names()

getlms <- function(IV, coeff) {
  lms <- lapply(behaviors, function(behavior) {
    lm(as.formula(paste0(behavior, IV)), manipCWVpilot.2_data)
  })
  estimates <- NULL
  ps <- NULL
  for(i in 1:length(behaviors)){
    estimates <- append(estimates, summary(lms[[i]])$coefficients[coeff,1] %>% round(3))
    ps <- append(ps, summary(lms[[i]])$coefficients[coeff,4] %>% round(3))
  }
  df <- data.frame(term = behaviors,
                   estimate = estimates,
                   p = ps,
                   stars = ifelse(ps<.001, '***',
                                  ifelse(ps<.01, '**',
                                         ifelse(ps<.05, '*',
                                                ifelse(ps<.1, '.', '')))),
                   stringsAsFactors = F)
  df <- df %>% rename_with(~ paste(., IV), -term)
  return(df)
}
behaviors_condition <- getlms('~ condition', 2)
behaviors_CWV <- getlms('~ CWV', 2)
behaviors_both_cond <- getlms('~ condition+CWV', 2) %>%
  rename_with(~ paste(., '(condition)'), -term)
behaviors_both_CWV <- getlms('~ condition+CWV', 3) %>%
  rename_with(~ paste(., '(CWV)'), -term)

behaviors_lms <- behaviors_condition %>%
  full_join(behaviors_CWV, by='term') %>%
  full_join(behaviors_both_cond, by='term') %>%
  full_join(behaviors_both_CWV, by='term')
write.csv(behaviors_lms, file='behaviors_lms.csv', row.names=F)


summary(lm(bell_behav ~ condition, manipCWVpilot.2_onlymed))
summary(lm(warm_behav ~ condition, manipCWVpilot.2_onlymed))
summary(lm(bell_behav ~ condition, manipCWVpilot.2_data %>% filter(comprehension==1)))
summary(lm(warm_behav ~ condition, manipCWVpilot.2_data %>% filter(comprehension==1)))





# export choices for CPT modeling ----

# format for WinBugs
# block of gain gambles, then loss gambles, then mixed gambles.

# load library
library(tidyverse)

# load rawdata
load('data/rawclean.Rdata')

# generate summary df to see missing choices
short <- 
  rawclean %>%
  select(study, subject, trial, task, choice) %>%
  distinct(task, .keep_all = TRUE) %>%
  group_by(subject, study, trial) %>%
  summarise(gamble_count = n()) %>%
  arrange(gamble_count)
# one line per choice
raw_short <- 
  rawclean %>%  
  select(study, subject, trial, task, choice) %>%
  distinct(task, .keep_all = TRUE)

# add proper sequence of 91 gambles
good_seq <- data.frame(task = seq(1,91)) 
# see which subject had choices missing
raw_short %>%
  group_by(subject, study, trial) %>%
  summarise(gamble_count = n()) %>%
  arrange(gamble_count)
# fusion good sequence with raw data frame
choices <-   
  raw_short %>%
  complete(good_seq, subject, study, trial) %>%
  mutate(choice = case_when(
    choice == 'SpielA' ~ '0',
    choice == 'SpielB' ~ '1',
    is.na(choice) ~ '',
    TRUE ~ as.character(choice))
  )

# re-order choices and export in wide format  
# re-order for gain, loss, mixed
order_preferred <- tibble(task = c(c(1:25, 82:91), c(26:50), c(51:81)))

# mouse condition - trial 1
mouse1 <- 
  choices %>%
  filter(study == 'mouse' & trial == 1) %>% 
  spread(subject, choice) %>%
  ungroup() %>%
  select(-study) %>%
  select(-trial)

m1 <-   
  order_preferred %>%
  inner_join(mouse1) %>%
  select(-task)

# mouse condition - trial 2
mouse2 <- 
  choices %>%
  filter(study == 'mouse' & trial == 2) %>% 
  spread(subject, choice) %>%
  ungroup() %>%
  select(-study) %>%
  select(-trial)

m2 <-   
  order_preferred %>%
  inner_join(mouse2) %>%
  select(-task)

# click condition - trial 1
click1 <- 
  choices %>%
  filter(study == 'click' & trial == 1) %>% 
  spread(subject, choice) %>%
  ungroup() %>%
  select(-study) %>%
  select(-trial)

c1 <-   
  order_preferred %>%
  inner_join(click1) %>%
  select(-task)

# click condition - trial 2
click2 <- 
  choices %>%
  filter(study == 'click' & trial == 2) %>% 
  spread(subject, choice) %>%
  ungroup() %>%
  select(-study) %>%
  select(-trial)

c2 <-   
  order_preferred %>%
  inner_join(click2) %>%
  select(-task)

write_delim(m1, ('CPTModeling/Mouse_Trial1.txt'), delim = '\t', col_names = FALSE)
write_delim(m2, ('CPTModeling/Mouse_Trial2.txt'), delim = '\t', col_names = FALSE)
write_delim(c1, ('CPTModeling/Click_Trial1.txt'), delim = '\t', col_names = FALSE)
write_delim(c2, ('CPTModeling/Click_Trial2.txt'), delim = '\t', col_names = FALSE)

# export order of ids
m1_ids <- data.frame(colnames(m1))
c1_ids <- data.frame(colnames(c1))

write_delim(m1_ids, ('CPTModeling/m1_ids.txt'), delim = '\t', col_names = TRUE)
write_delim(c1_ids, ('CPTModeling/c1_ids.txt'), delim = '\t', col_names = TRUE)

# export subject per condition
sm1 <- 
  choices %>%
  filter(study == 'mouse' & trial == 1) %>%
  distinct(subject) %>%
  ungroup() %>%
  select(-trial) %>%
  rename(s_mouse1 = subject) 

sm2 <- 
  choices %>%
  filter(study == 'mouse' & trial == 2) %>%
  distinct(subject) %>%
  ungroup() %>%
  select(-trial) %>%
  rename(s_mouse2 = subject) 

sc1 <- 
  choices %>%
  filter(study == 'click' & trial == 1) %>%
  distinct(subject) %>%
  ungroup() %>%
  select(-trial) %>%
  rename(s_click1 = subject) 

sc2 <- 
  choices %>%
  filter(study == 'click' & trial == 2) %>%
  distinct(subject) %>%
  ungroup() %>%
  select(-trial) %>%
  rename(s_click2 = subject) 

subject_mapping_trials <- cbind(sm1, sm2, sc1, sc2)

# read in gamble values 
gambles <- read_csv('data/LotteryProblems.csv')
# re-order for gain, loss, mixed
order_preferred <- tibble(Number = c(c(1:25, 82:91), c(26:50), c(51:81)))
gambles <- 
  order_preferred %>%
  inner_join(gambles) %>%
  mutate_at(vars(starts_with('p')), funs(. / 100)) 

# split into gamble A and B 

gambleA <- 
  gambles %>%
  select('oA1', 'pA1', 'oA2', 'pA2')
gambleB <- 
  gambles %>%
  select('oB1', 'pB1', 'oB2', 'pB2')

# Rank-order outcomes in preparation for cummulative prospect theory
# Gamble A
gambleA$oA1_n <- ifelse(gambleA$oA1 >= 0 & gambleA$oA2 >= 0,
                        ifelse(gambleA$oA1 > gambleA$oA2, gambleA$oA1, gambleA$oA2),
                        ifelse(gambleA$oA1 <= 0 & gambleA$oA2 <= 0,
                               ifelse(gambleA$oA1 < gambleA$oA2, gambleA$oA1, gambleA$oA2),
                               ifelse(gambleA$oA1 > gambleA$oA2, gambleA$oA1, gambleA$oA2)))
gambleA$pA1_n <- ifelse(gambleA$oA1 >= 0 & gambleA$oA2 >= 0,
                        ifelse(gambleA$oA1 > gambleA$oA2, gambleA$pA1, gambleA$pA2),
                        ifelse(gambleA$oA1 <= 0 & gambleA$oA2 <= 0,
                               ifelse(gambleA$oA1 < gambleA$oA2, gambleA$pA1, gambleA$pA2),
                               ifelse(gambleA$oA1 > gambleA$oA2, gambleA$pA1, gambleA$pA2)))
gambleA$oA2_n <- ifelse(gambleA$oA1 >= 0 & gambleA$oA2 >= 0,
                        ifelse(gambleA$oA1 > gambleA$oA2, gambleA$oA2, gambleA$oA1),
                        ifelse(gambleA$oA1 <= 0 & gambleA$oA2 <= 0,
                               ifelse(gambleA$oA1 < gambleA$oA2, gambleA$oA2, gambleA$oA1),
                               ifelse(gambleA$oA1 > gambleA$oA2, gambleA$oA2, gambleA$oA1)))
gambleA$pA2_n <- ifelse(gambleA$oA1 >= 0 & gambleA$oA2 >= 0,
                        ifelse(gambleA$oA1 > gambleA$oA2, gambleA$pA2, gambleA$pA1),
                        ifelse(gambleA$oA1 <= 0 & gambleA$oA2 <= 0,
                               ifelse(gambleA$oA1 < gambleA$oA2, gambleA$pA2, gambleA$pA1),
                               ifelse(gambleA$oA1 > gambleA$oA2, gambleA$pA2, gambleA$pA1)))

gambleA$oA1 <- NULL; gambleA$pA1 <- NULL; gambleA$oA2 <- NULL; gambleA$pA2 <- NULL
colnames(gambleA) <- c("oA1", "pA1", "oA2", "pA2")

# Gamble B
gambleB$oB1_n <- ifelse(gambleB$oB1 >= 0 & gambleB$oB2 >= 0,
                        ifelse(gambleB$oB1 > gambleB$oB2, gambleB$oB1, gambleB$oB2),
                        ifelse(gambleB$oB1 <= 0 & gambleB$oB2 <= 0,
                               ifelse(gambleB$oB1 < gambleB$oB2, gambleB$oB1, gambleB$oB2),
                               ifelse(gambleB$oB1 > gambleB$oB2, gambleB$oB1, gambleB$oB2)))
gambleB$pB1_n <- ifelse(gambleB$oB1 >= 0 & gambleB$oB2 >= 0,
                        ifelse(gambleB$oB1 > gambleB$oB2, gambleB$pB1, gambleB$pB2),
                        ifelse(gambleB$oB1 <= 0 & gambleB$oB2 <= 0,
                               ifelse(gambleB$oB1 < gambleB$oB2, gambleB$pB1, gambleB$pB2),
                               ifelse(gambleB$oB1 > gambleB$oB2, gambleB$pB1, gambleB$pB2)))
gambleB$oB2_n <- ifelse(gambleB$oB1 >= 0 & gambleB$oB2 >= 0,
                        ifelse(gambleB$oB1 > gambleB$oB2, gambleB$oB2, gambleB$oB1),
                        ifelse(gambleB$oB1 <= 0 & gambleB$oB2 <= 0,
                               ifelse(gambleB$oB1 < gambleB$oB2, gambleB$oB2, gambleB$oB1),
                               ifelse(gambleB$oB1 > gambleB$oB2, gambleB$oB2, gambleB$oB1)))
gambleB$pB2_n <- ifelse(gambleB$oB1 >= 0 & gambleB$oB2 >= 0,
                        ifelse(gambleB$oB1 > gambleB$oB2, gambleB$pB2, gambleB$pB1),
                        ifelse(gambleB$oB1 <= 0 & gambleB$oB2 <= 0,
                               ifelse(gambleB$oB1 < gambleB$oB2, gambleB$pB2, gambleB$pB1),
                               ifelse(gambleB$oB1 > gambleB$oB2, gambleB$pB2, gambleB$pB1)))

gambleB$oB1 <- NULL; gambleB$pB1 <- NULL; gambleB$oB2 <- NULL; gambleB$pB2 <- NULL
colnames(gambleB) <- c("oB1", "pB1", "oB2", "pB2")

write_delim(gambleA, ('CPTModeling/GambleA.txt'), delim = '\t', col_names = FALSE)
write_delim(gambleB, ('CPTModeling/GambleB.txt'), delim = '\t', col_names = FALSE)


#### get problem solved with help from stackoverflow ### ----  
full <- 
  data.frame(id = seq(1,5))

problem <- 
  data.frame(id = c(c(1,2,4,5),c(1,3,4,5)), group = c(rep(1,4), rep(2,4)), target = c(rep(c('A','B'),4)))

problem %>%
  complete(id, group) # for all combinations of two dataframes
####

### Thomas takes care of mixed gamble recoding - but Michael was faster doing this manually

rm(list=ls())
setwd("~/Desktop")
data <- read.table("GambleA.txt")

data$outcome_1 <- ifelse(data$V1 >= 0 & data$V3 >= 0, data$V1,
                         ifelse(data$V1 <= 0 & data$V3 <= 0, data$V1,
                                ifelse(data$V1 >= 0, data$V1, data$V3)))

data$outcome_2 <- ifelse(data$V1 >= 0 & data$V3 >= 0, data$V3,
                         ifelse(data$V1 <= 0 & data$V3 <= 0, data$V3,
                                ifelse(data$V3 > 0, data$V1, data$V3)))
# The Attention-Aversion Gap
# Tomas Lejarraga, Michael Schulte-Mecklenbeck, Thorsten Pachur, Ralph Hertwig

# data cleaning

# packages
  library(tidyverse)
  library(lubridate)
  
# load data from three experiments
  rawdata <- read_csv('data/rawdata.csv')

# remove participants based on less thank 90% choices made
  rawdata <- 
  rawdata %>%
    filter(!subject %in% c('c48', 'c61', 'm47', 'm66'))

  # fusion gamble information into rawdata frame
  # Number: gamble number [1:91]
  lotteryproblems <- read.table("data/LotteryProblems_boxvalue.csv", header=TRUE, sep=',')
 
  # merge rawclean file with lottery problems
  rawdata <- merge(rawdata, lotteryproblems, 
                   by.x = c("task", "boxname"), 
                   by.y = c("task", "boxname"), 
                   all.x = TRUE)
  
# remove very short acquisitions <100ms on acquisition level
  rawclean  <- 
  rawdata %>%
    filter(boxtime > 100)
  
# remove outliers on boxtime level with mean > 2SD 
  mean_boxtime <- mean(rawclean$boxtime)
  sd_boxtime <- sd(rawclean$boxtime)
  
  rawclean <- 
  rawclean %>%
    filter(boxtime < mean_boxtime + 2*sd_boxtime) 

# remove button events
  rawclean <-  
    rawclean %>%
    filter(!(type == 'bA' | type == 'bB')) # remove button events
  
# convert submitted into real date 
  rawclean$time_corrected <- ymd_hms(rawclean$submitted, locale = Sys.getlocale("LC_TIME"))
  # order                                                     
  rawclean <- arrange(rawclean, time_corrected, counter)
  # enumerate trial position into early, middle, late trials within each subjects
  rawclean <- 
  rawclean %>%
    group_by(trial, subject, time_corrected) %>%
    mutate(
      flag = case_when(
        time_corrected == lag(time_corrected) ~ 0,
        TRUE ~ 1)
           ) %>%
    group_by(trial, subject) %>%
    mutate(position = cumsum(flag)) %>%
    mutate(
      position_gamble = case_when(
        position <= 30 ~ 'early',
        position > 30 & position  <= 60 ~ 'middle',
        position > 60 ~ 'late')
    )

# re-order factor levels for gamble_position
  rawclean$position_gamble <- factor(rawclean$position_gamble, levels = c("early", "middle", "late"))
  
# change type labels O - Outcome / P - Probability
  rawclean <- 
    rawclean %>%
    mutate(type = as.factor(type)) %>%
    mutate(type = fct_recode(type,
                             "Outcome" = "O",
                             "Probability" = "P"))

# add new column with cell based info pos / neg
  rawclean$value_pn <- str_sub(rawclean$celltypeF, 2,4)
  
# save rawclean
  save(rawclean, file = 'data/rawclean.Rdata') # 661483 rows | 27 variables
  
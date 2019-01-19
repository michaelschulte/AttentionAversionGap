# Loss Vigilance
# Tomas Lejarraga, Michael Schulte-Mecklenbeck
# stats and figures for paper 1:1

# packages
library(tidyverse)


# Preparation for data fusion

# load parameter data ----
click_conv <- read_csv('data/Parameter_CLICK_subject_conversion.csv')
click_para <- read_csv('data/Parameter_CLICK.csv')
mouse_conv <- read_csv('data/Parameter_MOUSE_subject_conversion.csv')
mouse_para <- read_csv('data/Parameter_MOUSE.csv')

# fusion parameter and subject ID data
click <- 
  click_para %>%
  left_join(click_conv, by = c('thorsten.numbers' = 'subject')) %>%
  select(thorsten.num, trial, lambda) %>%
  group_by(thorsten.num) %>%
  summarise(lambda = mean(lambda)) %>%
  rename('subject' = 'thorsten.num') %>%
  mutate(condition = 'Click')

mouse <- 
  mouse_para %>%
  left_join(mouse_conv, by = c('thorsten.numbers' = 'subject')) %>%
  select(thorsten.num, trial, lambda) %>%
  group_by(thorsten.num) %>%
  rename('subject' = 'thorsten.num') %>%
  summarise(lambda = mean(lambda)) %>%
  mutate(condition = 'Mouse')    

parameters <- bind_rows(mouse, click)

# save parameters
save(parameters, file = 'data/parameters.Rdata')      




# load data from three experiments
  load('data/rawclean.Rdata')
  gambles <- read_csv('data/LotteryProblems.csv')
  
  # parameter estimates 
  results_lambdas <- read_csv('data/parameters.txt')

# Figures - gain_loss ----  
  rawclean %>%
    group_by(trial, subject, task, type, gambletype) %>%
    summarise(overall_time = sum(boxtime)) %>%
    group_by(subject, type, gambletype) %>%
    summarise(time = mean(overall_time)) %>%
    ggplot(., (aes(x = gambletype, y = time, colour = gambletype))) + 
    stat_summary(fun.y = mean, geom = "point", size = 1.6) +
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1.2) +  
    facet_grid(. ~ type) + 
    xlab('Problem domain') + 
    ylab('Average time in milliseconds') +
    scale_colour_manual(values = c("loss" = "dark red", "gain" = "dark green", "mixed" = "black")) +
    scale_x_discrete(labels = c("loss" = "Loss","gain" = "Gain", "mixed" = "Mixed")) +
    theme_minimal(base_family = 'mono', base_size = 18) +
    theme(legend.position="none")
     
  #ggsave(file = 'plots/gain_loss.png', height = 5, width = 10)
  
  #  Figures - mixed ----  
  
  rawclean_mixed <- 
  rawclean %>%
    filter(gambletype == 'mixed') %>%
    group_by(trial, subject, task, type, gambletype, celltypeF) %>%
    summarise(overall_time = sum(boxtime)) %>%
    group_by(subject, type, celltypeF) %>%
    summarise(time = mean(overall_time)) %>%
    mutate(celltypeF = substring(celltypeF, 2, 4))
  
    ggplot(rawclean_mixed, (aes(x = celltypeF, y = time, colour = celltypeF))) + 
    #geom_point(alpha = .1) + 
    stat_summary(fun.y = mean, geom = "point", colour = 'black') +
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1.1, colour = 'black') +  
    facet_grid(. ~ type) + 
    xlab('Problem domain') + 
    ylab('Average time in milliseconds') +
    scale_x_discrete(labels = c("neg" = "Loss","pos" = "Gain")) +
    theme_minimal(base_family = 'mono', base_size = 18) +
    theme(legend.position="none")
  
  #ggsave(file = 'plots/mixed.png', height = 5, width = 10)
  
# Figures - pos_neg within ----
  
  pos_neg <-   
    rawclean %>%
    filter(!gambletype == 'mixed') %>%
    select(task, subject, boxname, trial, event, boxtime, value_pn, type) %>%
    group_by(trial, subject, type, value_pn, task) %>%
    summarise(sum_boxtime = sum(boxtime)) %>%
    group_by(subject, type, value_pn) %>%
    summarise(av_boxtime = (mean(sum_boxtime))) %>%
    spread(value_pn, av_boxtime) %>%
    mutate(line = case_when(
      neg > pos ~ 1,
      pos > neg ~ 0
    ))
  
  ggplot(pos_neg, aes(neg, pos)) +
    geom_abline(intercept = 0, colour = 'Grey') +
    geom_point(alpha = .2, size = 3, aes(colour = as.factor(line)), show.legend=F) +
    scale_color_manual(values = c("1" = "dark red", "0" = "dark green")) +
    xlab('Loss') +
    ylab('Gain') +
    ylim(0,26000) + 
    xlim(0,26000) + 
    facet_grid(. ~ type) +
    theme_minimal(base_family = 'mono', base_size = 18) 
  
  #ggsave('plots/gain_loss_within.png', height = 5, width = 10)  
  
# Parameters versus attention ----
 # michael's replication version + tomas' nice version 
 head(results_lambdas)
  
 # differences in No of subjects in lambda and rawclean
  subj_raw <- sort(unique(rawclean$subject))
  subj_lam <- sort(unique(results_lambdas$subject))
  
  subj_raw %in% subj_lam
  subj_lam %in% subj_raw 
  
 head(rawclean)
    open_time <- 
    rawclean %>%
      select(task, trial, subject, boxtime, gambletype, study) %>%
      filter(!gambletype == 'mixed') %>%
      group_by(trial, subject, gambletype, task) %>%
      summarise(time = sum(boxtime)) %>%
      left_join(results_lambdas, by = c('subject' = 'subject')) %>% 
      group_by(subject, gambletype, mean_median_lambda) %>%
      summarise(av_time = mean(time)) %>%
      spread(gambletype, av_time) %>%
      mutate(diff_time = loss/gain)
    
    open_time$line <- ifelse(open_time$diff_time < 1, 0,
                             ifelse(open_time$diff_time > 1, 1, "NA"))
    
      
      # Opening time: counting cases
      open_time <- subset(open_time, mean_median_lambda > 0) # Corrected 16/01/2019
      la_la <- round(nrow(subset(open_time, line == 1 & mean_median_lambda > 1))/nrow(open_time), 2)*100 # Loss attention & loss aversion
      la_gs <- round(nrow(subset(open_time, line == 1 & mean_median_lambda < 1))/nrow(open_time), 2)*100 # Loss attention & gain seeking
      ga_la <- round(nrow(subset(open_time, line == 0 & mean_median_lambda > 1))/nrow(open_time), 2)*100 # Gain attention & loss aversion
      ga_gs <- round(nrow(subset(open_time, line == 0 & mean_median_lambda < 1))/nrow(open_time), 2)*100 # Gain attention & gain seeking
      
      ggplot(open_time, aes(diff_time, mean_median_lambda)) +
        scale_color_manual(values = c("1" = "dark red", "0" = "dark green")) +
        xlab(paste('Relative box opening time')) +
        ylab(expression(paste("Loss aversion (",lambda, ") "))) +
        annotate("rect", xmin=-Inf, xmax=Inf, ymin=1, ymax=Inf, fill = "dark grey", alpha = 0.3) +
        annotate("rect", xmin=1, xmax=Inf, ymin=-Inf, ymax=Inf, fill = "lightblue", alpha = 0.3) +
        geom_smooth(method=lm,se=FALSE, colour = "dark grey", size = 0.5) +
        geom_point(alpha = .7, size = 3, aes(colour = as.factor(line)), show.legend=F) +
        annotate("text", x = 2, y = 2.1, label =  paste0(la_la,"%", "\n loss averse & \n loss vigilant "), size = 3, family = "mono") +
        annotate("text", x = 2, y = 0.41, label =  paste0(la_gs,"%", "\n gain seeking & \n loss vigilant "), size = 3, family = "mono") +
        annotate("text", x = 0.75, y = 2.1, label =  paste0(ga_la,"%", "\n loss averse & \n gain vigilant "), size = 3, family = "mono") +
        annotate("text", x = 0.75, y = 0.41, label =  paste0(ga_gs,"%", "\n gain seeking & \n gain vigilant "), size = 3, family = "mono") +
        #annotate("text", x = 4300, y = 2.5, label =  paste0("TEST"), size = 6, family = "mono") +
        theme_minimal(base_family = 'mono', base_size = 18)  

  
    #ggsave('plots/aversion_attention_open_time.png', width = 6, height = 6,  dpi = 300) 
 
# Appendix - Figures gain-loss split on trials and mouse-click ----
  library(gridExtra)
  
  p1 <- 
  rawclean %>%
    filter(!celltypeF == 'Butt') %>%
    filter(trial == 1) %>%
    group_by(study, trial, subject, task, type, gambletype) %>%
    summarise(overall_time = sum(boxtime)) %>%
    group_by(study, trial, subject, type, gambletype) %>%
    summarise(time = mean(overall_time)) %>%
    ggplot(., (aes(x = gambletype, y = time, colour = gambletype))) + 
    #geom_point(alpha = .1) + 
    stat_summary(fun.y = mean, geom = "point") +
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +  
    facet_grid(study ~ type) + 
    xlab('Problem domain') + 
    ylab('Average time in milliseconds') +
    scale_colour_manual(values = c("loss" = "dark red", "gain" = "dark green", "mixed" = "black")) +
    scale_x_discrete(labels = c("loss" = "Loss","gain" = "Gain", "mixed" = "Mixed")) +
    theme_minimal(base_family = 'mono', base_size = 18) +
    theme(legend.position="none") +
    ggtitle('Trial 1')
  
  p2 <- 
    rawclean %>%
    filter(!celltypeF == 'Butt') %>%
    filter(trial == 2) %>%
    group_by(study, subject, task, type, gambletype) %>%
    summarise(overall_time = sum(boxtime)) %>%
    group_by(study, subject, type, gambletype) %>%
    summarise(time = mean(overall_time)) %>%
    ggplot(., (aes(x = gambletype, y = time, colour = gambletype))) + 
    #geom_point(alpha = .1) + 
    stat_summary(fun.y = mean, geom = "point") +
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +  
    facet_grid(study ~ type) + 
    xlab('Problem domain') + 
    ylab('Average time in milliseconds') +
    scale_colour_manual(values = c("loss" = "dark red", "gain" = "dark green", "mixed" = "black")) +
    scale_x_discrete(labels = c("loss" = "Loss","gain" = "Gain", "mixed" = "Mixed")) +
    theme_minimal(base_family = 'mono', base_size = 18) +
    theme(legend.position="none")+
    ggtitle('Trial 2')
  
  out <- grid.arrange(p1, p2, nrow = 1)
  
  #ggsave('plots/appendix_gain_loss_comparison.png', out)  

  
# Figure 5: Loss aversion as a function of vigilance to losses and gains -----

  # load parameter datasets

  m_trial1 <- read_delim('data/results_lambda_mouse_Trial1.txt', delim = ' ')
  m_trial2 <- read_delim('data/results_lambda_mouse_Trial2.txt', delim = ' ')
  c_trial1 <- read_delim('data/results_lambda_Click_Trial1.txt', delim = ' ')
  c_trial2 <- read_delim('data/results_lambda_Click_Trial2.txt', delim = ' ')
  
  mouse_lambda <- merge(m_trial1, m_trial2, by = "row")
  click_lambda <- merge(c_trial1, c_trial2, by = "row")
  
  mouse_lambda$mean_median_lambda <- (mouse_lambda$hierarchical.median.lambda.x + mouse_lambda$hierarchical.median.lambda.y)/2
  mouse_lambda$mean_mean_lambda <- (mouse_lambda$hierarchical.mean.lambda.x + mouse_lambda$hierarchical.mean.lambda.y)/2
  mouse_lambda$mean_sd_lambda <- (mouse_lambda$hierarchical.sd.lambda.x + mouse_lambda$hierarchical.sd.lambda.y)/2
  mouse_lambda <- mouse_lambda[c(1,8:10)]
  
  click_lambda$mean_median_lambda <- (click_lambda$hierarchical.median.lambda.x + click_lambda$hierarchical.median.lambda.y)/2
  click_lambda$mean_mean_lambda <- (click_lambda$hierarchical.mean.lambda.x + click_lambda$hierarchical.mean.lambda.y)/2
  click_lambda$mean_sd_lambda <- (click_lambda$hierarchical.sd.lambda.x + click_lambda$hierarchical.sd.lambda.y)/2
  click_lambda <- click_lambda[c(1,8:10)]
  
  # Get maps of subject ID's from choice data
  m1_ids <- read_delim('data/m1_ids.txt', delim = ' ')
  c1_ids <- read_delim('data/c1_ids.txt', delim = ' ')
  
  
  m1_ids$row <- 1:nrow(m1_ids)
  c1_ids$row <- 1:nrow(c1_ids)
  
  mouse_lambda <- merge(mouse_lambda, m1_ids, by.x = "row", by.y = "row")
  mouse_lambda$condition <- "mouse"
  mouse_lambda$row <- NULL
  
  click_lambda <- merge(click_lambda, c1_ids, by.x = "row", by.y = "row")
  click_lambda$condition <- "click"
  click_lambda$row <- NULL 
  
  lambdas <- rbind(mouse_lambda, click_lambda)
  rm(m_trial1, m_trial2, c_trial1, c_trial2, mouse_lambda, click_lambda, m1_ids, c1_ids)
  
# Relate vigilance to aversion
  
  open_time <- rawclean %>%
    group_by(trial, subject, task, gambletype) %>%
    summarise(overall_time = sum(boxtime)) %>%
    group_by(subject, gambletype) %>%
    summarise(time = mean(overall_time))
  
  open_time <-   
    open_time %>%
    select(subject, gambletype, time) %>%
    spread(gambletype, time) %>%
    mutate(asymmetry_box = loss/gain)
  
  open_time$line <- ifelse(open_time$asymmetry_box < 1, 0,
                           ifelse(open_time$asymmetry_box > 1, 1, "NA"))
  names(lambdas)[names(lambdas) == "id"] <- "subject"
  
  open_time <- merge(results_lambdas, open_time, by = "subject") # Some subjects were removed based on missing values
  
  # Opening time: counting cases
  la_la <- round(nrow(subset(open_time, line == 1 & mean_median_lambda > 1))/nrow(open_time), 2)*100 # Loss attention & loss aversion
  la_gs <- round(nrow(subset(open_time, line == 1 & mean_median_lambda < 1))/nrow(open_time), 2)*100 # Loss attention & gain seeking
  ga_la <- round(nrow(subset(open_time, line == 0 & mean_median_lambda > 1))/nrow(open_time), 2)*100 # Gain attention & loss aversion
  ga_gs <- round(nrow(subset(open_time, line == 0 & mean_median_lambda < 1))/nrow(open_time), 2)*100 # Gain attention & gain seeking
  
  ggplot(open_time, aes(asymmetry_box, mean_median_lambda)) +
    scale_color_manual(values = c("1" = "dark red", "0" = "dark green")) +
    xlab(paste('Relative box opening time')) +
    ylab(expression(paste("Loss aversion (",lambda, ") "))) +
    annotate("rect", xmin=-Inf, xmax=Inf, ymin=1, ymax=Inf, fill = "dark grey", alpha = 0.3) +
    annotate("rect", xmin=1, xmax=Inf, ymin=-Inf, ymax=Inf, fill = "lightblue", alpha = 0.3) +
    annotate("text", x = 2, y = 2.15, label =  paste0(la_la,"%", "\n loss averse & \n loss vigilant "), size = 3, family = "mono") +
    annotate("text", x = 2, y = 0.4, label =  paste0(la_gs,"%", "\n gain seeking & \n loss vigilant "), size = 3, family = "mono") +
    annotate("text", x = 0.75, y = 2.15, label =  paste0(ga_la,"%", "\n loss averse & \n gain vigilant "), size = 3, family = "mono") +
    annotate("text", x = 0.75, y = 0.4, label =  paste0(ga_gs,"%", "\n gain seeking & \n gain vigilant "), size = 3, family = "mono") +
    theme_minimal(base_family = 'mono', base_size = 18) +
    geom_smooth(method=lm,se=FALSE, colour = "dark grey", size = 0.5) +
    geom_point(alpha = .7, size = 3, aes(colour = as.factor(line)), show.legend=F) 
  
  #ggsave('plots/aversion_attention_open_time.png', width = 6, height = 6,  dpi = 300)
  
  

# Counting cases ----
# Loss aversion in choice
nrow(subset(results_lambdas, mean_median_lambda > 1))  # Loss averse
nrow(subset(results_lambdas, mean_median_lambda < 1))  # Gain averse
nrow(subset(results_lambdas, mean_median_lambda == 1)) # Loss neutral

# Opening time
nrow(subset(open_time, line == 1)) # Loss attention
nrow(subset(open_time, line == 0)) # Gain attention
nrow(subset(open_time, line == 1 & mean_median_lambda > 1)) # Loss attention & loss aversion
nrow(subset(open_time, line == 1 & mean_median_lambda < 1)) # Loss attention & gain aversion
nrow(subset(open_time, line == 0 & mean_median_lambda > 1)) # Gain attention & loss aversion
nrow(subset(open_time, line == 0 & mean_median_lambda < 1)) # Gain attention & gain aversion


# Stats for paper
round(mean(results_lambdas$mean_median_lambda), 2)
round(sd(results_lambdas$mean_median_lambda), 2)
cor.test(open_time$mean_median_lambda, open_time$asymmetry_box, method = "spearman")


# Conceptual discussion of loss aversion in McDermott et al.'s (2008) survival function ----
#install.packages('pBrackets')
library(pBrackets)
#install.packages('Cairo')
library(Cairo)


# McDermott et al.'s (2008) survival function
eq = function(x){1/(1+(exp(-x)))}
plot(eq(-10:10), type='l')

ggplot(data = data.frame(x = 0), mapping = aes(x = x)) +
  stat_function(fun = eq) +
  #xlim(-5, 5) +
  ylab("Probability of survival") +
  xlab("Total payoff") +
  # critical line
  annotate("segment", x = 0, xend = 0, y = 0, yend = 1, colour = "black", linetype="dashed") +
  # "a" and "b" points
  annotate("point", x = -2.5, y = eq(-2.5)) +
  annotate("point", x = 2.5, y = eq(2.5)) +
  # "a" and "b" labels
  annotate("text", x = -2.5, y = -0.03, label = "a") +
  annotate("text", x = 2.5, y = -0.03, label = "b") +
  # "a" and "b" lines
  annotate("segment", x = -2.5, xend = -Inf, y = eq(-2.5), yend = eq(-2.5), colour = "black", size = .1) +
  annotate("segment", x = -2.5, xend = -2.5, y = eq(-2.5), yend = 0, colour = "black", size = .1) +
  annotate("segment", x = 2.5, xend = -Inf, y = eq(2.5), yend = eq(2.5), colour = "black", size = .1) +
  annotate("segment", x = 2.5, xend = 2.5, y = eq(2.5), yend = 0, colour = "black", size = .1) +
  # "a" and "b" minus mu points
  annotate("point", x = -3.5, y = eq(-3.5), colour = "dark red") +
  annotate("point", x = 1.5, y = eq(1.5), colour = "dark red") +
  # "a" and "b" minus mu lines
  annotate("segment", x = -3.5, xend = -Inf, y = eq(-3.5), yend = eq(-3.5), colour = "dark red", size = .1) +
  annotate("segment", x = -3.5, xend = -3.5, y = eq(-3.5), yend = 0, colour = "dark red", size = .1) +
  annotate("segment", x = 1.5, xend = -Inf, y = eq(1.5), yend = eq(1.5), colour = "dark red", size = .1) +
  annotate("segment", x = 1.5, xend = 1.5, y = eq(1.5), yend = 0, colour = "dark red", size = .1) +
  # "a" and "b" plus mu points
  annotate("point", x = -1.5, y = eq(-1.5), colour = "dark green") +
  annotate("point", x = 3.5, y = eq(3.5), colour = "dark green") +
  # "a" and "b" plus mu lines
  annotate("segment", x = -1.5, xend = -Inf, y = eq(-1.5), yend = eq(-1.5), colour = "dark green", size = .1) +
  annotate("segment", x = -1.5, xend = -1.5, y = eq(-1.5), yend = 0, colour = "dark green", size = .1) +
  annotate("segment", x = 3.5, xend = -Inf, y = eq(3.5), yend = eq(3.5), colour = "dark green", size = .1) +
  annotate("segment", x = 3.5, xend = 3.5, y = eq(3.5), yend = 0, colour = "dark green", size = .1) +
  # delta's
  annotate("text", x = 1.5, y = -0.03, label = "b - delta", parse=T, colour = "dark red") +
  annotate("text", x = 3.5, y = -0.03, label = "b + delta", parse=T, colour = "dark green") +
  annotate("text", x = -3.5, y = -0.03, label = "a - delta", parse=T, colour = "dark red") +
  annotate("text", x = -1.5, y = -0.03, label = "a + delta", parse=T, colour = "dark green") +
  # alphas
  annotate("text", x = -5, y = 0.125, label = "alpha[a]", parse=T, colour = "dark green") +
  annotate("text", x = -5, y = 0.95, label = "alpha[b]", parse=T, colour = "dark green") +
  # betas
  annotate("text", x = -5, y = 0.05, label = "beta[a]", parse=T, colour = "dark red") +
  annotate("text", x = -5, y = 0.875, label = "beta[b]", parse=T, colour = "dark red") +
  # x axis labels
  theme_minimal(base_family = 'mono', base_size = 18) +
  scale_x_continuous(limits=c(-5,5), breaks=c(-2.5, 0, 2.5), labels=c(expression(mu < tau), 
                                                                      expression(mu == tau), 
                                                                      expression(mu > tau)))

#ggsave("plots/sensitivity_theory.png", width = 6, height = 5,  dpi = 300)

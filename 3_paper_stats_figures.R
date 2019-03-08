# The Attention-Aversion Gap
# Tomas Lejarraga, Michael Schulte-Mecklenbeck, Thorsten Pachur, Ralph Hertwig

# statistics and figures for manuscript

# packages
library(tidyverse); library(pBrackets); library(Cairo); library(gridExtra)

# load data from three experiments
  load('data/rawclean.Rdata')
  
# Figure 2. Do losses prompt more visual attention than gains? Aggregate analysis
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
  
# Figure 3. Do losses prompt more visual attention than gains? Individual analysis
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
  
  pos_neg$ratio <- pos_neg$neg/pos_neg$pos
  nrow(subset(pos_neg, type == "Outcome" & ratio > 1))
  nrow(subset(pos_neg, type == "Outcome" & ratio < 1))
  nrow(subset(pos_neg, type == "Probability" & ratio > 1))
  nrow(subset(pos_neg, type == "Probability" & ratio < 1))
  
# Figure 4. Does the degree of loss aversion in choice depend on loss attention?
  
  # load parameter datasets
  m_trial1 <- read.table('data/lambda_thorsten_mouse_trial1.txt', sep="\t", header=TRUE)
  m_trial2 <- read.table('data/lambda_thorsten_mouse_trial2.txt', sep="\t", header=TRUE)
  c_trial1 <- read.table('data/lambda_thorsten_click_trial1.txt', sep="\t", header=TRUE)
  c_trial2 <- read.table('data/lambda_thorsten_click_trial2.txt', sep="\t", header=TRUE)
  
  mouse_lambda <- merge(m_trial1, m_trial2, by = "row")
  click_lambda <- merge(c_trial1, c_trial2, by = "row")
  
  mouse_lambda$mean_lambda <- (mouse_lambda$lambda.x + mouse_lambda$lambda.y)/2
  mouse_lambda <- mouse_lambda[c(1,4)]
  
  click_lambda$mean_lambda <- (click_lambda$lambda.x + click_lambda$lambda.y)/2
  click_lambda <- click_lambda[c(1,4)]
  
  # Get maps of subject ID's from choice data
  m1_ids <- read_delim('CPTModeling/m1_ids.txt', delim = ' ')
  c1_ids <- read_delim('CPTModeling/c1_ids.txt', delim = ' ')
  
  colnames(m1_ids) <- "subject"
  colnames(c1_ids) <- "subject"
  
  mouse_lambda$subject <- m1_ids$subject
  mouse_lambda$condition <- "mouse"
  
  click_lambda$subject <- c1_ids$subject
  click_lambda$condition <- "click"
  
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
  
  open_time <- merge(lambdas, open_time, by = "subject")
  
  # Opening time: counting cases
  la_la <- round(nrow(subset(open_time, line == 1 & mean_lambda > 1))/nrow(open_time), 2)*100 # Loss attention & loss aversion
  la_gs <- round(nrow(subset(open_time, line == 1 & mean_lambda < 1))/nrow(open_time), 2)*100 # Loss attention & gain seeking
  ga_la <- round(nrow(subset(open_time, line == 0 & mean_lambda > 1))/nrow(open_time), 2)*100 # Gain attention & loss aversion
  ga_gs <- round(nrow(subset(open_time, line == 0 & mean_lambda < 1))/nrow(open_time), 2)*100 # Gain attention & gain seeking
  
  ggplot(open_time, aes(asymmetry_box, mean_lambda)) +
    scale_color_manual(values = c("1" = "dark red", "0" = "dark green")) +
    xlab(paste('Relative box opening time')) +
    ylab(expression(paste("Loss aversion (",lambda, ") "))) +
    annotate("rect", xmin=-Inf, xmax=Inf, ymin=1, ymax=Inf, fill = "dark grey", alpha = 0.3) +
    annotate("rect", xmin=1, xmax=Inf, ymin=-Inf, ymax=Inf, fill = "lightblue", alpha = 0.3) +
    annotate("text", x = 2, y = 1.45, label =  paste0(la_la,"%", "\n loss attentive & \n loss averse "), size = 3, family = "mono") +
    annotate("text", x = 2, y = 0.35, label =  paste0(la_gs,"%", "\n loss attentive & \n gain seeking "), size = 3, family = "mono") +
    annotate("text", x = 0.73, y = 1.45, label =  paste0(ga_la,"%", "\n gain attentive & \n loss averse "), size = 3, family = "mono") +
    annotate("text", x = 0.73, y = 0.35, label =  paste0(ga_gs,"%", "\n gain attentive & \n gain seeking "), size = 3, family = "mono") +
    theme_minimal(base_family = 'mono', base_size = 18) +
    geom_smooth(method=lm,se=FALSE, colour = "dark grey", size = 0.5) +
    geom_point(alpha = .7, size = 3, aes(colour = as.factor(line)), show.legend=F)  +
    scale_y_continuous(breaks=c(0.6, 1, 1.4))
  
  ggsave('plots/aversion_attention_open_time.png', width = 6, height = 6,  dpi = 300)
  
  # Count cases
  
  # Loss aversion
  nrow(subset(lambdas, mean_lambda > 1))  # Loss averse
  nrow(subset(lambdas, mean_lambda > 1))/(88*2)
  nrow(subset(lambdas, mean_lambda < 1))  # Gain seeking
  nrow(subset(lambdas, mean_lambda == 1)) # Loss neutral
  
  # Loss attention
  nrow(subset(open_time, line == 1)) # Loss attention
  nrow(subset(open_time, line == 1))/(88*2)
  nrow(subset(open_time, line == 0)) # Gain attention
  nrow(subset(open_time, line == 1 & mean_lambda > 1)) # Loss attention & loss aversion
  nrow(subset(open_time, line == 1 & mean_lambda < 1)) # Loss attention & gain seeking
  nrow(subset(open_time, line == 0 & mean_lambda > 1)) # Gain attention & loss aversion
  nrow(subset(open_time, line == 0 & mean_lambda < 1)) # Gain attention & gain seeking
  
  # Statistics
  round(mean(lambdas$mean_lambda), 2)
  t.test(lambdas$mean_lambda)
  cor.test(open_time$mean_lambda, open_time$asymmetry_box, method = "spearman")
  
# Figure 5. Probability of survival according to McDermott et al. (2008)

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

# Figure 6. Analysis of boxtime openings

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

p3 <- 
  rawclean %>%
  filter(!celltypeF == 'Butt') %>%
  filter(trial == 1) %>%
  filter(task != 7 & task != 14 & task != 82 & task != 83 & task != 84) %>%
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

p4 <- 
  rawclean %>%
  filter(!celltypeF == 'Butt') %>%
  filter(trial == 2) %>%
  filter(task != 7 & task != 14 & task != 82 & task != 83 & task != 84) %>%
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

out_excl <- grid.arrange(p3, p4, nrow = 1)

#ggsave('plots/appendix_gain_loss_excl_comparison.png', out)


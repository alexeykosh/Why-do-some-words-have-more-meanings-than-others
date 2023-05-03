library(tidyverse)
library(ggpubr)
library(DescTools)
library(scales)
library(ppcor)
# remove
library(brms)
library(tidybayes)
library(ggdag)

theme_set(theme_bw())
options(scipen=999) # preventing scientific notation for glue

save <- TRUE # whether to save figures or not

# Initializing the model
S <- 6000 # number of senses
N <- round(S * 0.507) # number of types (calculated with ratio from WordNet)
mu <- 0.01 # replacement rate
k <- 0.02 # reuse rate
p <- 100000 # number of tokens to be sampled
t <- 300 # number of time-steps

# Running the model (this should be uncommented if the reader wants 
# to run the model by himself)
# NB: the N/S ratio is scalled, but we should prob. scale 
# it down according to the size of the corpus
# system2('python3',
#         args = c('src/NeutralModel.py',
#                  str_glue('--N={N}'),
#                  str_glue('--S={S}'),
#                  str_glue('--mu={mu}'),
#                  str_glue('--k={k}'),
#                  str_glue('--p={p}'),
#                  str_glue('--t={t}'))
# )

# Reading the data
data <- read.csv(str_glue('data/model-data/model-N{N}_S{S}_p{p}.csv')) %>%
  filter(extinct != 1) %>% # remove extinct words
  group_by(step) %>% # group by time-step
  mutate(relative_frequency = count / sum(count)) # add relative frequency

# Figure 1: Rank-frequency distribution of words in the last time-step 
# of the model
log_sc <- data %>%
  filter(step == t-1) %>%
  mutate(rank=row_number(desc(relative_frequency))) %>%
  ggplot(aes(x=rank, y=log(relative_frequency)))+
  geom_point(alpha=0.5)+
  xlab('Frequency rank')+
  ylab(' Relative frequency\n (log-odds)')+
  ggtitle('Log scale')+
  theme(text = element_text(size = 20))+
  scale_x_continuous(trans = log2_trans())

lin_sc <- data %>%
  filter(step == t-1) %>%
  mutate(rank=row_number(desc(relative_frequency))) %>%
  ggplot(aes(x=rank, y=relative_frequency))+
  geom_point(alpha=0.5)+
  xlab('Frequency rank')+
  ylab('Relative frequency')+
  ggtitle('Linear scale')+
  theme(text = element_text(size = 20))

ggarrange(log_sc, lin_sc, 
          widths = c(1, 1), 
          ncol = 1, 
          nrow =2,
          align = "hv")+
  theme(text = element_text(size = 20))
if (isTRUE(save)){
  ggsave('figures/Fig1.pdf', width = 10, height = 6)
}

# Figure 2: The evolution of number of types and senses.
## Sums for each times-step
sum_step <- data %>% 
  filter(extinct != 1) %>%
  group_by(step) %>%
  summarize(step = step, 
            sum_m = sum(meanings), 
            n = n(), 
            sum_c = sum(count), 
            share = n/sum_m)
## Plotting
sum_step %>%
  ggplot(aes(x=step, y=sum_m))+
  geom_path(aes(x=step, y=sum_m), color='grey', size=2)+
  geom_hline(yintercept = S, color='grey', size=2, alpha=0.5)+
  geom_path(aes(x=step, y=n), color='blue', size=2)+
  geom_hline(yintercept = N, color='blue', size=2, alpha=0.5)+
  xlab('Time-step (t)')+
  ylab('Count')+
  scale_color_manual(values = colors)+
  theme(text = element_text(size = 20))
if (isTRUE(save)){
  ggsave('figures/Fig2.pdf', width = 10, height = 4)
}

# Figure 3: Age ~ number of meanings vs. frequency ~ number of meanings 
freq_meanings <- data %>% 
  filter(step == t-1 & extinct != 1) %>%
  mutate(age=age)%>%
  ggplot(aes(x=age, y=meanings))+
  geom_point(aes(color=relative_frequency), alpha=0.5)+
  stat_cor(method = "spearman",
           aes(label = after_stat(r.label)))+
  xlab('Longevity')+
  ylab('Number of meanings')+
  labs(color='Frequency')+
  theme(text = element_text(size = 20),
        axis.text = element_text(size=10))+
  scale_colour_gradient(low = "blue", high = "red")
age_meanings <- data %>% 
  filter(step == t-1  & extinct != 1) %>%
  mutate(log_f=relative_frequency) %>%
  ggplot(aes(x=log_f, y=meanings))+
  geom_point(aes(color=age), alpha=0.5)+
  stat_cor(method = "spearman", 
           aes(label = after_stat(r.label)))+
  xlab('Relative frequency')+
  ylab('Number of meanings')+
  labs(color='Longevity')+
  theme(text = element_text(size = 20),
        axis.text= element_text(size=10))+
  scale_colour_gradient(low = "blue", high = "red")+
  scale_x_continuous(trans = log2_trans(), 
                     labels = function(x) format(round(x, 3), scientific = TRUE))
ggarrange(freq_meanings, age_meanings, 
          widths = c(1, 1), 
          ncol = 2, 
          nrow =1,
          align = "hv",
          labels = c("A", "B"))+
  theme(text = element_text(size = 20))
 if (isTRUE(save)){
  ggsave('figures/Fig3.png', width = 10, height = 6)
}

# Correlation coefficients
last_step <- data %>% 
  filter(step == t-1 & extinct != 1)  %>% 
  mutate(z_age = (age - mean(age))/sd(age), 
         z_freq = (relative_frequency - 
                     mean(relative_frequency))/sd(relative_frequency), 
         z_meanings = (meanings - mean(meanings))/sd(meanings))
## age~meaning
cor.test(x=last_step$meanings, 
         y=last_step$age,
         method = 'spearman')
## longevity~frequency
cor.test(x=last_step$relative_frequency, 
         y=last_step$age,
         method = 'spearman')
## controlling for longevity
pcor.test(x=last_step$meanings, 
          y=last_step$relative_frequency,
          z=last_step$age,
          method = 'spearman')
## frequency~meaning
cor.test(x=last_step$meanings, 
         y=last_step$relative_frequency,
         method = 'spearman')
## controlling for frequency
pcor.test(x=last_step$meanings, 
          y=last_step$age,
          z=last_step$relative_frequency,
          method = 'spearman')

# Figure 4
sample_bd <- data %>%
  filter(step != 0) %>%
  group_by(id)  %>%
  mutate(birth=min(step), death=max(step),
         frequency=max(relative_frequency)) %>%
  dplyr::select(birth, death, age) %>%
  arrange(desc(age))

# Figure 4: plotting the intervals
sample_bd[sample.int(10000:10000*30, 40),] %>%
  arrange(desc(age)) %>%
  ggplot()+
  geom_point(aes(y=id, x=birth), color='red')+
  geom_point(aes(y=id, x=death), color='blue')+
  geom_segment(aes(x=birth, xend=death, y=id, yend=id),colour = "grey50")+
  theme(axis.text.y=element_blank(), #remove x axis labels
        axis.ticks.y=element_blank())+
  xlab('Step')+
  ylab('')+
  geom_text(aes(x=320, y=id, label=age))
if (isTRUE(save)){
  ggsave('figures/Fig4_int.png', width = 10, height = 6)
}

# Bayesian poisson regression
## Meanings ~ frequency + age
mod1 <- brm(meanings ~ 1 + z_freq + z_age, data = last_step, family = poisson)
summary(mod1)
### plots
p_m1 <- posterior_samples(mod1) %>%
  dplyr::select(b_z_age, b_z_freq) %>%
  rename('Longevity' = 'b_z_age',
         ' Relative\n frequency' = 'b_z_freq') %>%
  gather() %>%
  ggplot(aes(x = value, y = reorder(key, value))) + 
    geom_vline(xintercept = 0, color = "firebrick4", alpha = 3/10) +
    stat_pointinterval(point_interval = mode_hdi, .width = .95, 
                        size = 3/4, color = "firebrick4") +
    theme_bw() +
    theme(panel.grid   = element_blank(),
          panel.grid.major.y = element_line(color = alpha("firebrick4", 1/4), 
                                            linetype = 3),
          axis.text.y  = element_text(hjust = 0),
          axis.ticks.y = element_blank())+
    xlab('beta coefficient')+
    ylab('')+
    xlim(-0.2, 1)
## Meanings ~ frequency
mod2 <- brm(meanings ~ 1 + z_freq, data = last_step, family = poisson)
summary(mod2)

p_m2 <- posterior_samples(mod2) %>%
  dplyr::select(b_z_freq) %>%
  rename(' Relative\n frequency' = 'b_z_freq') %>%
  gather() %>%
  ggplot(aes(x = value, y = reorder(key, value))) + 
  geom_vline(xintercept = 0, color = "firebrick4", alpha = 3/10) +
  stat_pointinterval(point_interval = mode_hdi, .width = .95, 
                     size = 3/4, color = "firebrick4") +
  theme_bw() +
  theme(panel.grid   = element_blank(),
        panel.grid.major.y = element_line(color = alpha("firebrick4", 1/4),
                                          linetype = 3),
        axis.text.y  = element_text(hjust = 0),
        axis.ticks.y = element_blank())+
  xlab('beta coefficient')+
  ylab('')+
  xlim(-0.2, 1)
## Meanings ~ age
mod3 <- brm(meanings ~ 1 + z_age, data = last_step, family = poisson)
summary(mod3)
### Plot
p_m3 <- posterior_samples(mod3) %>%
  dplyr::select(b_z_age) %>%
  rename('Longevity' = 'b_z_age') %>%
  gather() %>%
  ggplot(aes(x = value, y = reorder(key, value))) + 
  geom_vline(xintercept = 0, color = "firebrick4", alpha = 3/10) +
  stat_pointinterval(point_interval = mode_hdi, .width = .95, 
                     size = 3/4, color = "firebrick4") +
  theme_bw() +
  theme(panel.grid   = element_blank(),
        panel.grid.major.y = element_line(color = alpha("firebrick4", 1/4), 
                                          linetype = 3),
        axis.text.y  = element_text(hjust = 0),
        axis.ticks.y = element_blank())+
  xlab('beta coefficient')+
  ylab('')+
  xlim(-0.2, 1)
ggarrange(p_m1, p_m2, p_m3, 
          widths = c(1, 1), 
          ncol = 1, 
          nrow = 3,
          align = "hv",
          labels = c("A", "B", "C"))
if (isTRUE(save)){
  ggsave('figures/Fig1_p.pdf', width = 10, height = 6)
}

# # Drawing a DAG (frequency as a confound)
# meaning_dag <- dagify(polysemy ~ longevity, 
#                       longevity ~ frequency,
#                       polysemy ~ frequency,
#                       labels = c(
#                         'polysemy' = 'Polysemy',
#                         'longevity' = 'Longevity',
#                         'frequency' = 'Frequency'
#                       ))
# ggdag(meaning_dag,
#       text = FALSE, 
#       use_labels = "label")+
#   theme_void()

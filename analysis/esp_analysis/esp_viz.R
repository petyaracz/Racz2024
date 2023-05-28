# -- header -- #

set.seed(1337)

setwd('~/Github/Racz2024/')
library(tidyverse)
library(glue)
library(gghalves)

options(MC.cores=parallel::detectCores())

d1 = read_tsv('exp_data/esp/esp_master_lakok.tsv')
d2 = read_tsv('exp_data/esp/esp_master_cselekszik.tsv')
b = read_tsv('exp_data/baseline/baseline_tidy_proc.tsv')  

# -- wrangling -- #

d1$part_yob = as.double(d1$part_yob)

d = bind_rows(d1,d2)

d = b %>% 
  select(base,log_odds,derivational,nsyl) %>% 
  rename(baseline_log_odds = log_odds) %>% 
  right_join(d)

posttest = d %>% 
  filter(trial_kind == 'posttest trial') %>% 
  mutate(
    derivational = fct_relevel(derivational, '-szik'),
    two_syl = nsyl == 2,
    reg_rate = fct_relevel(reg_rate, 'high'),
    reg_dist = fct_relevel(reg_dist, 'typical')
  )

# -- fun -- #

logOdds = . %>% 
  pivot_wider(names_from = picked_v1, values_from = n, values_fill = 0) %>% 
  mutate(log_odds = log((`TRUE`+1)/(`FALSE`+1)))

cloudPlot = function(dat,var){
  dat %>% 
  ggplot(aes({{var}},log_odds)) +
  geom_hline(yintercept = 0) +
  geom_half_violin(side = 'r') +
  geom_half_boxplot(width = .1, side = 'r') +
  geom_half_point(width = .25, side = 'l') +
  theme_bw() +
  coord_flip()
}
# -- viz -- #

# fit2
# reg_ratelow                               -1.1    0.2  
# reg_distreversed                          -0.2    0.2  
# baseline_log_odds_jitter                   1.1    0.1  
# variationlakok/lakom                       0.8    0.2  
# reg_distreversed:baseline_log_odds_jitter -0.2    0.1 

posttest %>% 
  count(base,reg_dist,baseline_log_odds,variation,picked_v1) %>% 
  logOdds %>% 
  ggplot(aes(baseline_log_odds,log_odds,colour = variation)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw() +
  facet_wrap( ~ reg_dist)

posttest %>% 
  count(base,reg_dist,baseline_log_odds,picked_v1) %>% 
  logOdds %>% 
  ggplot(aes(baseline_log_odds,log_odds,colour = reg_dist,label = base)) +
  geom_label() +
  geom_smooth(method = 'lm') +
  theme_bw() +
  scale_colour_grey()

posttest %>% 
  count(part_id,reg_rate,picked_v1) %>% 
  logOdds %>% 
  cloudPlot(reg_rate)

posttest %>% 
  count(part_id,variation,picked_v1) %>% 
  logOdds %>% 
  cloudPlot(variation)

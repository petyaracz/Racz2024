# this is the power analysis for the pre-registration.

library(tidyverse)
library(glue)
library(magrittr)
library(ggthemes)

library(lme4)
library(broom.mixed)

# we go back to the experiment that sort of had a similar structure and look at variance across groups.

# d = read_csv('~/Github/RaczBecknerHayPierrehumbert2019/data/convergence_paper_esp_test2_predictions.txt')

# distros = d %>% 
  # filter(resp_post_reg == 1) %>% 
  # count(participant_id,lex_typicality,reg_rate, name = 'P')

# distros %>%   
  # ggplot(aes(P)) +
  # geom_histogram() +
  # theme_few() +
  # facet_wrap( ~ reg_rate + lex_typicality )
# sqrt[ n * P * ( 1 - P ) ].

# whatever we'll just go with the normal distribution
# distros %>% 
#   group_by(reg_rate,lex_typicality) %>% 
#   summarise(
#     median = median(P),
#     n = n(),
#     mad = mad(P),
#     lower3 = median - 3 * mad,
#     upper3 = median + 3 * mad,
#     lower2.5 = median - 2.5 * mad,
#     upper2.5 = median + 2.5 * mad
#   )

# now we simulate something.

my_mean = 25 # mean for no change condition, very clever
my_sd_small = 8 # small variance given data
my_sd_large = 16 # large variance
small_effect = my_sd_small / 2 # I made up some effect sizes
mid_effect = my_sd_small # This is realistic, maybe even a bit understated given data
large_effect = my_sd_large # You wish!
sample_size = 30

beerMat = function(sample_size, effect_size, my_mean, my_sd){
  
  # for progress bar
  pb$tick()$print()
  
  # distribution of participant probabilities
  p_high = rnorm(sample_size, mean = ( my_mean + effect_size ) / 50, sd = my_sd / 50)
  p_low = rnorm(sample_size, mean = ( my_mean - effect_size ) / 50, sd = my_sd / 50)
  
  # generate tibbles of participants for high and low group
  t_high = tibble(
    id = glue('id{1:sample_size}'),
    p = p_high,
    group = 'high'
  )
  t_low = tibble(
    id = glue('id{1:sample_size}'),
    p = p_low,
    group = 'low'
  )
  
  # build answers based on participant p-s
  t_high %<>% 
    rowwise() %>% 
    mutate(
      resp = map(p, ~ rbinom(50, 1, .))
    )
  t_low %<>% 
    rowwise() %>% 
    mutate(
      resp = map(p, ~ rbinom(50, 1, .))
    )
  t = bind_rows(t_high,t_low)
  
  t %<>%
    unnest(resp)
  
  # fit glmm
  fit = glmer(resp ~ 1 + group + ( 1 | id ), family = binomial(link = 'logit'), data = t)
  
  # grab stats
  tidy(fit, conf.int = T) %>% 
    filter(term != 'sd__(Intercept)') %>% 
    select(term,statistic,p.value,conf.low,conf.high)
  
}

# smallish effect, small sample
pb = progress_estimated(100)
sim1 = map_df(1:100, ~ beerMat(20,4,25,4))
ggplot(sim1, aes(statistic)) +
  geom_histogram() +
  theme_bw() +
  geom_vline(xintercept = c(-2,2)) +
  facet_wrap( ~ term)

# smallish effect, larger sample
pb = progress_estimated(100)
sim2 = map_df(1:100, ~ beerMat(25,4,25,4))
ggplot(sim2, aes(statistic)) +
  geom_histogram() +
  theme_bw() +
  geom_vline(xintercept = c(-2,2)) +
  facet_wrap( ~ term)

# smallish effect, larger sample, larger spread
pb = progress_estimated(100)
sim3 = map_df(1:100, ~ beerMat(25,4,25,8))
ggplot(sim3, aes(statistic)) +
  geom_histogram() +
  theme_bw() +
  geom_vline(xintercept = c(-2,2)) +
  facet_wrap( ~ term)

# -- header -- #

set.seed(1337)

setwd('~/Github/Racz2024/')
library(tidyverse)
library(magrittr)
library(glue)
library(ggthemes)
library(gghalves)


# -- fun -- #

logOdds = function(dat,var){
  dat %>% 
    pivot_wider(names_from = {{var}}, values_from = n, values_fill = 0) %>% 
    mutate(log_odds = log((`TRUE`+1)/(`FALSE`+1)))
} 

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

newIndex = . %>% 
  arrange(trial_index) %>% 
  group_by(dat_id) %>% 
  mutate(i = 1:n()) %>% 
  ungroup()

# -- source -- #

source('analysis/esp_analysis/source_esp.R')

# -- wranglë -- #

esp %<>% 
  newIndex()

posttest %<>% 
  newIndex()

# -- viz -- #

b %>% 
  filter(variation != 'hotelban/hotelben') %>% 
  distinct(base,variation,log_odds) %>% 
  ggplot(aes(log_odds,variation)) +
  geom_vline(xintercept = 0) +
  geom_boxplot() +
  theme_bw()

# Baseline responses to nonce words center around zero. The lakok variation is clearly more poised towards + vs - than the cselekszenek variation.

# People don't just pick words at random in the baseline. Let's assume two models, where one predicts responses based only on participant mean. The other based on participant mean and word mean.

# participant weight
b0 %<>% 
  count(file_name,resp_is_first_variant) %>% 
  logOdds(resp_is_first_variant) %>% 
  select(file_name,log_odds) %>% 
  rename(participant_weight = log_odds) %>% 
  left_join(b0)

# word weight
b0 %<>% 
  count(word,resp_is_first_variant) %>% 
  logOdds(resp_is_first_variant) %>% 
  select(word,log_odds) %>% 
  rename(word_weight = log_odds) %>% 
  left_join(b0)

# two models
b0 %<>% 
  mutate(combined_weight = participant_weight + word_weight)

b0 %>% 
  ggplot(aes(combined_weight,as.double(resp_is_first_variant))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  theme_bw()

b0 %>% 
  ggplot(aes(participant_weight,as.double(resp_is_first_variant))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  theme_bw()

# The combined weight is a better. If people picked words at random, word weight would not be informative. 
# Uh I'm not sure this is true. I think so?


esp %>% 
  filter(i %in% 12:54) %>% # burn-in
  mutate(
    reg_rate = fct_relevel(reg_rate, 'low'),
    reg_dist = fct_relevel(reg_dist, 'reversed')
         ) %>%
  count(esp_match,reg_rate,reg_dist,variation,part_id) %>% 
  logOdds(esp_match) %>% 
  ggplot(aes(log_odds,reg_rate,fill = reg_dist)) +
  geom_vline(xintercept = 0) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap( ~ variation, ncol = 2)

# We look at the log odds of matching the co-player in the ESP phase. We only check the last 80% of the ESP phase to skip "burn-in".
# For lakok, obvious distinctions across the four categories. For cselekszenek, much less pronounced.

esp %>% 
  ggplot(aes(i,as.double(esp_match), colour = interaction(reg_rate,reg_dist))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  theme_bw() +
  facet_wrap( ~ variation, ncol = 2) +
  scale_colour_colorblind()

# Learning trajectories are also different. For lakok, there is little learning in low typical, lot of learning in low reversed, the other two in between (with different intercepts). This is in part due to how much people like variant 1 in the first place (as seen in the baseline distributions). But the typical / reversed difference says that lexical distributions also matter: people have a lexical distribution they need to converge away from. For cselekszenek, there is some learning going on, but it is smeared overall. Low typical seems like the hardest category to learn, combined with the fact that people are already slightly good at it.

esp %>% 
  ggplot(aes(i,as.double(picked_v1), colour = interaction(reg_rate,reg_dist))) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  theme_bw() +
  facet_wrap( ~ variation, ncol = 2) +
  scale_colour_colorblind()

# Differences across variation are apparent in what people converge to. For lakok, reaching the target set by reg rate (lots of variant 1 or few variant 1) is made harder by reg distribution. For cselekszenek, this isn't the case.

esp %>% 
  filter(i %in% 12:54) %>% # burn-in
  count(base,baseline_log_odds,reg_dist,variation,picked_v1) %>% 
  logOdds(picked_v1) %>% 
  ggplot(aes(baseline_log_odds,log_odds,colour = reg_dist)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  theme_bw() +
  facet_wrap( ~ variation)

# Distributions matter. For prompt words, correlation w/ baseline log odds gets worse with a reversed distribution, but only in lakok.

posttest %>% 
  mutate(
    reg_rate = fct_relevel(reg_rate, 'low'),
    reg_dist = fct_relevel(reg_dist, 'reversed')
  ) %>%
  count(picked_v1,reg_rate,variation,part_id) %>% 
  logOdds(picked_v1) %>% 
  ggplot(aes(log_odds,reg_rate)) +
  geom_vline(xintercept = 0) +
  geom_boxplot() +
  theme_bw()# +
  # facet_wrap( ~ variation, ncol = 2)

# The reg rate effects persist for the post test. There's no real difference here across variation or reg dist.

# However, the lexical distributions still matter.

posttest %>% 
  ggplot(aes(baseline_log_odds,as.double(picked_v1),colour = reg_dist)) +
  # geom_point(alpha = .1) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  theme_bw() +
  facet_wrap( ~ variation)

# We've seen that, in esp, the rate of convergence to the desired outcome (low or high rate of use for variant 1) is mediated by reg dist. The use of a verb in esp correlates with its use in the baseline. This is less true for the reversed distribution, where participant expectations are overwritten. But this is only happening in the lakok category, possible because there were relatively few participant expectations for cselekszenek in the first place. We see the same thing in the posttest.
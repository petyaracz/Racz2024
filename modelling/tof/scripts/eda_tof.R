
setwd('~/Github/Racz2024')

library(tidyverse)
library(ggthemes)
library(glue)
library(patchwork)

d = read_tsv('modelling/tof/dat/esp_master_all_filtered_rules.tsv')

# -- 1-dim -- #

d %>% 
  distinct(rule,variation,rule_type,adjusted_reliability) %>% 
  ggplot(aes(adjusted_reliability)) +
  geom_histogram() +
  facet_wrap( ~ variation + rule_type) +
  theme_bw()

d %>% 
  distinct(rule,variation,rule_type,hits) %>% 
  ggplot(aes(hits)) +
  geom_histogram() +
  facet_wrap( ~ variation + rule_type) +
  theme_bw()

# -- 2-dim -- #

p0 = d %>% 
  distinct(rule,variation,rule_type,log_odds_rule_weighted,adjusted_reliability) %>% 
  ggplot(aes(adjusted_reliability,log_odds_rule_weighted,colour = rule_type)) +
  geom_point() +
  geom_smooth() +
  facet_wrap( ~ variation) +
  theme_bw() +
  scale_colour_colorblind()

p1 = d %>% 
  filter(trial_kind == 'posttest trial') %>% 
  mutate(
    reg = glue('{reg_rate} {reg_dist}') %>% 
      factor(levels = c(
        'high typical', 'high reversed', 'low reversed', 'low typical'
      ))
         ) %>% 
  count(rule,rule_type,variation,log_odds_rule_weighted,adjusted_reliability,reg,picked_v1) %>% 
  pivot_wider(names_from = picked_v1, values_from = n, values_fill = 0) %>% 
  mutate(
    log_odds_exp = case_when(
      rule_type == 'rule1' ~ log(`TRUE`/`FALSE`),
      rule_type == 'rule2' ~ log(`FALSE`/`TRUE`)
         )
    ) %>% 
  ggplot(aes(adjusted_reliability,log_odds_exp,colour = reg)) +
  geom_point(alpha = .5) +
  geom_smooth() +
  theme_bw() +
  facet_wrap( ~ variation, ncol = 2) +
  scale_colour_colorblind()

p0 / p1
# the best rules are not necessarily the ones with the highest adjusted reliability
# specific rules move around: not the best / worst ones necessarily.
# rules move around across reg rate, not so much reg dist, which is interesting.

p1 + facet_wrap( ~ rule_type + variation)

p3 = d %>% 
  distinct(rule,variation,rule_type,log_odds_rule_weighted,impugned_lower_confidence_limit) %>% 
  ggplot(aes(impugned_lower_confidence_limit,log_odds_rule_weighted,colour = rule_type)) +
  geom_point() +
  geom_smooth() +
  facet_wrap( ~ variation) +
  theme_bw() +
  scale_colour_colorblind()

p4 = d %>% 
  filter(trial_kind == 'posttest trial') %>% 
  mutate(
    reg = glue('{reg_rate} {reg_dist}') %>% 
      factor(levels = c(
        'high typical', 'high reversed', 'low reversed', 'low typical'
      ))
  ) %>% 
  count(rule,rule_type,variation,log_odds_rule_weighted,impugned_lower_confidence_limit,reg,picked_v1) %>% 
  pivot_wider(names_from = picked_v1, values_from = n, values_fill = 0) %>% 
  mutate(log_odds_exp = log(`TRUE`/`FALSE`)) %>% 
  ggplot(aes(impugned_lower_confidence_limit,log_odds_exp,colour = reg)) +
  geom_point(alpha = .5) +
  geom_smooth() +
  theme_bw() +
  facet_wrap( ~ variation, ncol = 2) +
  scale_colour_colorblind()

p3 / p4

# if we sort rules by impugned reliability, the best ones are less good. but this is hardly a surprise and might be all explained by how rule impugnment works.

# -- find the best rules -- #

best_baseline = d %>% 
  distinct(rule,variation,rule_type,log_odds_rule_weighted,adjusted_reliability,impugned_lower_confidence_limit) %>% 
  arrange(-log_odds_rule_weighted) %>% 
  group_by(variation) %>% 
  slice_head(n = 3)

worst_baseline = d %>% 
  distinct(rule,variation,rule_type,log_odds_rule_weighted,adjusted_reliability,impugned_lower_confidence_limit) %>% 
  arrange(log_odds_rule_weighted) %>% 
  group_by(variation) %>% 
  slice_tail(n = 3)

# walk these through the esp phase

d %>% 
  filter(trial_kind == 'esp trial') %>% 
  mutate(
    reg = glue('{reg_rate} {reg_dist}') %>% 
      factor(levels = c(
        'high typical', 'high reversed', 'low reversed', 'low typical'
      ))
  ) %>%
  inner_join(best_baseline) %>% 
  count(rule,rule_type,variation,reg,trial_index,picked_v1) %>% 
  pivot_wider(names_from = picked_v1, values_from = n, values_fill = 0) %>% 
  mutate(
    p_1 = case_when(
      is.na(`TRUE`) ~ 0,
      is.na(`FALSE`) ~ 1,
      !is.na(`TRUE`) & !is.na(`FALSE`) ~ `TRUE` / (`TRUE` + `FALSE`)
      ),
    p = case_when(
      rule_type == 'rule1' ~ p_1,
      rule_type == 'rule2' ~ 1 - p_1
    )
  ) %>% 
  ggplot(aes(trial_index,p,colour = rule)) +
  geom_line(alpha = .5) +
  geom_smooth() +
  theme_bw() +
  scale_colour_colorblind() +
  facet_wrap( ~ variation + reg, ncol = 4)

# oh boy oh boy
# well that was silly.
# we need to pool rules somehow.
# first, we try rules that apply to many forms. some of these are alright.

largest_baseline = d %>% 
  distinct(rule,variation,rule_type,log_odds_rule_weighted,adjusted_reliability,impugned_lower_confidence_limit,scope) %>% 
  arrange(-scope,-adjusted_reliability,-log_odds_rule_weighted) %>% 
  group_by(variation) %>% 
  slice_head(n = 3)

# walk THESE through the esp phase

d %>% 
  filter(trial_kind == 'esp trial') %>% 
  mutate(
    reg = glue('{reg_rate} {reg_dist}') %>% 
      factor(levels = c(
        'high typical', 'high reversed', 'low reversed', 'low typical'
      ))
  ) %>%
  inner_join(largest_baseline) %>% 
  count(rule,rule_type,variation,reg,trial_index,picked_v1) %>% 
  pivot_wider(names_from = picked_v1, values_from = n, values_fill = 0) %>% 
  mutate(
    p_1 = case_when(
      is.na(`TRUE`) ~ 0,
      is.na(`FALSE`) ~ 1,
      !is.na(`TRUE`) & !is.na(`FALSE`) ~ `TRUE` / (`TRUE` + `FALSE`)
    ),
    p = case_when(
      rule_type == 'rule1' ~ p_1,
      rule_type == 'rule2' ~ 1 - p_1
    )
  ) %>% 
  ggplot(aes(trial_index,p,colour = rule)) +
  geom_line(alpha = .5) +
  geom_smooth() +
  theme_bw() +
  scale_colour_colorblind() +
  facet_wrap( ~ variation + reg, ncol = 4)

# this is the way.
# but I need sg more consistent than plotting every rule against trial index in the esp or whatever

# pick rules or pool rules. or both.
# does any of this say anything beyond what the stimuli end up looking like cross-cut w/ the picks of the co-player?
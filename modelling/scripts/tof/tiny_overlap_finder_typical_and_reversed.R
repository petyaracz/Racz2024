# spaghetti two: comparing tiny overlaps across the cogsci paper data
# funny if cogsci rejects it and I have to find a new name. oh well
# -- head -- #

setwd('~/Github/Racz2024')

library(tidyverse)
library(magrittr)
library(glue)
library(ggthemes)
library(patchwork)

# -- fun -- #

# convert s posttests to look like b
countBase = . %>% 
  count(base,variation,suffix,picked_v1) %>% 
  pivot_wider(names_from = picked_v1, values_from = n) %>% 
  rename(resp1 = `TRUE`, resp2 = `FALSE`) %>% 
  mutate(log_odds = log(resp1/resp2))

# cross the word counts from baseline, posttest, 2nd posttest w/ r and get relevant rules
crossRules = . %>% 
  # change intersecting colnames, add suffix to lakok/lakom
  rename(suffix_t = suffix, variation_t = variation) %>% 
  mutate(
    suffix_t = case_when(
      variation_t == 'cselekszenek/cselekednek' ~ suffix_t,
      variation_t == 'lakok/lakom' ~ '3sg'
    )
  ) %>% 
  crossing(r) %>%
  # drop rules where variation and suffix don't match
  filter(
    suffix_t == suffix,
    variation_t == variation
  ) %>% 
  rowwise() %>% 
  filter(
    # drop rules where rule doesn't match word input
    str_detect(input, glue('{input_ending}$'))
  ) %>% 
  mutate(
    # mark if rule marks output1 or 2
    rule_type = case_when(
      str_detect(output1, glue('{output_ending}$')) ~ 'rule1',
      str_detect(output2, glue('{output_ending}$')) ~ 'rule2'
    )
  ) %>% 
  # drop rules where rule doesn't match either word output
  filter(
    !is.na(rule_type)
  ) %>% 
  # ungroup rowwise
  ungroup() %>% 
  # if this is a rule2 type rule, it actually predicts output2, so its success is the prop of output2 over output1. this means that the log odds have to be reversed.
  mutate(
    log_odds_rule_weighted = case_when(
      rule_type == 'rule1' ~ log_odds,
      rule_type == 'rule2' ~ -log_odds,
    )
  )

# count forms per rule
countRules2 = . %>%
  # data already grouped!
  # we count resp1 and resp2 across rules.
  summarise(
    resp1 = sum(resp1),
    resp2 = sum(resp2)
  ) %>% 
  mutate(
    # since we counted resp1 resp2 anew across we reset log odds again as above.
    log_odds_12 = log(resp1/resp2),
    log_odds_rule_weighted = case_when(
      rule_type == 'rule1' ~ log_odds_12,
      rule_type == 'rule2' ~ -log_odds_12,
    )
  ) %>% 
  ungroup()

# -- read -- #

# rules from our sort of MGL (the best one for each var)
r = read_tsv('~/Github/Racz2024b/dat/tof/best_tofs.tsv')
# test forms formatted to pair up with rules
b = read_tsv('~/Github/Racz2024b/dat/training_sets/test_mgl.tsv')
# exp data
d = read_tsv('~/Github/Racz2024/exp_data/esp/esp_master_all_filtered.tsv')

# -- wrangle -- #

# the noun variation wasn't tested in sleep so we remove it
r %<>% filter(variation != 'hotelban/hotelben')
b %<>% filter(variation != 'hotelban/hotelben')

# get posttest resp1/2 counts from s
t = d %>% 
  filter(
    variation != 'hotelban/hotelben',
    trial_kind == 'posttest trial'
         ) %>% 
  group_by(reg_dist) %>% 
  countBase()

# add to rules: what the input form ends in, what the output form ends in, what the suffix is.
r %<>% 
  mutate(
    input_ending = case_when(
      # is.na(A) ~ C, # this never happens
      is.na(C) ~ A,
      !is.na(C) ~ glue('{C}{A}')
    ),
    output_ending = case_when(
      # is.na(B) ~ C, # this never happens either (I checked)
      is.na(C) ~ B,
      !is.na(C) ~ glue('{C}{B}')
    ),
    suffix_string = case_when(
      variation == "cselekszenek/cselekednek" ~ str_extract(B, '(nVk|tVk|Vnk)$'),
      variation == "lakok/lakom" ~ B
    )
  )

# -- set up inputs to rules -- #

## baseline input
b_in = b %>% 
  select(base,resp1,resp2,log_odds,variation,suffix,input,output1,output2)

## test input
t_in = b_in %>% 
  select(base,input,output1,output2) %>% 
  inner_join(t)

# -- add rules to forms -- #

# this takes a sec
b_rules = b_in %>% 
  crossRules()

t_rules = t_in %>%
  crossRules()

# -- count hits per rule -- #

b_rule_count = b_rules %>% 
  group_by(variation,suffix,rule,rule_type,scope,hits,reliability,adjusted_reliability,impugned_lower_confidence_limit) %>% 
  countRules2()

t_rule_count = t_rules %>% 
  group_by(reg_dist,variation,suffix,rule,rule_type,scope,hits,reliability,adjusted_reliability,impugned_lower_confidence_limit) %>% 
  countRules2()

# -- combine -- #

b_rule_count$experiment = 'baseline'
t_rule_count$experiment = t_rule_count$reg_dist

rules_long = bind_rows(b_rule_count,t_rule_count)

b_min = b_rule_count %>% 
  select(variation,rule,scope,hits,reliability,adjusted_reliability,impugned_lower_confidence_limit,rule_type,log_odds_rule_weighted) %>% 
  rename(log_odds_baseline_weighted = log_odds_rule_weighted)

t_t_min = t_rule_count %>% 
  filter(reg_dist == 'typical') %>% 
  select(variation,rule,log_odds_rule_weighted) %>% 
  rename(log_odds_typical_weighted = log_odds_rule_weighted)

t_r_min = t_rule_count %>% 
  filter(reg_dist == 'reversed') %>% 
  select(variation,rule,log_odds_rule_weighted) %>% 
  rename(log_odds_reversed_weighted = log_odds_rule_weighted)

rules_wide_min = b_min %>% 
  inner_join(t_t_min) %>% 
  inner_join(t_r_min) %>% 
  mutate(
    complete_rule = T,
    baseline_typical_diff = (log_odds_baseline_weighted + 10) - (log_odds_typical_weighted + 10),
    baseline_reversed_diff = (log_odds_baseline_weighted + 10) - (log_odds_reversed_weighted + 10),
    typical_reversed_diff = (log_odds_typical_weighted + 10) - (log_odds_reversed_weighted + 10),
         )

rules_long %<>% 
  left_join(rules_wide_min) %>%
  mutate(complete_rule = ifelse(is.na(complete_rule), F, complete_rule))

# -- viz -- #

# scope not helpful
rules_long %>% 
  ggplot(aes(scope,log_odds_rule_weighted,label = rule,colour = variation)) +
  geom_label() +
  theme_bw() +
  facet_wrap( ~ rule_type + experiment) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_colour_colorblind()

# adjusted reliability: weaker rules go in reverse!
rules_long %>% 
  filter(complete_rule) %>% 
  ggplot(aes(adjusted_reliability,log_odds_rule_weighted,label = rule,colour = variation)) +
  geom_label() +
  theme_bw() +
  facet_wrap( ~ rule_type + experiment) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_colour_colorblind()

# something something rule 1 vs 2?

p0 = rules_wide_min %>% 
  ggplot(aes(scope,typical_reversed_diff,colour = rule_type)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw() + 
  facet_wrap( ~ variation)
p1 = rules_wide_min %>% 
  ggplot(aes(hits,typical_reversed_diff,colour = rule_type)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw() + 
  facet_wrap( ~ variation)
p2 = rules_wide_min %>% 
  ggplot(aes(reliability,typical_reversed_diff,colour = rule_type)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw() + 
  facet_wrap( ~ variation)
p3 = rules_wide_min %>% 
  ggplot(aes(adjusted_reliability,typical_reversed_diff,colour = rule_type)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw() + 
  facet_wrap( ~ variation)
p4 = rules_wide_min %>% 
  ggplot(aes(impugned_lower_confidence_limit,typical_reversed_diff,colour = rule_type)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw() + 
  facet_wrap( ~ variation)
p0 / p1 / p2 / p3 / p4

# look at only lakok

p0 = rules_wide_min %>% 
  filter(variation == 'lakok/lakom') %>% 
  ggplot(aes(adjusted_reliability,typical_reversed_diff,colour = rule_type, label = rule)) +
  geom_label() +
  geom_smooth(method = 'lm') +
  theme_bw() +
  guides(colour = 'none') +
  scale_colour_colorblind()

p1 = rules_wide_min %>% 
  filter(variation == 'lakok/lakom') %>% 
  ggplot(aes(impugned_lower_confidence_limit,typical_reversed_diff,colour = rule_type, label = rule)) +
  geom_label() +
  geom_smooth(method = 'lm') +
  theme_bw() +
  guides(colour = 'none') +
  scale_colour_colorblind()

p2 = rules_wide_min %>% 
  filter(variation == 'lakok/lakom') %>% 
  ggplot(aes(adjusted_reliability,baseline_reversed_diff,colour = rule_type, label = rule)) +
  geom_label() +
  geom_smooth(method = 'lm') +
  theme_bw() +
  guides(colour = 'none') +
  scale_colour_colorblind()

p3 = rules_wide_min %>% 
  filter(variation == 'lakok/lakom') %>% 
  ggplot(aes(impugned_lower_confidence_limit,baseline_reversed_diff,colour = rule_type, label = rule)) +
  geom_label() +
  geom_smooth(method = 'lm') +
  theme_bw() +
  guides(colour = 'none') +
  scale_colour_colorblind()

p0 + p1 + p2 + p3

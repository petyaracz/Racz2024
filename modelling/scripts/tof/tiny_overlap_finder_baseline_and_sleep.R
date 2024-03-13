# spaghetti one: comparing tiny overlap finder rule accuracy on the baseline and the sleep datasets
# lesson so far: posttest 2 doesn't do interesting thing to rules, it seems. crying shame! but I might need to be more careful about picking my rules. some rules might be completely daft.
# let's face it: people didn't regress back to the mean after sleep. so rules could have moved around across rule reliability or whatever, but it's not a surprise if they didn't.
# -- head -- #

setwd('~/Github/Racz2024')

library(tidyverse)
library(magrittr)
library(glue)
library(ggthemes)

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
countRules = . %>% 
  # we count resp1 and resp2 across rules.
  group_by(variation,suffix,rule,rule_type,scope,hits,reliability,adjusted_reliability,impugned_lower_confidence_limit) %>% 
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
# sleep data
s = read_tsv('~/Github/Racz2024/exp_data/esp/esp_sleep_pt1_pt2_master_all_filtered.tsv')

# -- tweak -- #

# the noun variation wasn't tested in sleep so we remove it
r %<>% filter(variation != 'hotelban/hotelben')
b %<>% filter(variation != 'hotelban/hotelben')

# get posttest resp1/2 counts from s
t = s %>% 
  filter(trial_kind == 'posttest trial') %>% 
  countBase()
# get 2nd posttest resp1/2 counts from s
t2 = s %>% 
  filter(trial_kind == 'second posttest trial') %>% 
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

## test2 input
t2_in = b_in %>% 
  select(base,input,output1,output2) %>% 
  inner_join(t2)

# -- add rules to forms -- #

# this takes a sec
b_rules = b_in %>% 
  crossRules()

t_rules = t_in %>%
  crossRules()

t2_rules = t2_in %>% 
  crossRules()

# -- count hits per rule -- #

b_rule_count = b_rules %>% 
  countRules()

t_rule_count = t_rules %>% 
  countRules()

t2_rule_count = t2_rules %>% 
  countRules()

# -- combine -- #

b_rule_count$experiment = 'baseline'
t_rule_count$experiment = 'test'
t2_rule_count$experiment = 'test2'

rules_long = bind_rows(b_rule_count,t_rule_count,t2_rule_count)

b_rule_count2 = b_rule_count %>% 
  select(-resp1,-resp2,-log_odds_12) %>% 
  rename(baseline_lo_weighted = log_odds_rule_weighted)

t_rule_count2 = t_rule_count %>% 
  select(variation,suffix,rule,log_odds_rule_weighted) %>% 
  rename(test_lo_weighted = log_odds_rule_weighted)

t2_rule_count2 = t2_rule_count %>% 
  select(variation,suffix,rule,log_odds_rule_weighted) %>% 
  rename(test2_lo_weighted = log_odds_rule_weighted)

rules_wide = b_rule_count2 %>% 
  left_join(t_rule_count2) %>% 
  left_join(t2_rule_count2) %>% 
  mutate(
    baseline_to_test_penalty = (baseline_lo_weighted + 10) - (test_lo_weighted + 10),
    baseline_to_test2_penalty = (baseline_lo_weighted + 10) - (test2_lo_weighted + 10),
    test_to_test2_penalty = (test_lo_weighted + 10) - (test2_lo_weighted + 10)
  )

complete_rules = rules_wide %>% 
  filter(!is.na(baseline_lo_weighted),!is.na(test_lo_weighted),!is.na(test2_lo_weighted)) %>%
  select(variation,suffix,rule) %>% 
  mutate(complete_rule = T)

rules_long %<>% 
  left_join(complete_rules) %>%
  mutate(complete_rule = ifelse(is.na(complete_rule), F, complete_rule))

# -- viz -- #

## rules across experiments ##

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

# various log odds
rules_long %>% 
  filter(complete_rule) %>% 
  mutate(rule = fct_reorder(rule,adjusted_reliability)) %>% 
  ggplot(aes(experiment,log_odds_rule_weighted,label = rule, group = rule, colour = variation)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  facet_wrap( ~ rule) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_colour_colorblind()

# rule penalties in test
rules_wide %>% 
  ggplot(aes(adjusted_reliability,baseline_to_test_penalty)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw() +
  facet_wrap( ~ rule_type)

with(rules_wide, cor.test(adjusted_reliability,baseline_to_test_penalty))

rules_wide %>% 
  ggplot(aes(reliability,baseline_to_test_penalty)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw()

with(rules_wide, cor.test(reliability,baseline_to_test_penalty))

rules_wide %>% 
  ggplot(aes(impugned_lower_confidence_limit,baseline_to_test_penalty)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw()

with(rules_wide, cor.test(impugned_lower_confidence_limit,baseline_to_test_penalty))

rules_wide %>% 
  ggplot(aes(adjusted_reliability,baseline_to_test2_penalty)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw() +
  facet_wrap( ~ rule_type)

with(rules_wide, cor.test(adjusted_reliability,baseline_to_test2_penalty))

rules_wide %>% 
  ggplot(aes(adjusted_reliability,test_to_test2_penalty)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  theme_bw() +
  facet_wrap( ~ rule_type)

with(rules_wide, cor.test(adjusted_reliability,test_to_test2_penalty))

# everything #

penalties = rules_wide %>% 
  select(rule,variation,baseline_to_test_penalty,baseline_to_test2_penalty,test_to_test2_penalty)

rules_long %>% 
  filter(complete_rule) %>% 
  left_join(penalties) %>% 
  mutate(
    q_adjusted_reliability = ntile(adjusted_reliability, 4),
    reversed_worse = baseline_to_test_penalty > 0
  ) %>% 
  ggplot(aes(experiment,log_odds_rule_weighted,group = rule)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap( ~ reversed_worse + q_adjusted_reliability, nrow = 2)

# only tops rules #
quantile(rules_wide$adjusted_reliability)
quantile(rules_wide$scope)

rules_wide_tops = rules_wide %>% 
  filter(adjusted_reliability > .8) %>% 
  mutate(
    scope_large = scope > 8
  )

quantile(rules_wide_tops$scope)

rules_long_tops = rules_long %>% 
  filter(adjusted_reliability > .8) %>% 
  mutate(
    scope_large = scope > 8
  )

rules_long_tops %>% 
  filter(complete_rule) %>% 
  ggplot(aes(experiment,log_odds_rule_weighted,label = rule,group = rule)) +
  # geom_label() +
  geom_point() +
  geom_line() +
  theme_bw() +
  facet_wrap( ~ scope_large)

rules_wide_tops %>% 
  ggplot(aes(adjusted_reliability,baseline_to_test_penalty)) +
  geom_point() +
  geom_smooth(method = 'lm')

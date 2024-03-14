# we take rules from mgl/tof trained on real words (see Racz2024b), combine them with nonce words, get rule accuracy from baseline study, then cross everything with esp/test data from main experiment to get a very long dataset. write to file.
# -- head -- #

setwd('~/Github/Racz2024')

library(tidyverse)
library(magrittr)
library(glue)
library(ggthemes)
library(patchwork)

# -- fun -- #

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

# the noun variation acts weird so we drop it
r %<>% filter(variation != 'hotelban/hotelben')
b %<>% filter(variation != 'hotelban/hotelben')
d %<>% filter(variation != 'hotelban/hotelben')

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

# -- give each rule a baseline accuracy using the baseline data -- #

## baseline input
b_in = b %>% 
  select(base,resp1,resp2,log_odds,variation,suffix,input,output1,output2)

# cross with rules, keep relevant rules
rules = b_in %>% 
  crossRules()

# for each rule, count resp1 and resp2 across words that rule applies to. calc log odds. if the rule made resp2, the relevant measure is -log_odds.
rules %<>% 
  select(-log_odds,-log_odds_rule_weighted) %>% 
  group_by(across(-c(resp1,resp2))) %>% 
  summarise(
    resp1 = sum(resp1),
    resp2 = sum(resp2)
  ) %>% 
  mutate(
    log_odds = log(resp1/resp2),
    baseline_log_odds_rule = case_when(
      rule_type == 'rule1' ~ log_odds,
      rule_type == 'rule2' ~ -log_odds,
    )
  ) %>% 
  ungroup() %>% 
  select(-log_odds)

with(rules, cor.test(reliability,baseline_log_odds_rule))
with(rules, cor.test(adjusted_reliability,baseline_log_odds_rule))
with(rules, cor.test(impugned_lower_confidence_limit,baseline_log_odds_rule))
# yup.

# -- add to long data -- #

intersect(names(rules), names(d))
# yup.

# take d add necessary cols for crossing with rules
## test input
d2 = b_in %>% 
  select(base,input,output1,output2) %>% 
  inner_join(d)

# takes a while:
fulld = d2 %>% 
  crossRules()

# check fulld
fulld %>% 
  count(rule,variation,reg_rate,reg_dist)
fulld %>% 
  count(part_id,trial_kind)
fulld

# -- write -- #

write_tsv(fulld, 'modelling/tof/dat/esp_master_all_filtered_rules.tsv')

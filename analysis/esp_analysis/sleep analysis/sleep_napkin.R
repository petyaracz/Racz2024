library(tidyverse)

setwd('~/Github/Racz2024')

d = read_tsv('exp_data/esp/esp_master_sleep_pt1_all_unfiltered.tsv')

b = read_tsv('exp_data/baseline/baseline_tidy_proc.tsv')

d = b %>% 
  select(base,log_odds) %>% 
  rename(baseline_log_odds = log_odds) %>% 
  left_join(d)

d %>% 
  filter(trial_kind == 'posttest trial') %>% 
  count(base,baseline_log_odds,picked_v1) %>% 
  pivot_wider(names_from = picked_v1, values_from = n, values_fill = 0) %>% 
  mutate(post_test_log_odds = (`TRUE` + 1) / (`FALSE` + 1)) %>% 
  ggplot(aes(baseline_log_odds,post_test_log_odds)) +
  geom_point() +
  geom_smooth() +
  theme_bw()

d %>% 
  filter(trial_kind == 'posttest trial') %>% 
  count(part_id,picked_v1) %>% 
  pivot_wider(names_from = picked_v1, values_from = n, values_fill = 0) %>% 
  mutate(post_test_log_odds = (`TRUE` + 1) / (`FALSE` + 1)) %>% 
  ggplot(aes(post_test_log_odds)) +
  geom_histogram() +
  theme_bw()

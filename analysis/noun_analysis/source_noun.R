# -- header -- #

setwd('~/Github/Racz2024')

set.seed(1337)

options(MC.cores=parallel::detectCores())

# -- fun -- #

newIndex = . %>% 
  arrange(trial_index) %>% 
  group_by(dat_id) %>% 
  mutate(i = 1:n()) %>% 
  ungroup()

# -- read -- #

d = read_tsv('exp_data/esp/esp_master_all_filtered.tsv')
b = read_tsv('exp_data/baseline/baseline_tidy_proc.tsv')  

# -- wrangling -- #

b %<>% 
  mutate(baseline_p = resp1 / (resp1+resp2)) %>% 
  filter(variation == 'hotelban/hotelben')

d = b %>% 
  select(base,log_odds,baseline_p,derivational,nsyl) %>% 
  rename(baseline_log_odds = log_odds) %>% 
  right_join(d) %>% 
  mutate(
    baseline_log_odds_jitter = jitter(baseline_log_odds, factor = .0001),
    derivational = fct_relevel(derivational, '-szik'),
    two_syl = nsyl == 2,
    reg_rate = fct_relevel(reg_rate, 'high'),
    reg_dist = fct_relevel(reg_dist, 'typical'),
    variation = fct_relevel(variation, 'lakok/lakom')
  ) %>% 
  filter(variation == 'hotelban/hotelben')

posttest = d %>% 
  filter(trial_kind == 'posttest trial') %>% 
  newIndex

esp = d %>% 
  filter(trial_kind == 'esp trial') %>% 
  newIndex

esp$abs_baseline_log_odds_jitter = abs(esp$baseline_log_odds_jitter)

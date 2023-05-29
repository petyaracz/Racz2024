# -- header -- #

set.seed(1337)

options(MC.cores=parallel::detectCores())

d1 = read_tsv('exp_data/esp/esp_master_lakok.tsv')
d2 = read_tsv('exp_data/esp/esp_master_cselekszik.tsv')
b = read_tsv('exp_data/baseline/baseline_tidy_proc.tsv')  
b0 = read_tsv('exp_data/baseline/baseline_tidy.tsv')

# -- wrangling -- #

d1$part_yob = as.double(d1$part_yob)
print("don't worry about it.")

b %<>% 
  mutate(baseline_p = resp1 / (resp1+resp2))

d = bind_rows(d1,d2)

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
  )

posttest = d %>% 
  filter(trial_kind == 'posttest trial')

esp = d %>% 
  filter(trial_kind == 'esp trial')

# -- gc -- #

# rm(b)
rm(d1)
rm(d2)
gc()
#############################################
#############################################
# source data, functions, models for ESP / matching game analysis
#############################################
#############################################

# -- header -- #

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
# b0 = read_tsv('exp_data/baseline/baseline_tidy.tsv')
s = read_tsv('exp_data/sleep/sleep.tsv')

# -- wrangling -- #

b %<>% 
  mutate(baseline_p = resp1 / (resp1+resp2))

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
  filter(trial_kind == 'posttest trial') %>% 
  newIndex

esp = d %>% 
  filter(trial_kind == 'esp trial') %>% 
  newIndex

esp = filter(esp, variation != 'hotelban/hotelben')
esp$abs_baseline_log_odds_jitter = abs(esp$baseline_log_odds_jitter)

posttest = filter(posttest, variation != 'hotelban/hotelben')

sesp = filter(s, trial_kind == 'esp trial') %>% 
  mutate(abs_baseline_log_odds = abs(baseline_log_odds))

sposttests = s %>%
  filter(str_detect(trial_kind, 'posttest trial'))

# -- best models -- #

# see esp_analysis, sleep_analysis/sleep_napkin

esp_fit = glmer(as.double(esp_match) ~ 1 + reg_rate + reg_dist * scale(abs_baseline_log_odds_jitter) + variation + scale(i) + (1|part_id) + (1|base), 
                data = esp, 
                family = binomial(link = 'logit'), 
                control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=20000)
                ))

posttest_fit = glmer(as.double(picked_v1) ~ 1 + reg_rate + reg_dist * baseline_log_odds_jitter + variation + (1 + 1|part_id) + (1|base), 
                     data = posttest, 
                     family = binomial(link = 'logit'), 
                     control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=20000)
                     ))

s_esp_fit = glmer(as.double(esp_match) ~ 1 + scale(abs_baseline_log_odds) + variation + scale(i) + (1|part_id) + (1|base), 
             data = sesp, 
             family = binomial(link = 'logit'), 
             control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=20000)
             ))

s_posttests_fit = glmer(as.double(picked_v1) ~ 1 + trial_kind * variation + scale(baseline_log_odds) + (1|part_id) + (1|base), data = sposttests, family = binomial)

# -- camera ready -- #

esp$var = ifelse(esp$variation == 'lakok/lakom', 'levelling','vowel deletion')
posttest$var = ifelse(posttest$variation == 'lakok/lakom', 'levelling','vowel deletion')

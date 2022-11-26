# -- setup -- #

setwd('~/Github/Racz2024/')

library(tidyverse)
library(magrittr)
library(glue)
library(patchwork)

source('scripts/gcm/purrrgcm.R')
source('scripts/gcm/transcribe.R')

# -- data -- #

b = read_tsv('exp_data/baseline/baseline_tidy_proc.tsv')
v = read_tsv('resource/real_words/verb_bag.tsv')
lakok = read_tsv('resource/real_words/ik_verbs/ikes_pairs_webcorpus2.tsv')
cselekszik = read_tsv('resource/real_words/epenthetic_stems/epenthesis_pairs_webcorpus2.tsv')
cselekszik_r = read_tsv('resource/real_words/epenthetic_stems/epenthesis_reference_webcorpus2.tsv')
esp = read_tsv('resource/exp_input_files/esp/esp_master_input.tsv')

# -- a little wrangling -- #

cselekszik_r %<>%
  rename(base = lemma) %>% 
  mutate(base_tr = transcribe(base, 'single'))

# -- baseline -- #

# For lakok verbs, the training set is fairly obviously verbs that end in -ik. For practical purposes, we can narrow this down to verbs that end in -ik and show some 1sg variation.

lakok2 = lakok %>% 
  summarise(
    n14 = quantile(log_odds, probs = .25),
    n34 = quantile(log_odds, probs = .75),
    n13 = quantile(log_odds, probs = .33),
    n23 = quantile(log_odds, probs = .66)
  )

p1 = lakok %>% 
  ggplot(aes(log_odds)) +
  geom_histogram() +
  theme_bw() +
  geom_vline(data = lakok2, aes(xintercept = n14)) +
  geom_vline(data = lakok2, aes(xintercept = n34), lty = 2) +
  geom_vline(data = lakok2, aes(xintercept = n13)) +
  geom_vline(data = lakok2, aes(xintercept = n23), lty = 2)

p2 = lakok %>% 
  mutate(stem = fct_reorder(stem,log_odds)) %>% 
  ggplot(aes(stem,log_odds)) +
  geom_point() +
  theme_void() +
  geom_hline(data = lakok2, aes(yintercept = n14)) +
  geom_hline(data = lakok2, aes(yintercept = n34), lty = 2) +
  geom_hline(data = lakok2, aes(yintercept = n13)) +
  geom_hline(data = lakok2, aes(yintercept = n23), lty = 2)

p1 + p2

# The GCM expects categories, but there aren't any here. We can split the set in two (say, log odds < 0) but then they will be very similar. We can exclude forms in the middle but then we waste comparisons.

lakok2 = lakok %>% 
  bind_cols(lakok2) %>% 
  select(base_tr,log_odds,n14,n34,n13,n23) %>% 
  rename(word = base_tr)
lakokb = b %>% 
  filter(variation == "lakok/lakom") %>% 
  select(base_tr,log_odds) %>% 
  rename(word = base_tr)
lakokb2 = select(lakokb,word)

# We hedge our bets with three cat boundaries and 9 values of s

lakoks = tibble(
  quantiles = c('fifty','thirty','twenty-five'),
  d = c(
    list(
      lakok2 %<>%
        mutate(category = case_when(
          log_odds >= 0 ~ 'k',
          log_odds < 0 ~ 'm'
        )
      ) %>% 
        filter(!is.na(category))
    ),
    list(
      lakok2 %<>%
        mutate(category = case_when(
          log_odds > n23 ~ 'k',
          log_odds < n13 ~ 'm'
        )
      ) %>% 
        filter(!is.na(category))
    ),
    list(
      lakok2 %<>%
        mutate(category = case_when(
          log_odds > n34 ~ 'k',
          log_odds < n14 ~ 'm'
        )
      ) %>% 
        filter(!is.na(category))
    )
  )
) %>% 
  crossing(
    var_s = seq(0.1,0.9,0.1)
  ) %>% 
  mutate(
    model = map2(d, var_s, ~ tidyGCM(training = .x, test = lakokb2, var_s = .y, var_p = 1, distance_metric = 'lv')
    )
)

# Let's take a look.

lakoks %<>% 
  select(quantiles,var_s,model) %>% 
  mutate(
    pred = map(model, ~ left_join(., lakokb, by = 'word')),
    r = map(pred, ~ summarise(., r = cor(k,log_odds))) %>% 
      unlist()
  )

lakokb = lakoks %>% 
  filter(r == max(r)) %>% 
  mutate(baseline_gcm_info = glue('q: {quantiles}, s: {var_s}, r: {round(r,2)}')) %>% 
  select(-model) %>% 
  unnest(cols = c(pred)) %>% 
  select(word,k,baseline_gcm_info) %>% 
  rename(base_tr = word)

# For cselekszik verbs, the training set is either all verbs that end in cc or cvc or only those that vary.

# Since I'm only doing this twice I'm not putting it in a function :P

cselekszik2 = cselekszik %>% 
  group_by(base) %>% 
  summarise(
    log_odds = mean(log_odds)
  ) %>% 
  summarise(
    n14 = quantile(log_odds, probs = .25),
    n34 = quantile(log_odds, probs = .75),
    n13 = quantile(log_odds, probs = .33),
    n23 = quantile(log_odds, probs = .66)
  )

p1 = cselekszik %>% 
  group_by(base) %>% 
  summarise(
    log_odds = mean(log_odds)
  ) %>% 
  ggplot(aes(log_odds)) +
  geom_histogram() +
  theme_bw() +
  geom_vline(data = cselekszik2, aes(xintercept = n14)) +
  geom_vline(data = cselekszik2, aes(xintercept = n34), lty = 2) +
  geom_vline(data = cselekszik2, aes(xintercept = n13)) +
  geom_vline(data = cselekszik2, aes(xintercept = n23), lty = 2)

p2 = cselekszik %>% 
  group_by(base) %>% 
  summarise(
    log_odds = mean(log_odds)
  ) %>% 
  mutate(stem = fct_reorder(stem,log_odds)) %>% 
  ggplot(aes(stem,log_odds)) +
  geom_point() +
  theme_void() +
  geom_hline(data = cselekszik2, aes(yintercept = n14)) +
  geom_hline(data = cselekszik2, aes(yintercept = n34), lty = 2) +
  geom_hline(data = cselekszik2, aes(yintercept = n13)) +
  geom_hline(data = cselekszik2, aes(yintercept = n23), lty = 2)

p1 + p2

# For the variable set, the story is the same as before. Except that (a) we need to calculate the mean log odds for the base forms and (b) we have two possible target sets: cc or cvc. We'll stick with cc for now. I thought about this a bit! It's fine.

cselekszik2 = cselekszik %>% 
  group_by(base_tr) %>% 
  summarise(log_odds = mean(log_odds)) %>% 
  bind_cols(cselekszik2) %>% 
  select(base_tr,log_odds,n14,n34,n13,n23) %>% 
  rename(word = base_tr)
cselekszikb = b %>% 
  filter(variation == "cselekszenek/cselekednek") %>% 
  select(base_tr,log_odds) %>% 
  mutate(word = str_extract(base_tr, '^[^ ]*(?= )'))
cselekszikb2 = select(cselekszikb,word)

cselekszik2 %<>% ungroup() 

# We start with the variable forms. We hedge our bets with three cat boundaries and 9 values of s.

cseleksziks = tibble(
  quantiles = c('fifty','thirty','twenty-five'),
  d = c(
    list(
      cselekszik2 %<>%
        mutate(category = case_when(
          log_odds >= 0 ~ 'cc',
          log_odds < 0 ~ 'cvc'
        )
        ) %>% 
        filter(!is.na(category))
    ),
    list(
      cselekszik2 %<>%
        mutate(category = case_when(
          log_odds > n23 ~ 'cc',
          log_odds < n13 ~ 'cvc'
        )
        ) %>% 
        filter(!is.na(category))
    ),
    list(
      cselekszik2 %<>%
        mutate(category = case_when(
          log_odds > n34 ~ 'cc',
          log_odds < n14 ~ 'cvc'
        )
        ) %>% 
        filter(!is.na(category))
    )
  )
) %>% 
  crossing(
    var_s = seq(0.1,0.9,0.1)
  ) %>% 
  mutate(
    model = map2(d, var_s, ~ tidyGCM(training = .x, test = cselekszikb2, var_s = .y, var_p = 1, distance_metric = 'lv')
    )
  )

# Let's take a look.

cseleksziks %<>% 
  select(quantiles,var_s,model) %>% 
  mutate(
    pred = map(model, ~ left_join(., cselekszikb, by = 'word')),
    r = map(pred, ~ summarise(., r = cor(cc,log_odds))) %>% 
      unlist()
  )

cselekszikb_narrow = cseleksziks %>% 
  filter(r == max(r)) %>% 
  mutate(baseline_gcm_info = glue('q: {quantiles}, s: {var_s}, type: variable, r: {round(r,2)}')) %>% 
  select(-model) %>% 
  unnest(cols = c(pred)) %>% 
  select(word,cc,baseline_gcm_info) %>% 
  rename(base_tr = word)

# So this isn't very good?

# We move on to stable forms.

cselekszik_r2 = cselekszik_r %>% 
  select(base_tr,category) %>% 
  rename(word = base_tr)

count(cselekszik_r2,category) # this will be fun

cseleksziks2 = tibble(
  d = list(cselekszik_r2)
) %>% 
  crossing(
    var_s = seq(0.1,0.9,0.1)
  ) %>% 
  mutate(
    model = map2(d, var_s, ~ tidyGCM(training = .x, test = cselekszikb2, var_s = .y, var_p = 1, distance_metric = 'lv')
    )
  )

# Let's take a look.

cseleksziks2 %<>% 
  select(var_s,model) %>% 
  mutate(
    pred = map(model, ~ left_join(., cselekszikb, by = 'word')),
    r = map(pred, ~ summarise(., r = cor(cc,log_odds))) %>% 
      unlist()
  )

cselekszikb_wide = cseleksziks2 %>% 
  filter(r == max(r)) %>% 
  mutate(baseline_gcm_info = glue('q: NA, s: {var_s}, type: all -ik, r: {round(r,2)}')) %>% 
  select(-model) %>% 
  unnest(cols = c(pred)) %>% 
  select(word,cc,baseline_gcm_info) %>% 
  rename(base_tr = word)

# We'll uh take this.

# Anyway.
lakokb %<>% 
  rename(cat1_weight = k)
cselekszikb_wide %<>% 
  rename(cat1_weight = cc,
         base_tr_cc = base_tr
         )
b_lakok = b %>% 
  inner_join(lakokb)
b_cselekszik = b %>% 
  mutate(
    base_tr_cc = str_extract(base_tr, '^[^ ]*(?= )')
  ) %>%
  inner_join(cselekszikb_wide) %>% 
  select(-base_tr_cc)

b2 = bind_rows(b_lakok,b_cselekszik)

# --- esp --- #

## lakok

lakok_training = lakok2 %>%
  mutate(category = case_when(
    log_odds > n23 ~ 'k',
    log_odds < n13 ~ 'm'
    )
  ) %>% 
  filter(!is.na(category)) %>% 
  select(word,category)

lakok_esp = esp %>% 
  filter(variation == 'lakok/lakom') %>% 
  mutate(
    category = ifelse(esp_response == variant1, 'k', 'm'),
    word = transcribe(base, 'single')
    ) %>% 
  select(word,category,file_name,list_number) %>% 
  nest(-c(file_name,list_number))

# to quote the hesp script:
# rollPostTest = function(stimuli_num){
#   if (stimuli_num < 4) {
#     stimuli_post_test_num = 4;
#   } else if (stimuli_num > 3 & stimuli_num < 8) {
#     stimuli_post_test_num = 8;
#   } else {
#     stimuli_post_test_num = 1;
#   }
# return(stimuli_post_test_num)  
# }

l4 = lakok_esp %>% 
  filter(list_number == 4) %>% 
  unnest(cols = data) %>% 
  select(word)
l8 = lakok_esp %>% 
  filter(list_number == 8) %>% 
  unnest(cols = data) %>% 
  select(word)
l1 = lakok_esp %>% 
  filter(list_number == 1) %>% 
  unnest(cols = data) %>% 
  select(word)

lakok_esp %<>% 
  mutate(
    etest = case_when(
      list_number %in% 0:3 ~ list(l4),
      list_number %in% 4:7 ~ list(l8),
      list_number %in% 8:11 ~ list(l1)
      ),
    etraining = map(data, ~ bind_rows(.,lakok_training))
    )

lakok_esp %<>% 
  mutate(
    model = map2(etraining, etest, ~ tidyGCM(training = .x, test = .y, var_s = .9, var_p = 1, distance_metric = 'lv')) 
  )

lakok_esp %<>% 
  unnest(cols = model) %>% 
  rename(cat1_weight = k) %>% 
  select(file_name,list_number,word,cat1_weight) 
  
## cselekszik

cselekszik_training = cselekszik_r2

cselekszik_esp = esp %>% 
  filter(variation == "cselekszenek/cselekednek") %>% 
  mutate(
    category = ifelse(esp_response == variant1, 'cc', 'cvc'),
    word = transcribe(base, 'single') %>% 
      str_extract('^[^ ]*(?= )')
  ) %>% 
  select(word,category,file_name,list_number) %>% 
  nest(-c(file_name,list_number))

# to quote the hesp script:
# rollPostTest = function(stimuli_num){
#   if (stimuli_num < 4) {
#     stimuli_post_test_num = 4;
#   } else if (stimuli_num > 3 & stimuli_num < 8) {
#     stimuli_post_test_num = 8;
#   } else {
#     stimuli_post_test_num = 1;
#   }
# return(stimuli_post_test_num)  
# }

l4 = cselekszik_esp %>% 
  filter(list_number == 4) %>% 
  unnest(cols = data) %>% 
  select(word)
l8 = cselekszik_esp %>% 
  filter(list_number == 8) %>% 
  unnest(cols = data) %>% 
  select(word)
l1 = cselekszik_esp %>% 
  filter(list_number == 1) %>% 
  unnest(cols = data) %>% 
  select(word)

cselekszik_esp %<>% 
  mutate(
    etest = case_when(
      list_number %in% 0:3 ~ list(l4),
      list_number %in% 4:7 ~ list(l8),
      list_number %in% 8:11 ~ list(l1)
    ),
    etraining = map(data, ~ bind_rows(.,cselekszik_training))
  )

cselekszik_esp %<>% 
  mutate(
    model = map2(etraining, etest, ~ tidyGCM(training = .x, test = .y, var_s = .9, var_p = 1, distance_metric = 'lv')) 
  )

cselekszik_esp %<>% 
  unnest(cols = model) %>% 
  rename(cat1_weight = cc) %>% 
  select(file_name,list_number,word,cat1_weight) 

# Anyway

lakok_esp %<>% 
  mutate(base = transcribe(word, 'double')) %>% 
  select(-word)
csmatcher = esp %>% 
  filter(variation == 'cselekszenek/cselekednek') %>% 
  mutate(base_1 = str_extract(base, '^[^ ]*(?= )')) %>% 
  select(base,base_1)
cselekszik_esp %<>% 
  mutate(base_1 = transcribe(word, 'double')) %>%
  left_join(csmatcher) %>%
  select(-base_1,-word)
esp2 = bind_rows(lakok_esp,cselekszik_esp)  

# -- thanks bye! -- #

b2 %>% 
  write_tsv('resource/gcm/baseline_with_weights.tsv')
esp2 %>% 
  write_tsv('resource/gcm/esp_with_weights.tsv')

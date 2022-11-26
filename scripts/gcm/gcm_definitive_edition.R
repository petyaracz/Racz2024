# train the edit distance GCM on the corpus (various constellations) fit on baseline data, train best model on corpus + esp, fit on posttest data
# -- setup -- #

setwd('~/Github/Racz2024/')

library(tidyverse)
library(magrittr)
library(glue)
library(patchwork)

source('scripts/gcm/purrrgcm.R')
source('scripts/gcm/transcribe.R')
source('scripts/gcm/gcm_functions.R')

# -- data -- #

b = read_tsv('exp_data/baseline/baseline_tidy_proc.tsv')
esp = read_tsv('resource/exp_input_files/esp/esp_master_input.tsv')

lakok = read_tsv('resource/real_words/ik_verbs/ikes_pairs_webcorpus2.tsv')
cselekszenek = read_tsv('resource/real_words/epenthetic_stems/epenthesis_pairs_webcorpus2.tsv')
cselekszenek_r = read_tsv('resource/real_words/epenthetic_stems/epenthesis_reference_webcorpus2.tsv')

# -- wrangling -- #

## test sets

b %<>%
  mutate(
    word = str_extract(base_tr, '^[^ ]*(?= |$)')
  )

baseline_test_lakok = b %>% 
  filter(variation == 'lakok/lakom') %>% 
  select(word)

baseline_test_cselekszenek = b %>% 
  filter(variation == 'cselekszenek/cselekednek') %>% 
  select(word)

## training sets

lakok %<>% 
  addQ() %>% 
  rename(word = base_tr)

cselekszenek2 = cselekszenek %>%
  group_by(base_tr) %>% 
  summarise(
    freq_1 = sum(freq_1),
    freq_2 = sum(freq_2),
    log_odds = log((freq_1 + 1 )/(freq_2 + 1))
  ) %>% 
  addQ() %>% 
  rename(word = base_tr) %>% 
  ungroup()
  
cselekszenek_r %<>% 
  mutate(
    word = transcribe(lemma, 'single'),
    category = case_when(
      category == 'cc' ~ 'cat_1',
      category == 'cvc' ~ 'cat_2',
    )
  ) %>% 
  select(word,category)

cselekszenek_quantiles = cselekszenek2 %>% 
  distinct(n14,n34,n13,n23)

cselekszenek3 = cselekszenek_r %>% 
  mutate(log_odds = case_when(
    category == 'cat_1' ~ 999,
    category == 'cat_2' ~ -999
    )
  ) %>% 
  crossing(cselekszenek_quantiles) %>% 
  bind_rows(cselekszenek2) %>% 
  select(-base_tr,-freq_1,-freq_2)

# -- baseline -- #

# For lakok verbs, the training set is fairly obviously verbs that end in -ik. For practical purposes, we can narrow this down to verbs that end in -ik and show some 1sg variation.

# The GCM expects categories, but there aren't any here. We can split the set in two (say, log odds < 0) but then they will be very similar. We can exclude forms in the middle but then we waste comparisons.

# We hedge our bets with three cat boundaries and 9 values of s
lakoks = iterateBaselineGCM(training_data = lakok, test_data = baseline_test_lakok)

# Let's take a look.

b_lakok = b %>% 
  filter(variation == 'lakok/lakom')

b_lakok = mineGCMtibble(lakoks,b_lakok)

# For cselekszik verbs, the training set is either all verbs that end in cc or cvc or only those that vary. or both!

# For the variable set, the story is the same as before. Except that (a) we need to calculate the mean log odds for the base forms and (b) we have two possible target sets: cc or cvc. We'll stick with cc for now. I thought about this a bit! It's fine.

# We start with the variable forms. We hedge our bets with three cat boundaries and 9 values of s.

cselekszeneks = iterateBaselineGCM(training_data = cselekszenek2, test_data = baseline_test_cselekszenek)

# Let's take a look.

b_cselekszenek = b %>% 
  filter(variation == 'cselekszenek/cselekednek')

b_cselekszenek_1 = mineGCMtibble(cselekszeneks,b_cselekszenek)

# So this isn't very good?

# We move on to stable forms.

cselekszeneks2 = tibble(
  training_col = list(cselekszenek_r)
) %>% 
  crossing(
    var_s = seq(0.1,0.9,0.1)
  ) %>% 
  mutate(
    model = map2(training_col, var_s, ~ tidyGCM(training = .x, test = b_cselekszenek, var_s = .y, var_p = 1, distance_metric = 'lv')
    )
  )

# Let's take a look.

b_cselekszenek_2 = cselekszeneks2 %>% 
  mutate(quantiles = NA) %>% 
  mineGCMtibble(b_cselekszenek)

# Yes this is better but not very good.

# Let's combine the two training sets.

cselekszeneks3 = iterateBaselineGCM(training_data = cselekszenek3, test_data = baseline_test_cselekszenek)

# Let's take a look.

b_cselekszenek_3 = mineGCMtibble(cselekszeneks3,b_cselekszenek)

# Nice.

# --- esp --- #

## lakok

b_lakok

# I should output this from the function but I like to live dangerously
lakok_training = lakok %>%
  mutate(category = case_when(
    log_odds > n23 ~ 'cat_1',
    log_odds < n13 ~ 'cat_2'
    )
  ) %>% 
  filter(!is.na(category)) %>% 
  select(word,category)

lakok_esp = grabESPtraining('lakok/lakom')
  
lakok_training

lakok_esp = iterateESPGCM(esp_dat = lakok_esp, training_dat = lakok_training, my_s = .9)

## cselekszik

b_cselekszenek_3
cselekszenek3

# I should output this from the function but I like to live dangerously
cselekszenek_training = cselekszenek3 %>%
  mutate(category = case_when(
    log_odds >= 0 ~ 'cat_1',
    log_odds < 0 ~ 'cat_2'
  )
  ) %>% 
  filter(!is.na(category)) %>% 
  select(word,category)

cselekszenek_esp = grabESPtraining('cselekszenek/cselekednek')

cselekszenek_esp = iterateESPGCM(esp_dat = cselekszenek_esp, training_dat = cselekszenek_training, my_s = .9)

# -- write-out -- #

all_baseline = bind_rows(b_lakok,b_cselekszenek_3)
all_esp = bind_rows(lakok_esp,cselekszenek_esp)

write_tsv(all_baseline, 'resource/gcm/baseline_with_weights.tsv')
write_tsv(all_esp, 'resource/gcm/esp_with_weights.tsv')

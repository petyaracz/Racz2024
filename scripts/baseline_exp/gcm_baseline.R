####################################
# GCM
####################################

# -- header -- #

setwd('~/Github/Racz2024')

set.seed(1989)

library(tidyverse)
library(glue)
library(furrr)
library(broom)

plan(multisession, workers = 8)

# -- functions -- #

source('scripts/build_words/build_nonce_list/build_nonce_list_functions.R')

GCMwrapper = function(variation_type,lower_bound,dat1,dat2,var_s,var_p){
  if(nrow(dat1[dat1$variation == variation_type,]) == 0){
    print('You mispelt the category label')
    break
  }
  
  training = dat1 %>% 
    filter(variation == variation_type) %>%
    rename(word = base_tr) %>% 
    group_by(word,stem,lemma_freq_corrected) %>% 
    summarise(
      freq_1 = sum(freq_1),
      freq_2 = sum(freq_2),
      log_odds = log( ( freq_1 + 1 ) / ( freq_2 + 1 ) ),
    ) %>% 
    filter(
      freq_1 > lower_bound, 
      freq_2 > lower_bound, 
      !is.na(log_odds)
    ) %>% 
    arrange(log_odds) %>% 
    ungroup() %>%
    mutate(
      quantile_rank = ntile(log_odds,3),
      category = case_when(
        quantile_rank == 1 ~ 'tilt_freq2',
        quantile_rank == 3 ~ 'tilt_freq1'
      )
    ) %>%
    select(word,category)
  
  target = r2 %>% 
    filter(variation == variation_type) %>%
    mutate(word = transcribe(base, 'single')) %>% 
    select(word,base,log_odds)
  
  n_gcm_real = furGCM(training,target, var_s = var_s, var_p = var_p, distance_metric = 'lv')
  
  result = inner_join(target,n_gcm_real, by = 'word')
  return(result)
}

# -- read -- #

s = read_tsv('resource/real_words/all_pairs_webcorpus2.tsv')
r2 = read_tsv('exp_data/baseline/baseline_tidy_proc.tsv')

# -- gcm -- #

## nouns: nonce vs real
nres = GCMwrapper('hotelban/hotelben',10,s,r2,.9,1)
## eszek: nonce vs real
lakokres = GCMwrapper('lakom/lakok',10,s,r2,.9,1)
## cselekedik: nocne vs real
cvcres = GCMwrapper('cselekszenek/cselekednek',10,s,r2,.9,1)

## merge back
r2 = bind_rows(nres,lakokres,cvcres) %>% 
  rename(cat1_weight = tilt_freq1) %>% 
  select(base,cat1_weight) %>% 
  right_join(r2, by = 'base')

# -- viz -- #

r2 %>% 
  ggplot(aes(cat1_weight,log_odds)) +
  geom_point(alpha = .1) +
  geom_smooth(method = 'lm') +
  theme_bw() +
  facet_wrap( ~ variation )

# -- model -- #

dzsungelban = filter(r2, variation == 'hotelban/hotelben')
lakok = filter(r2, variation == 'lakom/lakok')
cselekszik = filter(r2, variation == 'cselekszenek/cselekednek')

dzsungelban %<>% mutate(stem_vowel = str_extract(base, '[aáeéiíoóöőuúüű]'))

fit1 = glm(
  cbind(target2_resp,target1_resp) ~ 1 + cat1_weight + stem_vowel + vowel + suffix,
  data = dzsungelban,
  family = binomial(link = 'logit')
           )
tidy(fit1)

fit2 = glm(
  cbind(target2_resp,target1_resp) ~ 1 + cat1_weight + nsyl + derivational,
  data = lakok,
  family = binomial(link = 'logit')
)
tidy(fit2)

fit3 = glm(
  cbind(target2_resp,target1_resp) ~ 1 + cat1_weight + nsyl + derivational + suffix,
  data = cselekszik,
  family = binomial(link = 'logit')
)
tidy(fit3)


# create stimuli for the hesp experiment

library(tidyverse)
library(ggthemes)

set.seed(1337)

# -- fun -- #

buildMaster = function(dat){
  dat %>% 
    mutate(prompt = glue('{carrier_sentence} {target_sentence}')) %>% 
    select(base,log_odds,variation,prompt,variant1,variant2,nsyl,suffix) %>% 
    arrange(-log_odds) %>%
    mutate(
      list_number = rep(1:3,54),
      word_rank = sort(rep(1:54,3)),
      top_20 = word_rank %in% 1:15,
      bottom_20 = word_rank %in% 40:54
    ) %>% 
    crossing(
      reg_rate = c('low','high'),
      reg_dist = c('typical','reversed'),
    ) %>% 
    rowwise() %>% 
    mutate(
      # high, typ: !bottom_20, var1. 
      #bottom_20, var2
      # low, typ: top_20, var1. 
      #!top_20, var2
      # high, rev: !bottom_20,var2. 
      #bottom_20,var1
      # low, rev: top_20, var2. 
      #!top_20 var1
      esp_response = case_when(
        reg_rate == 'high' & reg_dist == 'typical' & !bottom_20 ~ variant1,
        reg_rate == 'high' & reg_dist == 'typical' & bottom_20 ~ variant2,
        reg_rate == 'low' & reg_dist == 'typical' & top_20 ~ variant1,
        reg_rate == 'low' & reg_dist == 'typical' & !top_20 ~ variant2,
        reg_rate == 'high' & reg_dist == 'reversed' & bottom_20 ~ variant1,
        reg_rate == 'high' & reg_dist == 'reversed' & !bottom_20 ~ variant2,
        reg_rate == 'low' & reg_dist == 'reversed' & !top_20 ~ variant1,
        reg_rate == 'low' & reg_dist == 'reversed' & top_20 ~ variant2
      ),
      target_words = list(c(variant1,variant2))
    )
}

checkBuild = function(dat,list_number){
  dat %>% 
    filter(list_number == list_number) %>% 
    mutate(
      word_quantile = case_when(
        word_rank %in% 1:15 ~ 'top',
        word_rank %in% 16:39 ~ 'middle',
        word_rank %in% 40:54 ~ 'bottom'
      )
    ) %>% 
    ggplot(aes(word_rank,log_odds,label = esp_response, colour = word_quantile)) +
    geom_text() +
    facet_wrap( ~ reg_rate + reg_dist, ncol = 2) +
    theme_classic()
}

# -- dat -- #

s = read_tsv('exp_data/baseline/baseline_tidy_proc.tsv')
cvc = filter(s, variation == 'cselekszenek/cselekednek')
ik = filter(s, variation == 'lakok/lakom')

# -- build list -- #

ik_m = buildMaster(ik)
cvc_m = buildMaster(cvc)

master = bind_rows(ik_m,cvc_m) %>%
  mutate(file_name = glue('{str_extract(variation, "^.*(?=/)")}_{list_number}_{reg_rate}_{reg_dist}'))
  

# -- write out -- #

# tsv

master %>% 
  select(-target_words) %>% 
  write_tsv('resource/exp_input_files/esp/esp_master_input.tsv')

# json

master %>% 
  select(file_name,word_rank,reg_rate,reg_dist,variation,prompt,target_words,esp_response) %>% 
  arrange(file_name,word_rank) %>% 
  nest(-file_name) %>% 
  pwalk(~ write_json(x = .y, path = glue('resource/exp_input_files/esp/jsons/{.x}.json')) )
  

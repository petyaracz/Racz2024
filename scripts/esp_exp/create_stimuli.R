# create stimuli for the hesp experiment
setwd('~/Github/Racz2024')
library(tidyverse)
library(magrittr)
library(ggthemes)
library(glue)
library(jsonlite)

set.seed(1337)

# -- fun -- #

buildMaster = function(dat){
  dat %>% 
    mutate(prompt = glue('{carrier_sentence} {target_sentence}')) %>% 
    select(base,log_odds,variation,prompt,variant1,variant2,nsyl,suffix) %>% 
    arrange(-log_odds) %>%
    mutate(
      sample_number = rep(1:3,54),
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
      # high typ: bottom 20 is variant2, rest is variant1
      # low typ: top 20 is variant1, rest is variant2
      # high rev: top 20 is variant2, rest is variant1
      # low rev: bottom 20 is variant1, rest is variant1
      esp_response = case_when(
        reg_rate == 'high' & reg_dist == 'typical' & bottom_20 ~ variant2,
        reg_rate == 'high' & reg_dist == 'typical' & !bottom_20 ~ variant1,
        reg_rate == 'low' & reg_dist == 'typical' & top_20 ~ variant1,
        reg_rate == 'low' & reg_dist == 'typical' & !top_20 ~ variant2,
        reg_rate == 'high' & reg_dist == 'reversed' & top_20 ~ variant2,
        reg_rate == 'high' & reg_dist == 'reversed' & !top_20 ~ variant1,
        reg_rate == 'low' & reg_dist == 'reversed' & bottom_20 ~ variant1,
        reg_rate == 'low' & reg_dist == 'reversed' & !bottom_20 ~ variant2
      ),
      target_words = list(c(variant1,variant2))
    )
}

checkBuild = function(dat,sample_number){
  dat %>% 
    filter(sample_number == sample_number) %>% 
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
  mutate(file_name = glue('{str_extract(variation, "^.*(?=/)")}_{sample_number}_{reg_rate}_{reg_dist}'))
  
# -- order, thin -- #

master %<>% 
  ungroup() %>% 
  arrange(file_name,word_rank) %>% 
  distinct(file_name,variation) %>% 
  mutate(column = 1:24) %>% 
  group_by(variation) %>% 
  mutate(list_number = 0:11) %>% 
  ungroup() %>% 
  left_join(master, by = c("variation", "file_name"))

# -- write out -- #

# tsv

master %>% 
  select(-target_words) %>% 
  write_tsv('resource/exp_input_files/esp/esp_master_input.tsv')

# json

master %>% 
  select(column,file_name,list_number,word_rank,prompt,target_words,esp_response) %>% 
  nest(-column) %>% 
  pwalk(~ write_json(x = .y, path = glue('resource/exp_input_files/esp/jsons/{.x}.json')) )
  

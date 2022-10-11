# check hesp data
setwd('~/Github/Racz2024')
library(tidyverse)
library(ggthemes)
library(glue)

# -- in -- #

d = read_tsv('exp_data/esp/esp_master.tsv')

flag1 = d %>% 
  filter(trial_kind == 'esp trial',esp_v1) %>% 
  distinct(list_number,reg_rate,reg_dist,file_name,word_rank,variant1,esp_response,esp_v1,log_odds) %>%
  group_by(reg_rate,reg_dist,list_number) %>% 
  summarise(
    word_ranks = glue('{min(word_rank)}:{max(word_rank)}')
  ) %>% 
  mutate(
    flag = 
      reg_rate == 'high' & reg_dist == 'typical' & word_ranks == '1:39' |
      reg_rate == 'low' & reg_dist == 'typical' & word_ranks == '1:15' |
      reg_rate == 'high' & reg_dist == 'reversed' & word_ranks == '16:54' |
      reg_rate == 'low' & reg_dist == 'reversed' & word_ranks == '40:54' 
  ) %>% 
  pull(flag) %>% 
  any()

flag2 = d %>% 
  filter(trial_kind == 'esp trial') %>% 
  count(part_id,list_number,reg_rate,reg_dist,esp_v1) %>% 
  mutate(flag = n == 15 | n == 39) %>% 
  pull(flag) %>% 
  any()
  
flag3 = d %>% 
  count(dat_id,part_id) %>% 
  arrange(part_id) %>% 
  mutate(flag = n == 108) %>% 
  pull(flag) %>% 
  any()

if(all(flag1,flag2,flag3)){print('Checks completed successfully.')}else{print('Ruh-roh.')}
  

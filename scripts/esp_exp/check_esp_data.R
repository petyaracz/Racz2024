# check hesp data
setwd('~/Github/Racz2024')
library(tidyverse)
library(ggthemes)
library(glue)
library(knitr)

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

nmissing = d %>% 
  mutate(
    missing = is.na(response_string)
  ) %>% 
  filter(missing) %>% 
  nrow()

if(nmissing == 0){print('No data missing.')}else{print('Data are missing.')}

ncareless = d %>% 
  filter(picked_left) %>% 
  count(part_id,trial_kind) %>% 
  filter(n > 40) %>% 
  nrow()

if(ncareless == 0){print('No careless participants.')}else{print('Some participants were careless.')}

# slow people

nslow = d %>% 
  group_by(part_id) %>%
  slice(1,108) %>%
  mutate(nt = 1:2) %>% 
  select(part_id,nt,time_elapsed) %>% 
  pivot_wider(id_cols=part_id, names_from = nt, values_from = time_elapsed) %>% 
  mutate(m_elapsed = (`2`-`1`) / 1000 / 60) %>% 
  filter(m_elapsed > 25) %>% 
  nrow()

if(nslow == 0){print('No participants over 25min.')}else{print('Some participants over 25min.')}

d %>% 
  filter(trial_kind == 'esp trial') %>% 
  distinct(part_id,list_number,reg_rate,reg_dist) %>% 
  count(list_number,reg_rate,reg_dist) %>% 
  kable(caption = 'List counts.')

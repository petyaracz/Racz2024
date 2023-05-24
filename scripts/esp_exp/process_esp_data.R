# l117 !
# process hesp data
setwd('~/Github/Racz2024')
library(tidyverse)
library(magrittr)
library(glue)
library(knitr)

# -- fun -- #

procDat = function(dat){
  
  part = dat %>% 
    filter(trial_type == 'survey-text') %>% 
    pull(response)
  
  id = str_extract(part, '(?<=\"Q0\":\").*(?=\",\"Q1)')
  gender = str_extract(part, '(?<=\"Q1\":\").*(?=\",\"Q2)')
  yob = str_extract(part, '(?<=\"Q2\":\").*(?=\",\"Q3)')
  edu = str_extract(part, '(?<=\"Q3\":\").*(?=\")')
  
  gender = ifelse(nchar(gender) == 0, 'nő', gender)
  
  exp_pair = dat %>% 
    filter(trial_kind %in% c('esp trial','posttest trial')) %>%
    mutate(
      trial_order = 1:n(),
      part_id = id,
      part_gender = gender,
      part_yob = yob,
      part_edu = edu,
      choice1 = str_extract(choices, '[a-záéíóőúűöü]+(?=\",)'),
      choice2 = str_extract(choices, '(?<=,\")[a-záéíóőúűöü]+'),
    ) %>%
    select(part_id,part_gender,part_yob,part_edu,list_number,trial_kind,trial_index,stimulus,choice1,choice2,response,response_string,esp_response,esp_match,rt,time_elapsed) %>% 
    group_split(trial_kind)
  
  master = master %>% 
    rename('stimulus' = prompt)
  master2 = master %>% 
    select(-esp_response)
    
  esp = exp_pair[[1]] %>%
    inner_join(master, by = c("list_number", "stimulus", "esp_response"))
  posttest = exp_pair[[2]] %>% 
    inner_join(master2, by = c("list_number", "stimulus")) %>% 
    mutate( # this has to be populated from the esp bit
      reg_rate = NA,
      reg_dist = NA
    )
  
  bind_rows(esp,posttest) %>% 
    mutate(
      picked_left = response_string == choice1,
      picked_v1 = response_string == variant1,
      esp_v1 = esp_response == variant1,
      picked_majority = case_when(
        reg_rate == 'high' ~ picked_v1,
        reg_rate == 'low' ~ !picked_v1
      )
    ) %>% 
    fill(reg_rate) %>% 
    fill(reg_dist)
  
}

# -- helper -- #

master = read_tsv('resource/exp_input_files/esp/esp_master_input.tsv')

# -- dat -- #

dat_id = list.files('~/Github/Pavlovia/hesp/data/')
# dat_id = list.files('resource/exp_output_files/lakok/')
# dat_id = dat_id[str_detect(dat_id, 'ESP-DEMO_SESSION_2022-10-06_2')]
path = '~/Github/Pavlovia/hesp/data/'
# path = 'resource/exp_output_files/lakok/'
# simulated data:
# dat_id = list.files('resource/simulated_hesp/')
# path = 'resource/simulated_hesp/'

d = tibble(
    dat_id = dat_id,
    record_date = str_extract(dat_id, '202[234]-[0-9]{2}-[0-9]{2}_[0-9]{2}h[0-9]{2}') %>% # aah it's another year
      lubridate::ymd_hm()
  ) %>% 
  mutate(
    data = map(dat_id, ~ read_csv(glue('{path}{.}'))),
    n_rows = map(data, ~ nrow(.))
    )

d %<>%
  filter(
    record_date > '2022-11-17',
    n_rows == 176
         )

d %<>% mutate(
    proc = map(data, ~ procDat(.)),
    start = str_extract(dat_id, '2022.*^(?=\\.csv$)')
  ) %>% 
  select(dat_id,proc) %>% 
  unnest(cols = c(proc))

unique(d$dat_id)
unique(d$part_id)
# d[d$part_id == '' & d$part_gender == 'nő' & d$part_yob == 1997 & d$part_edu == 18,]$dat_id
d[d$dat_id == "hungarian-esp_esp_participant_SESSION_2022-11-21_15h30.11.810.csv",]$part_id = 'DAG7T4'

d %<>% filter(
  part_id != 'próba'
)

# write_tsv(d, 'exp_data/esp/esp_master_lakok.tsv')
write_tsv(d, 'exp_data/esp/esp_master_cselekszik.tsv') # !!!

# -- checks -- #

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

# counts

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

# totals

d %>% 
  filter(str_detect(dat_id, 'SESSION\\_2023')) %>% 
  distinct(dat_id,part_id) %>% 
  kable()

d %>% 
  filter(trial_kind == 'esp trial') %>% 
  distinct(part_id,list_number,reg_rate,reg_dist) %>% 
  count(reg_rate,reg_dist) %>% 
  kable(caption = 'Cond counts.')

d %>% 
  filter(trial_kind == 'esp trial') %>% 
  distinct(part_id,list_number,reg_rate,reg_dist) %>% 
  count(list_number,reg_rate,reg_dist) %>% 
  kable(caption = 'List counts.')

n_to_go = 84 - length(unique(d$part_id))

glue('Only {n_to_go} participants to go, brother!')
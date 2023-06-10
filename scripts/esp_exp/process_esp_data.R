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

countKeys = function(dat, file_name, keyboard_input.keys, left, right){
  dat %>% 
    pivot_wider(id_cols = {{file_name}}, names_from = {{keyboard_input.keys}}, values_from = n, values_fill = 0) %>%
    filter({{left}} == 0 | {{right}} == 0)
}

overUpper = function(dat, keyboard_input.rt){
  dat %>% 
    summarise(all_rt = sum({{keyboard_input.rt}})) %>% 
    ungroup() %>% 
    group_by(variation) %>% 
    mutate(
      median_all_rt = median(all_rt),
      mad_all_rt = mad(all_rt),
      upper = median_all_rt + 3 * mad_all_rt,
      over_upper = all_rt > upper
    ) %>% 
    filter(over_upper)
}

whichList = function(dat,l){
  dat %>% 
    group_by(part_id) %>% 
    overUpper(rt) %>% 
    select(part_id) %>% 
    left_join({{l}}) %>%
    filter(trial_kind == 'esp trial') %>% 
    distinct(list_number)
}

# -- helper -- #

master = read_tsv('resource/exp_input_files/esp/esp_master_input.tsv')

# -- dat -- #

dat_id = list.files('~/Github/Pavlovia/hesp/data/')
path = '~/Github/Pavlovia/hesp/data/'
# simulated data:
# dat_id = list.files('resource/simulated_hesp/')
# path = 'resource/simulated_hesp/'

d = tibble(
    dat_id = dat_id,
    record_date = str_extract(dat_id, '202[1-4]-[0-1][0-9]-[0-9]{2}_[0-9]{2}h[0-9]{2}') %>% # aah it's another year
      lubridate::ymd_hm()
  )

d %<>% 
  mutate(
    data = map(dat_id, ~ read_csv(glue('{path}{.}'))),
    n_rows = map(data, ~ nrow(.))
    )

d %<>%
  filter(
    n_rows == 176
         )

glue('{nrow(d)} complete files, {length(dat_id)-nrow(d)} attempts.')

d %<>% mutate(
    proc = map(data, ~ procDat(.)),
    start = str_extract(dat_id, '2022.*^(?=\\.csv$)')
  ) %>% 
  select(dat_id,record_date,proc) %>% 
  unnest(cols = c(proc))

# d[d$part_id == '' & d$part_gender == 'nő' & d$part_yob == 1997 & d$part_edu == 18,]$dat_id
d[d$dat_id == "hungarian-esp_esp_participant_SESSION_2022-11-21_15h30.11.810.csv",]$part_id = 'DAG7T4'

d %<>% filter(
  !(part_id %in% c('petikevagyok','próba','proba','Peti','CsM','','szis','NN'))
)

# save unfiltered data
write_tsv(d, 'exp_data/esp/esp_master_all_unfiltered.tsv')

# print ids to a tsv
d %>% 
  distinct(record_date,dat_id,part_id) %>% 
  write_tsv('exp_data/esp_completed_ids.tsv')

# -- some people did it twice -- #

did_twice = d %>% 
  distinct(dat_id,part_id) %>% 
  count(part_id) %>% 
  filter(n > 1)

did_twice %>% 
  kable()

d %>% 
  filter(part_id %in% did_twice$part_id) %>% 
  distinct(part_id,dat_id,variation)

bad_ids = c(
  'hungarian-esp_esp_participant_SESSION_2023-05-09_10h49.20.190.csv',
  'hungarian-esp_esp_participant_SESSION_2023-05-26_20h45.16.179.csv',
  'hungarian-esp_esp_participant_SESSION_2023-05-26_21h17.52.720.csv',
  'hungarian-esp_esp_participant_SESSION_2023-05-26_21h17.52.720.csv',
  'hungarian-esp_esp_participant_SESSION_2023-06-06_18h33.41.94.csv'
)

d %<>% filter(!dat_id %in% bad_ids)

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

# -- check list numbers -- #

d %>% 
  filter(trial_kind == 'esp trial',variation == 'hotelban/hotelben') %>% 
  distinct(part_id,list_number,reg_rate,reg_dist,variation) %>% 
  count(list_number,reg_rate,reg_dist,variation) %>% 
  pivot_wider(names_from = variation, values_from = n,values_fill = 0) %>% 
  kable(caption = 'List counts.')

# -- filters -- #

print('Now that we tallied everyone who finished one way or another, we need to make sure the data meet our exclusion criteria and we still have 7 people per list for each group.')

ncareless = d %>% 
  count(part_id,picked_left) %>% 
  countKeys(part_id,picked_left,`TRUE`,`FALSE`) %>% 
  nrow()

if(ncareless == 0){print('No very careless participants.')}else{glue('{ncareless} participants were very careless.')}

print('These people were too slow overall and we need to rerun them:')

slow_people = d %>% 
  mutate(rt = as.double(rt)) %>% 
  group_by(part_id,variation,trial_kind) %>% 
  overUpper(rt)

slow_people %>% 
  kable()

print('These trial prompts were too slow overall and we could drop them:')

slow_trials = d %>% 
  mutate(rt = as.double(rt)) %>% 
  group_by(stimulus,variation,trial_kind) %>% 
  overUpper(rt)

slow_trials %>% 
  kable()

print('But we need to drop specific responses if they were super slow:')

too_long = d %>%
  group_by(variation) %>% 
  mutate(
    rt = as.double(rt),
    median_rt = median(rt),
    mad_rt = mad(rt),
    upper = median_rt + 3 * mad_rt,
    over_upper = rt > upper,
    rt_m = rt / 1000 / 60
  ) %>% 
  select(dat_id,part_id,variation,stimulus,trial_kind,list_number,over_upper) %>% 
  filter(over_upper)

print('filtering...')

d2 = d %>% anti_join(too_long)
d2 %<>% filter(!part_id %in% slow_people$part_id)
d2 %<>% filter(!stimulus %in% slow_trials$stimulus)

print('now we need to check lists again.')

d2 %>% 
  filter(trial_kind == 'esp trial') %>% 
  distinct(part_id,list_number,reg_rate,reg_dist,variation) %>% 
  count(variation,reg_rate,reg_dist) %>% 
  pivot_wider(names_from = reg_dist, values_from = n) %>% 
  kable(caption = 'Cond counts.')

d2 %>% 
  filter(trial_kind == 'esp trial') %>% 
  distinct(part_id,list_number,reg_rate,reg_dist,variation) %>% 
  count(list_number,reg_rate,reg_dist,variation) %>% 
  pivot_wider(names_from = variation, values_from = n,values_fill = 0) %>% 
  kable(caption = 'List counts.')

print('some lists have more participants than we wanted.')

keep_participants = d2 %>%
  mutate(list_number = as.double(list_number)) %>% 
  filter(trial_kind == 'esp trial') %>% 
  distinct(part_id,dat_id,record_date,variation,list_number) %>% 
  arrange(list_number,variation,record_date) %>% 
  group_by(list_number,variation) %>% 
  mutate(id = 1:n()) %>% 
  filter(id < 8) %>% 
  pull(part_id)

d3 = filter(d2, part_id %in% keep_participants)

print("there we go:")

d3 %>% 
  filter(trial_kind == 'esp trial') %>% 
  distinct(part_id,list_number,reg_rate,reg_dist,variation) %>% 
  count(list_number,reg_rate,reg_dist,variation) %>% 
  pivot_wider(names_from = variation, values_from = n,values_fill = 0) %>% 
  kable(caption = 'List counts.')

print('how much data did we exclude by outlier removal for lakik and cselekszenek?')

rows_left = d3 %>% 
  filter(variation != 'hotelban/hotelben') %>% 
  nrow()
rows_expected = 108 * 7 * 12 * 2
glue('{round(rows_left/rows_expected*100)}% of observations remain.')

# write_tsv(d3, 'exp_data/esp/esp_master_all_filtered.tsv')

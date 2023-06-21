# process prolific hesp data w/ sleep
setwd('~/Github/Racz2024')
library(tidyverse)
library(magrittr)
library(glue)
library(knitr)

# -- fun -- #

procDat1 = function(dat){
  
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
    mutate(list_number = 2) %>%  # double whoops on a bun
    inner_join(master, by = c("list_number", "stimulus", "esp_response"))
  posttest = exp_pair[[2]] %>% 
    mutate(list_number = 4) %>%  # how EMBARASSING
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

procDat2 = function(dat){
  
  part = dat %>% 
    filter(trial_type == 'survey-text') %>% 
    pull(response)
  
  id = str_extract(part, '(?<=\"Q0\":\").*(?=\")')
  
  dat2 = dat %>% 
    mutate(
      trial_order = 1:n(),
      part_id = id,
      choice1 = str_extract(choices, '[a-záéíóőúűöü]+(?=\",)'),
      choice2 = str_extract(choices, '(?<=,\")[a-záéíóőúűöü]+'),
    ) %>%
    select(part_id,list_number,trial_kind,trial_index,stimulus,choice1,choice2,response,response_string,rt,time_elapsed)
  
  master2 = master %>% 
    rename('stimulus' = prompt) %>% 
    select(-esp_response)
  
  posttest = dat2 %>% 
    mutate(list_number = 8) %>%  # how EMBARASSING
    inner_join(master2) %>% 
    mutate( # this can stay empty
      picked_left = response_string == choice1,
      picked_v1 = response_string == variant1,
      reg_rate = NA,
      reg_dist = NA
    )
  
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

wrapDat = function(dat_id,path){
  
  dat = tibble(
    dat_id = dat_id,
    record_date = str_extract(dat_id, '202[1-4]-[0-1][0-9]-[0-9]{2}_[0-9]{2}h[0-9]{2}') %>% # aah it's another year
      lubridate::ymd_hm()
  )
  
  dat %<>% 
    mutate(
      data = map(dat_id, ~ read_csv(glue('{path}{.}'))),
      n_rows = map(data, ~ nrow(.))
    )

return(dat)
}
  
# -- helper -- #

master = read_tsv('resource/exp_input_files/esp/esp_master_input.tsv')

# -- dat 1 -- #

dat_id1 = list.files('~/Github/Pavlovia/hesp/data/')
path1 = '~/Github/Pavlovia/hesp/data/'

d1 = wrapDat(dat_id1,path1) %>% 
  filter(
  n_rows == 176
)

d1 %<>% mutate(
  proc = map(data, ~ procDat1(.)),
  start = str_extract(dat_id, '2022.*^(?=\\.csv$)')
) %>% 
  select(dat_id,record_date,proc) %>% 
  unnest(cols = c(proc)) %>% 
  filter(
  !(part_id %in% c('petikevagyok','próba','proba','Peti','CsM','','szis','NN'))
)

# -- dat 2 -- #

dat_id2 = list.files('~/Github/Pavlovia/hesp_sleep/data/')
path2 = '~/Github/Pavlovia/hesp_sleep/data/'

d2 = wrapDat(dat_id2,path2) %>% 
  filter(
    n_rows == 62 # ahem
  )

d2 %<>% mutate(
  proc = map(data, ~ procDat2(.)),
  start = str_extract(dat_id, '2022.*^(?=\\.csv$)')
) %>% 
  select(dat_id,record_date,proc) %>% 
  unnest(cols = c(proc)) %>% 
  filter(
  !(part_id %in% c('petikevagyok','próba','proba','Peti','CsM','','szis','NN'))
)

# -- combine -- #

d1 %>% 
  distinct(part_id,reg_rate,reg_dist,trial_kind) %>% 
  count(part_id,trial_kind)

d2 %>% 
  distinct(part_id,reg_rate,reg_dist,trial_kind) %>% 
  count(part_id,trial_kind)

d3 = bind_rows(d1,d2)

# save unfiltered data
write_tsv(d3, 'exp_data/esp/esp_master_sleep_pt1_pt2_all_unfiltered.tsv')

# -- checks -- #

# if the checks worked for the entire matching game run, they will work now, it's the same stimuli

# -- check list numbers -- #

d3 %>% 
  filter(trial_kind == 'esp trial',variation == 'lakok/lakom') %>% 
  distinct(part_id,list_number,reg_rate,reg_dist,variation) %>% 
  count(list_number,reg_rate,reg_dist,variation) %>% 
  pivot_wider(names_from = variation, values_from = n,values_fill = 0) %>% 
  kable(caption = 'List counts.')

# -- filters -- #

print('Now that we tallied everyone who finished one way or another, we need to make sure the data meet our exclusion criteria and we still have 21.')

ncareless = d3 %>% 
  count(part_id,picked_left) %>% 
  countKeys(part_id,picked_left,`TRUE`,`FALSE`) %>% 
  nrow()

if(ncareless == 0){print('No very careless participants.')}else{glue('{ncareless} participants were very careless.')}

print('These people were too slow overall and we need to rerun them:')

slow_people = d3 %>% 
  mutate(rt = as.double(rt)) %>% 
  group_by(part_id,variation,trial_kind) %>% 
  overUpper(rt)

slow_people %>% 
  kable()

print('These trial prompts were too slow overall and we could drop them:')

slow_trials = d3 %>% 
  mutate(rt = as.double(rt)) %>% 
  group_by(stimulus,variation,trial_kind) %>% 
  overUpper(rt)

slow_trials %>% 
  kable()

print('But we need to drop specific responses if they were super slow:')

too_long = d3 %>%
  group_by(variation,trial_kind) %>% 
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

too_long

print('filtering...')

d4 = d3 %>% anti_join(too_long)
d4 %<>% filter(!part_id %in% slow_people$part_id)
d4 %<>% filter(!stimulus %in% slow_trials$stimulus)

print('now we need to check lists again.')

d4 %>% 
  distinct(part_id,variation,trial_kind) %>% 
  count(variation,trial_kind) %>% 
  kable(caption = 'Cond counts.')

write_tsv(d4, 'exp_data/esp/esp_sleep_pt1_pt2_master_all_filtered.tsv')

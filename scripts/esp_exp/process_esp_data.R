# process hesp data
setwd('~/Github/Racz2024')
library(tidyverse)
library(ggthemes)
library(glue)

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
      variant1 = case_when( # specific to lakok!!!
        str_detect(choice1, 'k$') ~ choice1,
        str_detect(choice2, 'k$') ~ choice2
      ),
      variant2 = case_when(
        str_detect(choice1, 'm$') ~ choice1,
        str_detect(choice2, 'm$') ~ choice2
      ),
      picked_left = response_string == choice1,
      picked_v1 = response_string == variant1,
      esp_v1 = esp_response == variant1
    ) %>%
    select(part_id,part_gender,part_yob,part_edu,list_number,trial_kind,trial_index,stimulus,choice1,choice2,variant1,variant2,response,response_string,picked_v1,picked_left,esp_response,esp_match,esp_v1,rt) %>% 
    group_split(trial_kind)
  
  master2 = master %>% 
    select(-esp_response)
  
  esp = exp_pair[[1]] %>% 
    inner_join(master, by = c("list_number", "variant1", "variant2", "esp_response"))
  posttest = exp_pair[[2]] %>% 
    inner_join(master2, by = c("list_number", "variant1", "variant2"))
  
  bind_rows(esp,posttest) %>% 
    mutate(
      picked_majority = case_when(
        reg_rate == 'high' ~ picked_v1,
        reg_rate == 'low' ~ !picked_v1
      )
    )
  
}

# -- helper -- #

master = read_tsv('resource/exp_input_files/esp/esp_master_input.tsv')

# -- dat -- #

# dat_id = list.files('~/Github/Pavlovia/hesp/data/')
# dat_id = dat_id[str_detect(dat_id, 'ESP-DEMO_SESSION_2022-10-06_2')]
# path = '~/Github/Pavlovia/hesp/data/'
# simulated data:
dat_id = list.files('resource/simulated_hesp/')
path = 'resource/simulated_hesp/'

d = tibble(
    dat_id = dat_id
  ) %>% 
  mutate(
    data = map(dat_id, ~ read_csv(glue('{path}{.}')))
    )

# d$data[[1]] %>% View

d %<>% mutate(
    proc = map(data, ~ procDat(.)),
    start = str_extract(dat_id, '2022.*^(?=\\.csv$)')
  ) %>% 
  select(dat_id,proc) %>% 
  unnest(cols = c(proc))

write_tsv(d, 'exp_data/esp/esp_master.tsv')


# simulate hesp data
setwd('~/Github/Racz2024')
library(tidyverse)
library(ggthemes)
library(glue)

set.seed(1337)

# -- fun -- #
buildResponses = function(my_list_number = 1,n_v1_esp = 10,n_v1_pt = 10){
  # list number tidying
  my_list_number_posttest = case_when(
    my_list_number < 4 ~ 4,
    my_list_number < 8 & my_list_number > 3 ~ 8,
    my_list_number > 7 ~ 1
  )
  
  # part dat
  part = tibble(
    trial_type = 'survey-text',
    response = '{"Q0":"5f9eb8c1c1dc8505f5f1bcd4","Q1":"férfi","Q2":"1998","Q3":"17"}'
  )
  
  # esp setup
  esp = master %>% 
    filter(
      variation == 'lakok/lakom',
      list_number == my_list_number
    ) %>% 
    sample_n(n()) %>% 
    rowwise() %>% 
    mutate(
      choice_order = sample(1:2,1),
      rt = sample(50:100,1)
    ) %>% 
    ungroup() %>% 
    mutate(
      trial_kind = 'esp trial',
      trial_index = seq(8,114,2),
      stimulus = prompt,
      choice1 = case_when(
        choice_order == 1 ~ variant1,
        choice_order == 2 ~ variant2,
      ),
      choice2 = ifelse(choice1==variant1,variant2,variant1),
      choices = glue('["{choice1}","{choice2}"]')
    ) %>% 
    select(trial_kind,rt,variant1,variant2,choices,list_number,trial_index,stimulus,esp_response,word_rank) %>% # responses
    arrange(word_rank) %>% 
    mutate(
      response = as.character(c(rep(0,n_v1_esp),rep(1,54-n_v1_esp))),
      response_string = case_when(
        response == 0 ~ variant1,
        response == 1 ~ variant2,
      ),
      esp_match = case_when(
        response_string == esp_response ~ 1,
        response_string != esp_response ~ 0
      ),
    ) %>%
    select(-variant1,-variant2,-word_rank) %>% 
    arrange(trial_index)
  
  posttest = master %>% 
    filter(
      variation == 'lakok/lakom',
      list_number == my_list_number_posttest
    ) %>% 
    sample_n(n()) %>% 
    rowwise() %>% 
    mutate(
      choice_order = sample(1:2,1),
      rt = sample(50:100,1)
    ) %>% 
    ungroup() %>% 
    mutate(
      rt = sample(50:100,1),
      trial_kind = 'posttest trial',
      trial_index = 117:170,
      stimulus = prompt,
      choice1 = case_when(
        choice_order == 1 ~ variant1,
        choice_order == 2 ~ variant2,
      ),
      choice2 = ifelse(choice1==variant1,variant2,variant1),
      choices = glue('["{choice1}","{choice2}"]'),
      esp_response = NA
    ) %>% 
    select(trial_kind,rt,variant1,variant2,choices,list_number,trial_index,stimulus,esp_response,word_rank) %>% # responses
    arrange(word_rank) %>% 
    mutate(
      response = as.character(c(rep(0,n_v1_pt),rep(1,54-n_v1_pt))),
      response_string = case_when(
        response == '0' ~ variant1,
        response == '1' ~ variant2,
      )
    ) %>% 
    select(-variant1,-variant2,-word_rank) %>% 
    arrange(trial_index)
  
  bind_rows(part,esp,posttest)
}

# -- helper -- #

master = read_tsv('resource/exp_input_files/esp/esp_master_input.tsv')

buildResponses(1,1,1) %>% View

tibble(
  id = 1:10
) %>% 
  mutate(
    datae = map(id, ~ buildResponses(., 11, 22)),
    dat_id = glue('hesp_sim_dat{id}.csv')
  ) %>% 
  select(dat_id,datae) %>% 
  pwalk(~ write_csv(x = .y, file = glue('resource/simulated_hesp/{.x}')))

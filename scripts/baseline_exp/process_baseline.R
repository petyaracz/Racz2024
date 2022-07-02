# read in downloaded data from psychopy exp on pavlovia, tidy up, write out

# -- header -- #

setwd('~/Github/Racz2024/')

library(tidyverse)
library(glue)
library(magrittr)
library(lubridate)

col_specs = cols(
  my_list = col_character(),
  key.keys = col_character(),
  key.rt = col_double(),
  key_2.keys = col_character(),
  key_2.rt = col_double(),
  keyboard_input_2.keys = col_character(),
  keyboard_input_2.rt = col_double(),
  key_3.keys = col_character(),
  key_3.rt = col_double(),
  keyboard_input_3.keys = col_character(),
  keyboard_input_3.rt = col_double(),
  key_4.keys = col_character(),
  key_4.rt = col_double(),
  keyboard_input_4.keys = col_character(),
  keyboard_input_4.rt = col_double(),
  key_5.keys = col_character(),
  key_5.rt = col_double(),
  my_button1 = col_character(),
  my_button2 = col_character(),
  keyboard_input.keys = col_character(),
  keyboard_input.rt = col_double(),
  withinBlock.thisRepN = col_double(),
  withinBlock.thisTrialN = col_double(),
  withinBlock.thisN = col_double(),
  withinBlock.thisIndex = col_double(),
  withinBlock.ran = col_double(),
  rowname = col_double(),
  word = col_character(),
  prompt = col_character(),
  suffix = col_character(),
  vowel = col_character(),
  carrier_sentence = col_character(),
  target_sentence = col_character(),
  target1 = col_character(),
  target2 = col_character(),
  category = col_character(),
  derivational = col_character(),
  nsyl = col_double(),
  Azonosító = col_character(),
  `Melyik évben születtél?` = col_double(),
  `Mi a nemed?` = col_character(),
  `Tanultál nyelvészetet?` = col_character(),
  date = col_character(),
  expName = col_character(),
  psychopyVersion = col_character(),
  OS = col_character(),
  frameRate = col_double()
)

# -- functions -- #

transcribe = function(nonce_word,direction){
  
  case_when(
    direction == 'single' ~ nonce_word %>% 
      str_replace_all(., c('cs' = 'ç', 'sz' = 'ß', 'zs' = 'Ω', 'ty' = '†', 'gy' = '©', 'ny' = '¥', 'ly' = '¬')),
    direction == 'double' ~ nonce_word %>% 
      str_replace_all(., c('ç' = 'cs', 'ß' = 'sz', 'Ω' = 'zs', '†' = 'ty', '©' = 'gy', '¥' = 'ny', '¬' = 'ly')),
    T ~ 'wrong direction, either (to) "single" or "double"'
  )
}

# -- parse -- #

file_name = list.files('exp_data/baseline/data/') %>% 
  str_subset('.*csv') %>% 
  str_subset('hesp')

d = tibble(
  file_name
) %>% 
  mutate(
    file_name_date = str_extract(file_name, '2022-..-..') %>% 
      ymd()
  ) %>% 
  filter(file_name_date >= '2022-03-25')

d %<>% 
  filter(
    !file_name %in% c('PARTICIPANT_hesp_baseline_2022-04-14_11h50.46.771.csv','PARTICIPANT_hesp_baseline_2022-04-18_21h37.12.531.csv')
  ) # these have no Azonosító (nincsen) and parse weirdly

d %<>%
  mutate(
    dat = map(file_name, ~ read_csv(
      glue('exp_data/baseline/data/{.}'),
      col_types = col_specs
      ))
  )

d %<>% 
  unnest(cols = dat)

d %>% 
  filter(!is.na(my_list)) %>% 
  distinct(file_name,my_list,Azonosító)

d %>% 
  filter(!is.na(my_list)) %>% 
  count(my_list)

d %>% 
  count(Azonosító) %>% View

d %<>%
  fill(my_list) %>% 
  mutate(
    my_choice = case_when(
      keyboard_input.keys == 'left' ~ my_button1,
      keyboard_input.keys == 'right' ~ my_button2
    ),
    resp_is_first_variant = case_when(
      my_choice == target1 ~ T,
      my_choice == target2 ~ F
    ) 
  )

d %<>%
  mutate(Azonosító = str_replace(Azonosító, '2066', 'PR2066')) %>% 
  filter(str_detect(Azonosító, 'nincsen', negate = T)) # watch out

# -- write -- #

d %>% 
  select(file_name,file_name_date,my_list,my_button1,my_button2,keyboard_input.keys,keyboard_input.rt,withinBlock.thisN,word,prompt,suffix,vowel,carrier_sentence,target_sentence,target1,target2,my_choice,resp_is_first_variant,category,derivational,nsyl,Azonosító,`Melyik évben születtél?`,`Mi a nemed?`,`Tanultál nyelvészetet?`,date,expName,psychopyVersion,OS) %>% 
  write_tsv('exp_data/baseline/baseline_tidy.tsv')

# in corpus data: lakok/lakom,hotelban/hotelben,cselekszenek/cselekednek
# in test data: 
d2 = d %>% 
  count(
    word,suffix,target1,target2,carrier_sentence,target_sentence,category,derivational,nsyl,vowel,resp_is_first_variant
  ) %>% 
  pivot_wider(
    names_from = resp_is_first_variant, 
    values_from = n, 
    values_fill = 0
  ) %>%
  rename(
    'target1_resp' = `TRUE`, 
    'target2_resp' = `FALSE`
  ) %>% 
  mutate(
    variation = case_when(
      category == 'dzsungel' ~ 'hotelban/hotelben',
      category == 'lakik' ~ 'lakok/lakom',
      category == 'cselekszik' ~ 'cselekszenek/cselekednek'
    ),
    resp1 = case_when(
      variation == 'lakok/lakom' ~ target1_resp,
      variation == 'hotelban/hotelben' ~ target2_resp,
      variation == 'cselekszenek/cselekednek' ~ target1_resp
    ),
    resp2 = case_when(
      variation == 'lakok/lakom' ~ target2_resp,
      variation == 'hotelban/hotelben' ~ target1_resp,
      variation == 'cselekszenek/cselekednek' ~ target2_resp
    ),
    variant1 = case_when(
      variation == 'lakok/lakom' ~ target1,
      variation == 'hotelban/hotelben' ~ target2,
      variation == 'cselekszenek/cselekednek' ~ target1
    ),
    variant2 = case_when(
      variation == 'lakok/lakom' ~ target2,
      variation == 'hotelban/hotelben' ~ target1,
      variation == 'cselekszenek/cselekednek' ~ target2
    ),
    p = resp1 / ( resp1 + resp2 ),
    log_odds = log( ( resp1 + 1 ) / ( resp2 + 1 ) ),
    word = fct_reorder(word, p),
    type = 'nonce word response',
    base = word,
    base_tr = transcribe(base, 'single')
    ) %>% 
  select(base,base_tr,variation,type,variant1,variant2,resp1,resp2,log_odds,derivational,nsyl,vowel,suffix,carrier_sentence,target_sentence)

write_tsv(d2, 'exp_data/baseline/baseline_tidy_proc.tsv')

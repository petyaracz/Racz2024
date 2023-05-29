# -- header -- #

library(tidyverse)

setwd('~/Github/Racz2024/')

# -- fun -- #

countKeys = function(dat, file_name, keyboard_input.keys, left, right){
  dat %>% 
  pivot_wider(id_cols = {{file_name}}, names_from = {{keyboard_input.keys}}, values_from = n, values_fill = 0) %>%
  filter({{left}} == 0 | {{right}} == 0)
}

overUpper = function(dat, keyboard_input.rt){
  dat %>% 
  summarise(all_rt = sum({{keyboard_input.rt}})) %>% 
  ungroup() %>% 
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

# -- read -- #

b = read_tsv('exp_data/baseline/baseline_tidy.tsv')
cs = read_tsv('exp_data/esp/esp_master_cselekszik.tsv')
l = read_tsv('exp_data/esp/esp_master_lakok.tsv')

# -- QA -- #

########
# baseline
########

# based on https://aspredicted.org/BL1_S7V
# in the baseline, do some people keep pushing the same button throughout?
b %>% 
  count(file_name,keyboard_input.keys) %>% 
  countKeys(file_name, keyboard_input.keys, left, right)
# no they don't
# in the baseline, is anyone longer than median duration + 3 mad duration
b %>% 
  group_by(file_name) %>% 
  overUpper(keyboard_input.rt)
# they are but I can live with this
# how slow are people?
b %>% 
  ggplot(aes(keyboard_input.rt)) +
  geom_histogram() +
  theme_bw()
# I can live with this too. this wasn't a response time study and they weren't like repeatedly told to hurry up.
# how slow are trials?
b %>% 
  group_by(word) %>% 
  overUpper(keyboard_input.rt)
# okay these are pretty bad but at least there isn't a lot of them, like? median+3mad for trial duration being 5 minutes is also quite something.

########
# esp
########

# in the esp, do some people keep pushing the same button throughout?
l %>% 
  count(part_id,picked_left) %>% 
  countKeys(part_id,picked_left,`TRUE`,`FALSE`)
cs %>% 
  count(part_id,picked_left) %>% 
  countKeys(part_id,picked_left,`TRUE`,`FALSE`)
# no they don't. that would be an achievement in malicious effort
# in the esp, is anyone longer than median duration + 3 mad duration
l %>% 
  group_by(part_id) %>% 
  overUpper(rt)
cs %>% 
  group_by(part_id) %>% 
  overUpper(rt)
# this is miliseconds now, whew. people didn't spend thousands of hours on the experiment.
# we need to rerun these. joy.
# how slow are people?
l %>% 
  ggplot(aes(rt)) +
  geom_histogram() +
  theme_bw()
cs %>% 
  ggplot(aes(rt)) +
  geom_histogram() +
  theme_bw()
# I can live with this too.
# how slow are trials?
l %>% 
  group_by(stimulus) %>% 
  overUpper(rt)
cs %>% 
  group_by(stimulus) %>% 
  overUpper(rt)
# okay we need to remove these.

#########
# lists I need to rerun
#########

# this would be dangerous as it uniques lists but luckily I only have 1 in l and 5 in cs and the same amount of list numbers so it's fine.
l %>% 
  whichList(l) 
# 10
cs %>% 
  whichList(cs)
# 7,9,6,0,3
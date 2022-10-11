# check hesp data
setwd('~/Github/Racz2024')
library(tidyverse)
library(ggthemes)
library(glue)
library(magrittr)

# pilot had the labels and word ranking off

d = read_tsv('exp_data/esp/prolific_esp_pilot.tsv')

d = d %>% 
  filter(trial_kind == 'esp trial') %>% 
  distinct(bot_variant_preference,bot_lexical_direction,word_rank,esp_v1) %>% 
  count(bot_variant_preference,bot_lexical_direction,esp_v1) %>% 
  pivot_wider(names_from = esp_v1, values_from = n) %>% 
  rename(v1_count = `TRUE`) %>% 
  select(-`FALSE`) %>% 
  left_join(d)

d %>%
  distinct(list_number,v1_count)

d %>% 
  filter(trial_kind == 'esp trial') %>% 
  count(part_id,v1_count,picked_v1) %>% 
  ggplot(aes(as.character(v1_count),n)) +
  geom_violin() +
  geom_point()

d %>% 
  filter(trial_kind == 'posttest trial') %>% 
  count(part_id,v1_count,picked_v1) %>% 
  ggplot(aes(as.character(v1_count),n)) +
  geom_violin() +
  geom_point()

d %>% 
  filter(trial_kind == 'esp trial') %>% 
  count(part_id,v1_count,picked_v1) %>% 
  ggplot(aes(as.character(v1_count),n)) +
  geom_violin() +
  geom_point()

d %>% 
  count(part_id,bot_variant_preference,bot_lexical_direction,picked_v1,trial_kind) %>% 
  ggplot(aes(bot_variant_preference,n,colour = bot_lexical_direction)) +
  geom_boxplot() +
  facet_wrap( ~ trial_kind)

d %<>% 
  group_by(part_id) %>% 
  arrange(trial_index) %>% 
  mutate(trial = 1:108) %>% 
  ungroup()

d %>% 
  filter(trial_kind == 'esp trial') %>% 
  ggplot(aes(trial,as.double(esp_match))) +
  geom_point() +
  geom_smooth() +
  theme_bw()

d %>% 
  filter(trial_kind == 'posttest trial',picked_v1) %>% 
  count(part_id,v1_count) %>% 
  ggplot(aes(as.character(v1_count),n)) +
  geom_boxplot() +
  theme_bw()

laci = d %>% 
  filter(trial_kind == 'posttest trial',picked_v1) %>% 
  count(part_id,v1_count)

summary(lm(n ~ v1_count, data = laci)  )

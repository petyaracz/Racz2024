# creating lists for the esp experiment

setwd('~/Github/Racz2024/')

library(tidyverse)
library(magrittr)
library(glue)
library(ggthemes)
library(patchwork)
library(jsonlite)

addRanks = function(dat){
  dat %>% 
    select(-vowel,-type) %>% 
    arrange(log_odds) %>% 
    mutate(
      list = c(rep(c(1:3),54))
    ) %>% 
    group_by(list) %>% 
    mutate(
      rank = 1:54
    ) %>% 
    ungroup()
}

# 39:15

vizDat = function(dat,regrate){
  dat %>% 
    mutate(
      group = case_when(
        regrate == 'high' & rank %in% 1:39 ~ 'lower',
        regrate == 'high' & rank %in% 40:54 ~ 'upper',
        regrate == 'low' & rank %in% 1:15 ~ 'lower',
        regrate == 'low' & rank %in% 16:54 ~ 'upper',
        regrate == 'nc' & rank %in% 1:27 ~ 'lower',
        regrate == 'nc' & rank %in% 28:54 ~ 'upper'
      )
    ) %>% 
    mutate(base = fct_reorder(base, log_odds)) %>% 
    ggplot(aes(base,plogis(log_odds),label = base, colour = group)) +
    geom_label() +
    theme_few() +
    scale_y_continuous(breaks = seq(0,1,.1), limits = c(0,1)) +
    facet_wrap(~ list) +
    guides(group = 'none')
}

setFlags = function(regrate,regdist,dat){
  dat %>% 
    mutate(
      lower_upper = case_when(
        regrate == 'high39' & rank %in% 1:39 ~ 'lower',
        regrate == 'low15' & rank %in% 1:15 ~ 'lower',
        regrate == 'nc27' & rank %in% 1:27 ~ 'lower',
        regrate == 'high39' & rank %in% 40:54 ~ 'upper',
        regrate == 'low15' & rank %in% 16:54 ~ 'upper',
        regrate == 'nc27' & rank %in% 28:54 ~ 'upper'
      ),
      pick = case_when(
        lower_upper == 'upper' & regdist == 'typical' ~ 'variant1',
        lower_upper == 'lower' & regdist == 'reversed' ~ 'variant1',
        lower_upper == 'lower' & regdist == 'typical' ~ 'variant2',
        lower_upper == 'upper' & regdist == 'reversed' ~ 'variant2'
      )
    )
}

# target_word: 'weave', response_words: ['weaved','wove'], esp_response: 'wove'

s = read_tsv('exp_data/baseline/baseline_tidy_proc.tsv')
cvc = filter(s, variation == 'cselekszenek/cselekednek')
ik = filter(s, variation == 'lakok/lakom')

ik = addRanks(ik)
cvc = addRanks(cvc)

esp_master = crossing(
  regrate = c('low15','high39','nc27'),
  regdist = c('typical','reversed'),
  data = list(ik,cvc)
) %>% 
  mutate(
    t_data = pmap(list(regrate,regdist,data), ~ setFlags(..1,..2,..3))
  ) %>% 
  select(-data) %>% 
  unnest(cols = t_data) %>% 
  mutate(
    target_word = base,
    response_words = list(c(variant1,variant2)),
    esp_response = case_when(
      pick == 'variant1' ~ variant1,
      pick == 'variant2' ~ variant2
    ),
    var_name = str_extract(variation, '^.*(?=\\/)'),
    file_name = glue('type_{var_name}_rate_{regrate}_dist_{regdist}_list_{list}')
  ) %>% 
  arrange(var_name,regrate,regdist,list)

# check this

plots = esp_master %>% 
  group_by(file_name) %>% 
  mutate(base = fct_reorder(base,log_odds)) %>% 
  nest() %>% 
  mutate(
    plot = map2(data,file_name, ~ ggplot(.x,
                              aes(rank,log_odds,colour = lower_upper,label = esp_response)
                              ) +
                 geom_text() +
                 theme_few() +
                 ggtitle(glue('{.y}'))
               )
  ) %>% 
  pull(plot)

# for (i in 1:36){
#   ggsave(plots[[i]], filename = glue('whatever/file{i}.png'), width = 8, height = 8)
# }

# write master

write_tsv(esp_master, 'resource/exp_input_files/esp/esp_master_input.tsv')

# write json

# toy examples


# I'm here:
esp_master %>% 
  filter(list == 1) %>% 
  mutate(weight = case_when(
    regrate == 'high39' ~ .6,
    regrate == 'low15' ~ .4,
    regrate == 'nc27' ~ .5
  )
           ) %>%
  group_by(regrate,regdist,var_name) %>% 
  sample_n(6, weight = weight)

esp_master %>% 
  filter(file_name == 'type_lakok_rate_low15_dist_typical_list_3') %>% 
  write_json(path = 'resource/exp_input_files/esp/toy.json')

esp_master %>% 
  group_by(file_name) %>% 
  nest() %>% 
  map2(data, ~ write_json(glue('resource/exp_input_files/esp/')))
  

setwd('~/Github/Racz2024/')

library(googlesheets4)
library(tidyverse)
library(magrittr)

d = read_tsv('exp_data/esp/esp_master.tsv')

match = unique(d$part_id)

jel = read_sheet('https://docs.google.com/spreadsheets/d/1bXciBt7q3WNuQ5zvo1PJZ3hTtbYH3-P6fVkYHds201Q/edit#gid=0', sheet = 'Jelentkezés')

jel %<>% 
  slice(3:nrow(jel)) %>% 
  rename(
    'Neptun' = `...1`,
    'Petya kísérlet' = `ONLINE KÍSÉRLET (MINDKETTŐT KI KELL TÖLTENI 2 PLUSZPONTÉRT)`,
    'Noémi kísérlet' = `...3`,
    'Peti kísérlet' = `LABORKÍSÉRLET - 2 PLUSZPONT A KÍSÉRLET ELVÉGZÉSE FEJÉBEN`,
    'óra' = `...5`
  ) %>% 
  unnest(cols = c(`Petya kísérlet`, `Noémi kísérlet`, `Peti kísérlet`)) %>% 
  mutate(
    'Petya volt' = as.double(Neptun %in% match),
    'Petya kísérlet' = as.double(`Petya kísérlet`),
    'Noémi kísérlet' = as.double(`Noémi kísérlet`),
    'Peti kísérlet' = as.double(`Peti kísérlet`)
  ) %>% 
  select(Neptun, `Petya kísérlet`, `Petya volt`, `Noémi kísérlet`, `Peti kísérlet`, óra)

jel

write_sheet(jel, 'https://docs.google.com/spreadsheets/d/1kMow5UPK8t2FDxT5-d5SlW9JhpNTbTyeZN-quTaRS4A/edit?usp=sharing', sheet = 'voltak')  

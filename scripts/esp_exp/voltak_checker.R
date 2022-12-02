setwd('~/Github/Racz2024')
library(googlesheets4)
library(tidyverse)
library(magrittr)

d1a = read_tsv('exp_data/esp/esp_master_lakok.tsv')
d1b = read_tsv('exp_data/esp/esp_master_cselekszik.tsv')
d2 = read_tsv('~/Github/VargaLukicsLukacsRacz2023/data/sign_master.tsv')

match1 = c(unique(d1a$part_id),unique(d1b$part_id))
match2 = unique(d2$part_id)

gs4_auth(email = 'petermartonracz@gmail.com')

jel = read_sheet('https://docs.google.com/spreadsheets/d/1bXciBt7q3WNuQ5zvo1PJZ3hTtbYH3-P6fVkYHds201Q/edit#gid=0', sheet = 'Jelentkezés')

jel %<>% 
  slice(3:nrow(jel)) %>% 
  rename(
    'Neptun' = `...1`,
    'Petya kísérlet' = `ONLINE KÍSÉRLET (MINDKETTŐT KI KELL TÖLTENI 2 PLUSZPONTÉRT)`,
    'Noémi kísérlet' = `...3`,
    'Peti kísérlet' = `LABORKÍSÉRLET - 2 PLUSZPONT A KÍSÉRLET ELVÉGZÉSE FEJÉBEN`,
    'Kriszti kísérlet' = `2. ONLINE KÍSÉRLET - 1 PLUSZPONT JÁR A KITÖLTÉSÉÉRT`,
    'óra' = `...6`
  ) %>% 
  unnest(cols = c(`Petya kísérlet`, `Noémi kísérlet`, `Peti kísérlet`,`Kriszti kísérlet`)) %>% 
  mutate(
    'Petya volt' = as.double(Neptun %in% match1),
    'Petya kísérlet' = as.double(`Petya kísérlet`),
    'Noémi kísérlet' = as.double(`Noémi kísérlet`),
    'Peti kísérlet' = as.double(`Peti kísérlet`),
    'Kriszti kísérlet' = as.double(`Kriszti kísérlet`),
    'Kriszti volt' = as.double(Neptun %in% match2)
  ) %>% 
  select(Neptun, `Petya kísérlet`, `Petya volt`, `Noémi kísérlet`, `Kriszti kísérlet`,`Kriszti volt`,`Peti kísérlet`, óra)

jel

write_sheet(jel, 'https://docs.google.com/spreadsheets/d/1kMow5UPK8t2FDxT5-d5SlW9JhpNTbTyeZN-quTaRS4A/edit?usp=sharing', sheet = 'voltak')  

d1 = read_tsv('exp_data/esp/esp_master_all_unfiltered.tsv')
d2 = read_tsv('exp_data/esp/esp_master_all_filtered.tsv')

d1 %>% 
  group_by(part_id,trial_kind) %>% 
  summarise(sum = sum(rt)/1000/60) %>% 
  group_by(trial_kind) %>% 
  summarise(max = max(sum))

d1 %>% 
  group_by(part_id,trial_kind) %>% 
  summarise(sum = sum(rt)/1000/60) %>% 
  ggplot(aes(sum)) +
  geom_histogram() +
  theme_bw() +
  facet_wrap( ~ trial_kind)

d2 %>% 
  group_by(part_id) %>% 
  group_by(part_id,trial_kind) %>% 
  summarise(sum = sum(rt)/1000/60) %>% 
  group_by(trial_kind) %>% 
  summarise(max = max(sum))

d2 %>% 
  group_by(part_id,trial_kind) %>% 
  summarise(sum = sum(rt)/1000/60) %>% 
  ggplot(aes(sum)) +
  geom_histogram() +
  theme_bw() +
  facet_wrap( ~ trial_kind)


d1 %>% 
  group_by(base,trial_kind) %>% 
  summarise(sum = sum(rt)/1000/60) %>% 
  group_by(trial_kind) %>% 
  summarise(max = max(sum))

d1 %>% 
  group_by(base,trial_kind) %>% 
  summarise(sum = sum(rt)/1000/60) %>% 
  ggplot(aes(sum)) +
  geom_histogram() +
  theme_bw() +
  facet_wrap( ~ trial_kind)

d2 %>% 
  group_by(base) %>% 
  group_by(base,trial_kind) %>% 
  summarise(sum = sum(rt)/1000/60) %>% 
  group_by(trial_kind) %>% 
  summarise(max = max(sum))

d2 %>% 
  group_by(base,trial_kind) %>% 
  summarise(sum = sum(rt)/1000/60) %>% 
  ggplot(aes(sum)) +
  geom_histogram() +
  theme_bw() +
  facet_wrap( ~ trial_kind)

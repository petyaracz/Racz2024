d %>% 
  filter(trial_kind == 'esp trial') %>% 
  group_by(kind_index,variation) %>% 
  summarise(match = mean(esp_match)) %>% 
  ggplot(aes(kind_index, match, colour = variation)) +
  geom_point(alpha = .5) +
  # geom_jitter(width = .01, height = .05, alpha = .1) +
  geom_smooth(method = 'lm') +
  theme_bw() +
  geom_hline(yintercept = 0.5, lty = 2) +
  ylab('matches robot') +
  xlab('trial index 1:54') +
  ggtitle('agreement with robot in ESP phase (phase 1)')

d %>% 
  filter(trial_kind == 'esp trial') %>%
  group_by(kind_index,reg_rate,reg_dist,variation) %>% 
  summarise(match = mean(esp_match)) %>% 
  ggplot(aes(kind_index, match, colour = reg_dist)) +
  geom_jitter(width = .01, height = .05, alpha = .1) +
  geom_smooth(method = 'lm') +
  theme_bw() +
  facet_wrap( ~ reg_rate) +
  geom_hline(yintercept = 0.5, lty = 2) +
  ylab('matches robot') +
  xlab('trial index 1:54') +
  ggtitle('agreement with robot across conditions\nin ESP phase (phase 1)') +
  facet_wrap(~ variation)

d %>% 
  filter(trial_kind == 'posttest trial') %>%
  group_by(part_id,reg_rate,reg_dist,variation) %>% 
  summarise(picked_v1 = mean(picked_v1)) %>% 
  ggplot(aes(reg_rate,picked_v1,fill = reg_dist)) +
  geom_boxplot() +
  theme_bw() +
  geom_hline(yintercept = 0.5, lty = 2) +
  geom_rug() +
  labs(fill = 'robot\nresponse\ndistribution') +
  xlab('robot response 1 rate') +
  ylab('picked response 1') +
  facet_wrap( ~ variation)

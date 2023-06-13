library(knitr)

ik = read_tsv('resource/real_words/ik_verbs/ikes_pairs_webcorpus2.tsv')

ikb = ik %>% 
  pull(base)

ikb = b %>% 
  filter(variation == 'lakok/lakom') %>% 
  pull(base) %>% 
  c(ikb,.)

ikb %>% 
  sample(10)

ikex = b %>% 
  filter(variation == 'lakok/lakom') %>% 
  arrange(-log_odds) %>% 
  mutate(type = '1sg') %>% 
  select(base,variant1,resp1,variant2,resp2,type) %>% 
  slice(seq(1,152,15))

ikex %>% 
  kable
  
b %>% 
  filter(variation == 'cselekszenek/cselekednek') %>% 
  arrange(-log_odds) %>% 
  mutate(
    type = case_when(
      suffix == 'ünk' ~ '1pl',
      suffix == 'tek' ~ '2pl',
      suffix == 'nek' ~ '3pl'
    )
  ) %>% 
  select(base,variant1,resp1,variant2,resp2,type) %>% 
  slice(seq(2,152,15)) %>% 
  kable

ikex %>% 
  mutate(rank = 1:n()) %>% 
  # select(base,rank) %>% 
  mutate(
    `high typical` = ifelse(rank %in% 1:7, variant1, variant2),
    `low typical` = ifelse(rank %in% 1:3, variant1, variant2),
    `high reversed` = ifelse(rank %in% 4:10, variant1, variant2),
    `low reversed` = ifelse(rank %in% 8:10, variant1, variant2)
  ) %>% 
  select(base,rank,`high typical`,`low typical`,`high reversed`,`low reversed`) %>% 
  relocate(rank, .before = base) %>% 
  kable


posttest %>% 
  filter(variation == 'lakok/lakom',base %in% ikex$base,reg_dist == 'typical') %>% 
  count(base,reg_dist,baseline_log_odds,picked_v1) %>% 
  pivot_wider(names_from = picked_v1, values_from = n, values_fill = 0) %>% 
  mutate(posttest_log_odds = log((`TRUE` + 1)/(`FALSE` + 1))) %>% 
  select(-`TRUE`,-`FALSE`) %>% 
  ggplot(aes(baseline_log_odds,posttest_log_odds,label = base)) +
  # geom_line(lty = 3, colour = 'lightgrey') +
  geom_label(position = position_jitter()) +
  theme_few() +
  # scale_colour_colorblind() +
  xlab('baseline preference for variant 1 (log odds)') +
  ylab('post test preference for variant 1 (log odds)') +
  labs(colour = 'co-player distribution') +
  ggtitle('Typical co-player')

ggsave('~/Documents/lectures_apps/lectures/ESPtalks/Cologne_talk/pic8a.png', width = 7, height = 6)

posttest %>% 
  filter(variation == 'lakok/lakom',base %in% ikex$base) %>% 
  count(base,reg_dist,baseline_log_odds,picked_v1) %>% 
  pivot_wider(names_from = picked_v1, values_from = n, values_fill = 0) %>% 
  mutate(posttest_log_odds = log((`TRUE` + 1)/(`FALSE` + 1))) %>% 
  select(-`TRUE`,-`FALSE`) %>% 
  ggplot(aes(baseline_log_odds,posttest_log_odds,label = base,colour = reg_dist,group = base)) +
  geom_line(lty = 3, colour = 'lightgrey') +
  geom_label(position = position_jitter()) +
  theme_few() +
  scale_colour_colorblind() +
  xlab('baseline preference for variant 1 (log odds)') +
  ylab('post test preference for variant 1 (log odds)') +
  labs(colour = 'co-player distribution') +
  ggtitle('Typical and reversed co-player')

ggsave('~/Documents/lectures_apps/lectures/ESPtalks/Cologne_talk/pic8b.png', width = 9, height = 6)

v = read_tsv('resource/webcorpus2freqlist/verb_forms.tsv.gz')

v %>% 
  slice(21:50) %>% 
  pull(form)

v %>% 
  filter(lemma == 'tesz',lfpm10 > 1) %>% 
  arrange(-freq) %>% 
  pull(form)

cvc = read_tsv('resource/real_words/epenthetic_stems/epenthesis_pairs_webcorpus2.tsv')

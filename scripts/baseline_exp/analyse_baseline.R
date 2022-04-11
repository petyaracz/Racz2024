library(patchwork)

d = read_tsv('exp_data/baseline/baseline_tidy.tsv')

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
    p = target1_resp / ( target1_resp + target2_resp ),
    word = fct_reorder(word, p)
  )

plots = d2 %>% 
  group_by(category) %>% 
  nest() %>% 
  mutate(
    plot = map(
      data, ~ ggplot(., aes(word,p)) +
        geom_col() +
        theme_bw() +
        coord_flip()    
    )
  ) %>% 
  pull(plot)

wrap_plots(plots, ncol = 1)  
ggsave('~/Downloads/plots.pdf', width = 4, height = 40)

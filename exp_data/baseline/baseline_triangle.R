bl = read_tsv('exp_data/baseline/baseline_tidy.tsv')  

bl2 = bl %>% 
  filter(my_list == 'list1.csv',category == 'lakik') %>% 
  select(file_name,word,resp_is_first_variant)

bl_w = bl2 %>% 
  count(word,resp_is_first_variant) %>% 
  pivot_wider(word, names_from = resp_is_first_variant, values_from = n) %>% 
  mutate(p_word = `TRUE`/(`TRUE`+`FALSE`)) %>% 
  select(word,p_word)

bl_p = bl2 %>% 
  count(file_name,resp_is_first_variant) %>% 
  pivot_wider(file_name, names_from = resp_is_first_variant, values_from = n, values_fill = 0) %>%
  mutate(p_id = `TRUE`/(`TRUE`+`FALSE`)) %>% 
  select(file_name,p_id)

bl3 = bl2 %>% 
  inner_join(bl_w) %>% 
  inner_join(bl_p) %>% 
  mutate(
    word = fct_reorder(word, p_word),
    file_name = fct_reorder(file_name, p_id)
  )

bl3 %>% 
  ggplot(aes(file_name,word,fill = resp_is_first_variant)) +
  geom_tile() +
  theme_few()
  
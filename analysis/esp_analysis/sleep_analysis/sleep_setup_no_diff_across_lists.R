b = read_tsv('exp_data/baseline/baseline_tidy.tsv')

b %>% 
  count(resp_is_first_variant,word,my_list,category) %>% 
  pivot_wider(names_from = resp_is_first_variant, values_from = n) %>% 
  mutate(
    log_odds = (`TRUE`+1)/(`FALSE`+1),
    p = `TRUE`/(`TRUE`+`FALSE`)
         ) %>% 
  ggplot(aes(my_list,p)) +
  geom_violin() +
  geom_boxplot(width = .1) +
  facet_wrap( ~ category)

library(lme4)  

fit1 = glmer(resp_is_first_variant ~ 1 + my_list + (1|word) + (1|file_name), data = b[b$category == 'lakik',], family = 'binomial')

summary(fit1)

fit2 = glmer(resp_is_first_variant ~ 1 + my_list + (1|word) + (1|file_name), data = b[b$category == 'cselekszik',], family = 'binomial')

summary(fit2)

fit3 = glmer(picked_v1 ~ 1 + as.factor(list_number) + (1|base) + (1|part_id), data = posttest[posttest$variation == 'lakok/lakom',], family = 'binomial')

summary(fit3)

fit4 = glmer(picked_v1 ~ 1 + as.factor(list_number) + (1|base) + (1|part_id), data = posttest[posttest$variation == 'cselekszenek/cselekednek',], family = 'binomial')

summary(fit4)

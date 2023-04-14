library(lme4)
library(broom.mixed)

posttest %>% 
  ggplot(aes(baseline_log_odds,as.double(picked_v1), colour = reg_dist)) +
  geom_jitter(width = 0, height = .01) +
  geom_smooth(method = 'glm', method.args=list(family="binomial")) +
  theme_bw() +
  facet_wrap( ~ variation)

posttest_l = filter(posttest, variation == 'lakok/lakom')
posttest_cs = filter(posttest, variation == 'cselekszenek/cselekednek')

# fit1 = glmer(picked_v1 ~ 1 + reg_dist * baseline_log_odds + (1|part_id) + (1|base), family = binomial, data = posttest)  
# tidy(fit1)
# fit2 = glmer(picked_v1 ~ 1 + reg_dist * baseline_log_odds + (1|part_id) + (1|base), family = binomial, data = posttest_l)  
# tidy(fit2)
# fit3 = glmer(picked_v1 ~ 1 + reg_dist + baseline_log_odds + (1|part_id) + (1|base), family = binomial, data = posttest_l)  
# anova(fit2,fit3)
# 
# fit4 = glmer(picked_v1 ~ 1 + reg_dist * baseline_log_odds + (1|part_id) + (1|base), family = binomial, data = posttest_cs)  
# tidy(fit4)
# fit5 = glmer(picked_v1 ~ 1 + reg_dist + baseline_log_odds + (1|part_id) + (1|base), family = binomial, data = posttest_cs)  
# anova(fit4,fit5)

fit1 = glmer(picked_v1 ~ 1 + reg_rate + reg_dist + baseline_log_odds + variation + (1|part_id) + (1|base), family = binomial, data = posttest)  
tidy(fit1)
fit2 = glmer(picked_v1 ~ 1 + reg_rate + reg_dist * baseline_log_odds + variation + (1|part_id) + (1|base), family = binomial, data = posttest)  
tidy(fit2)
fit3 = glmer(picked_v1 ~ 1 + reg_rate + reg_dist * baseline_log_odds * variation + (1|part_id) + (1|base), family = binomial, data = posttest)  
tidy(fit3)
fit4 = glmer(picked_v1 ~ 1 + reg_rate + reg_dist * baseline_log_odds + baseline_log_odds * variation + (1|part_id) + (1|base), family = binomial, data = posttest)  
tidy(fit4)
anova(fit3,fit4)
anova(fit2,fit4)
anova(fit2,fit3)
# fit5 = glmer(picked_v1 ~ 1 + reg_rate + reg_dist * baseline_log_odds + baseline_log_odds * variation + reg_dist * variation + (1|part_id) + (1|base), family = binomial, data = posttest)  
# tidy(fit5) # no convergence

posttest_cs %>% 
  distinct(part_id,reg_rate,reg_dist,file_name) %>% 
  count(file_name)
posttest_l %>% 
  distinct(part_id,reg_rate,reg_dist,file_name) %>% 
  count(file_name)

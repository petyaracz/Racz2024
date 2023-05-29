

# -- jitter log odds -- #

posttest$baseline_log_odds_jitter = jitter(posttest$baseline_log_odds, factor = .0001)

# -- model syntax -- #

interactions = c(
  # no interactions
  'reg_rate + reg_dist + baseline_log_odds_jitter + variation',
  # baseline x dist interaction should definitely show
  'reg_rate + reg_dist * baseline_log_odds_jitter + variation',
  # it would be splendid if this depended on variation
  'reg_rate + baseline_log_odds_jitter * reg_dist * variation',
  # I guess reg rate effect could depend on variation too
  'reg_rate * variation + baseline_log_odds_jitter * reg_dist * variation',
  # This might work but with no interaction w/ variation for distxbasel which is awkward
  'reg_rate * variation + baseline_log_odds_jitter * reg_dist',
  # This could be a thing
  'reg_rate * baseline_log_odds_jitter * reg_dist + variation',
  # or we just overfit this
  'reg_rate * variation * baseline_log_odds_jitter * reg_dist',
  # this might be a thing
  'reg_rate * variation * reg_dist + baseline_log_odds_jitter',
  # also this
  'reg_rate * reg_dist + variation + baseline_log_odds_jitter'
)

formulae = glue('picked_v1 ~ 1 + {interactions} + (1|part_id) + (1|base)')

# -- model fitting -- #

fits = map(formulae, ~ glmer(formula = ., data = posttest, family = binomial(link = 'logit'), control = glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=20000))))

# -- best model -- #

anova(fits[[1]],fits[[2]])
anova(fits[[3]],fits[[2]])
anova(fits[[3]],fits[[4]])
anova(fits[[3]],fits[[5]])
anova(fits[[3]],fits[[6]])
anova(fits[[3]],fits[[7]])


fit2 = fits[[2]]
fit3 = fits[[3]]
tidy(fit3)
plot(effects::allEffects(fit3))

exp((BIC(fit2) - BIC(fit3))/2) # upszika

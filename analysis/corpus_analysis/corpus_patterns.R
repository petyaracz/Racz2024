setwd('~/Github/Racz2024')

library(tidyverse)
library(sjPlot)
library(lme4)
library(magrittr)
library(ggthemes)

# load corpus data
ik = read_tsv('resource/real_words/ik_verbs/ikes_pairs_webcorpus2.tsv')
ep = read_tsv('resource/real_words/epenthetic_stems/epenthesis_pairs_webcorpus2.tsv')

## ik

ik %<>% 
  mutate(
    s_log_lemma_freq = scale(log(lemma_freq_corrected)),
    nsyllable = ordered(nsyl),
    derivational_suffix = case_when(
        is.na(derivational) ~ 'no suffix',
        T ~ derivational
      ) %>% 
      fct_relevel('no suffix')
    )

fit1 = glmer(cbind(freq_1,freq_2) ~ 1 + s_log_lemma_freq + nsyl + derivational_suffix + (1|stem), data = ik, family = binomial, control = glmerControl(optimizer = "bobyqa"))
tidy(fit1,conf.int=T)
plot_model(fit1, 'est') +
  theme_bw()
plot_model(fit1, 'pred', terms = c('s_log_lemma_freq','nsyl [1,3,6]','derivational_suffix')) +
  theme_bw() +
  ylab('% eszek') +
  scale_colour_colorblind() +
  scale_fill_colorblind() +
  ggtitle('Predicted probabilities, -k/-m') +
  labs(colour = 'n syllables')
ggsave('~/Documents/lectures_apps/lectures/ESPtalks/Budapest_talk/corpus1.pdf', width = 6, height = 6)

## ep

count(ep,stem, sort = T) %>% View

ep %>% 
  filter(stem == 'hab') %>% 
  select(form_1,form_2,xpostag) %>% 
  knitr::kable()

ep %<>% 
  mutate(
    s_log_lemma_freq = scale(log(lemma_freq_corrected)),
    nsyllable = ordered(nsyl),
    derivational_suffix = case_when(
      is.na(derivational) ~ 'no suffix',
      T ~ derivational
    ) %>% 
      fct_relevel('no suffix')
  )

fit2 = glmer(cbind(freq_1,freq_2) ~ 1 + s_log_lemma_freq + nsyl + derivational_suffix + (1|stem), data = ep, family = binomial, control = glmerControl(optimizer = "bobyqa"))
tidy(fit2,conf.int=T)
plot_model(fit2, 'est') +
  theme_bw()
plot_model(fit2, 'pred', terms = c('s_log_lemma_freq','nsyl [1,3,6]','derivational_suffix')) +
  theme_bw() +
  ylab('% dohányzik') +
  scale_colour_colorblind() +
  scale_fill_colorblind() +
  ggtitle('Predicted probabilities, CC/CVC') +
  labs(colour = 'n syllables')
ggsave('~/Documents/lectures_apps/lectures/ESPtalks/Budapest_talk/corpus2.pdf', width = 6, height = 6)
plot_model(fit2, 'pred', terms = c('s_log_lemma_freq','derivational_suffix')) +
  theme_bw() +
  ylab('% dohányzik') +
  scale_colour_colorblind() +
  scale_fill_colorblind() +
  ggtitle('Predicted probabilities, CC/CVC') +
  labs(colour = 'n syllables')
ggsave('~/Documents/lectures_apps/lectures/ESPtalks/Budapest_talk/corpus2b.pdf', width = 4, height = 4)

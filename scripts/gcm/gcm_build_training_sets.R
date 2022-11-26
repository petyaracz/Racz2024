# -- setup -- #

setwd('~/Github/Racz2024/')

library(tidyverse)
library(magrittr)
library(glue)
library(patchwork)

source('scripts/gcm/purrrgcm.R')
source('scripts/gcm/transcribe.R')

# -- data -- #

b = read_tsv('exp_data/baseline/baseline_tidy_proc.tsv')
lakok = read_tsv('resource/real_words/ik_verbs/ikes_pairs_webcorpus2.tsv')
cselekszik = read_tsv('resource/real_words/epenthetic_stems/epenthesis_pairs_webcorpus2.tsv')

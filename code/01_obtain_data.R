library(magrittr)
library(stringi)
library(stringr)
library(tidyverse)
library(tm)

load("./data_trecenti/d_cjpg_final2.rda")
load("./data_trecenti/d_cjpg_txt.rda")

d_cjpg_final2 = as_tibble(d_cjpg_final2) # BD with clean data
d_cjpg_txt = as_tibble(d_cjpg_txt)       # BD with decision text

#############
# Join BD's #
#############

# n_processo: variable has repetitions in d_cjpg_txt.
# cod_sentenca: variavel without repetitions used for inner_join.
d_cjpg = d_cjpg_final2 %>% inner_join(d_cjpg_txt, c("cod_sentenca"))
d_cjpg %<>% select(-n_processo.y, -foro.y, -serasa, -consumo, -gratuidade, 
                   foro = foro.x,
                   n_processo = n_processo.x)

###############
# Text mining #
###############

# 1. Clean decision texts
d_cjpg %<>% mutate(txt_limpo=tolower(txt))
banned.words <- c(stopwords("portuguese"))
d_cjpg %<>% mutate(txt_limpo = removeWords(txt_limpo, banned.words))
d_cjpg %<>% mutate(txt_limpo = stri_trans_general(txt_limpo, "Latin-ASCII"))
d_cjpg %<>% mutate(txt_limpo = gsub("[\f,\n,\r]", "", txt_limpo))
d_cjpg %<>% mutate(txt_limpo = gsub("[[:digit:][:punct:]]", "", txt_limpo))
d_cjpg %<>% mutate(txt_limpo=stripWhitespace(txt_limpo))

saveRDS(d_cjpg, "./data/data_with_txt.rds")

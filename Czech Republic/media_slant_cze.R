##### Media Slant - CZE



library(tidyverse)
library(data.table)



# Database Creation

setwd("D:/media_slant/data/Czechia")
mediafileslist<-list.files(recursive = F)

media_data_cz <- mediafileslist %>%
  map(fread) 

media_database_cz <- media_data_cz %>%
  map(~select(.,id, domain = Domain, date = Created, text =`Content of posts`)) %>%
  reduce(rbind)

media_database_cz %>%
  group_by(domain) %>%
  tally()

media_database_cz <- media_database_cz %>%
  mutate(domain = str_remove(domain, "www."))

media_database_cz %>%
  group_by(domain) %>%
  tally()

write.csv2(media_database_cz, "D:/media_slant/database/media_database_cz.csv")

# Pre-Processing for LDA

library(quanteda)
library(quanteda.textmodels)
library(seededlda)
library(tm)

lda_database_cz <- media_database_cz %>%
  mutate(text = stringi::stri_replace_all_fixed(text, ".", " ")) %>%
  mutate(text = stringi::stri_replace_all_fixed(text, "@", "")) %>%
  mutate(text = stringi::stri_replace_all_fixed(text, "#", "")) %>%
  mutate(text = stringi::stri_replace_all_fixed(text, "—", " ")) %>%
  mutate(text = stringi::stri_replace_all_fixed(text, "-", " ")) %>%
  mutate(text = stringi::stri_replace_all_fixed(text, "–", " ")) %>%
  mutate(text = stringi::stri_replace_all_fixed(text, "/n", " ")) %>%
  mutate(text = removeNumbers(text)) %>%
  mutate(text = removePunctuation(text))



corpus_cze <- corpus(lda_database_cz)

summary(corpus_cze)

toks_cze <- corpus_cze %>%
  tokens(remove_punct = T,
         remove_numbers = T,
         remove_url = T,
         remove_separators = T,
         remove_symbols = T)

#toks_cze <- tokens_replace(toks_cze, 
#               pattern = types(toks_cze), 
#               replacement = stringi::stri_replace_all_fixed(types(toks_cze), ".", " "),
#               valuetype = "fixed") %>%
#  tokens()

toks_cze <- toks_cze %>%
  tokens_remove(stopwords::stopwords(language="cs", source ="stopwords-iso"))


dfm_cze <- toks_cze %>%
  dfm()

dfm_cze <- dfm_cze %>%
  dfm_trim(min_termfreq = 20, min_docfreq = 5)

vocabulary_cze <- as_tibble(dfm_cze@Dimnames[["features"]])

write.csv2(vocabulary_cze, "D:/media_slant/database/media_vocabulary_cz.csv")



lemma_cze <- read_csv("D:/media_slant/database/media_vocabulary_lemma_cz.csv", 
                                      col_types = cols(...1 = col_skip()))
feat %>%
  as_tibble() %>%
  drop_na()

lemma_cze <- lemma_cze %>%
  mutate(lemma = replace_na(lemma, "missing"))

feat <- featnames(dfm_cze)
featlemma <- lemma_cze$lemma
dfm_cze2 <- dfm_replace(dfm_cze, pattern = feat, replacement = featlemma, case_insensitive = T)


# LDA Analysis

dfm_a2larm <- dfm_cze2 %>%
  dfm_subset(domain == "a2larm.cz") %>%
  dfm_trim(min_termfreq = 1)

a2larm_lda <- textmodel_lda(dfm_a2larm, k = 30, batch_size = 0.01, verbose = T, auto_iter = T)

terms(a2larm_lda, n=20)

dfm_aktualne <- dfm_cze2 %>%
  dfm_subset(domain == "aktualne.cz") %>%
  dfm_trim(min_termfreq = 1)

aktualne_lda <- textmodel_lda(dfm_aktualne, k = 30, batch_size = 0.01, verbose = T, auto_iter = T)

terms(aktualne_lda, n=20)

dfm_blesk <- dfm_cze2 %>%
  dfm_subset(domain == "blesk.cz") %>%
  dfm_trim(min_termfreq = 1)

blesk_lda <- textmodel_lda(dfm_blesk, k = 30, batch_size = 0.01, verbose = T, auto_iter = T)

terms(blesk_lda, n=20)

dfm_idnes <- dfm_cze2 %>%
  dfm_subset(domain == "idnes.cz") %>%
  dfm_trim(min_termfreq = 1)

idnes_lda <- textmodel_lda(dfm_idnes, k = 30, batch_size = 0.01, verbose = T, auto_iter = T)

terms(idnes_lda, n=20)

dfm_info <- dfm_cze2 %>%
  dfm_subset(domain == "info.cz") %>%
  dfm_trim(min_termfreq = 1)

info_lda <- textmodel_lda(dfm_info, k = 30, batch_size = 0.01, verbose = T, auto_iter = T)

terms(info_lda, n=20)

dfm_irozhlas <- dfm_cze2 %>%
  dfm_subset(domain == "irozhlas.cz") %>%
  dfm_trim(min_termfreq = 1)

irozhlas_lda <- textmodel_lda(dfm_irozhlas, k = 30, batch_size = 0.01, verbose = T, auto_iter = T)

terms(irozhlas_lda, n=20)

dfm_novinky <- dfm_cze2 %>%
  dfm_subset(domain == "novinky.cz") %>%
  dfm_trim(min_termfreq = 1)

novinky_lda <- textmodel_lda(dfm_novinky, k = 30, batch_size = 0.01, verbose = T, auto_iter = T)

terms(novinky_lda, n=20)

dfm_parlamentnilisty <- dfm_cze2 %>%
  dfm_subset(domain == "parlamentnilisty.cz") %>%
  dfm_trim(min_termfreq = 1)

parlamentnilisty_lda <- textmodel_lda(dfm_parlamentnilisty, k = 30, batch_size = 0.01, verbose = T, auto_iter = T)

terms(parlamentnilisty_lda, n=20)

dfm_seznamzpravy <- dfm_cze2 %>%
  dfm_subset(domain == "seznamzpravy.cz") %>%
  dfm_trim(min_termfreq = 1)

seznamzpravy_lda <- textmodel_lda(dfm_seznamzpravy, k = 30, batch_size = 0.01, verbose = T, auto_iter = T)

terms(seznamzpravy_lda, n=20)


save(list=c("a2larm_lda", "aktualne_lda", "blesk_lda", "idnes_lda", "info_lda", "irozhlas_lda", "novinky_lda", "parlamentnilisty_lda", "seznamzpravy_lda", "dfm_cze2"), file = "D:/media_slant/lda_cze.Rdata")





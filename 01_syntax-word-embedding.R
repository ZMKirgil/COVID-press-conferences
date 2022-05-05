# 01_syntax-word-embedding 
# by Z.M.Kirgil
# Code is based on https://cbail.github.io/textasdata/word2vec/rmarkdown/word2vec.html by Chris Bail

# Libraries
library(tidytext)
library(tidyverse)
data("stop_words")
library(widyr)

library(SnowballC)
library(readr)
library(textmineR)

# viz
library(ggplot2)
library(ggthemes)
library(ggpubr)
library(ggwordcloud)

# Load data -------------------------------------------------------
load("cov19_CA.rda")
load("cov19_FL.rda")
load("cov19_ME.rda")
load("cov19_MD.rda")
load("cov19_NV.rda")
load("cov19_NJ.rda")
load("cov19_NY.rda")
load("cov19_OH.rda")
load("cov19_TX.rda")

# Preparation -----------------------------------------------------
stopwords <- stop_words %>%
  select(word) %>%
  filter(!str_detect(word, 'we'),
         !str_detect(word, 'our'))

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# California -----------------------------------------------------
covid19_CA_skip <- cov19_CA %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 8) %>%
  mutate(ngramID = row_number()) %>% 
  tidyr::unite(skipgramID, line, ngramID) %>%
  unnest_tokens(word, ngram) %>%
  filter(!str_detect(word, "[:punct:]|[:digit:]")) %>%
  anti_join(stopwords) 
  
unigram_probs_CA <- cov19_CA %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

skipgram_probs_CA <- covid19_CA_skip %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

normalized_prob_CA <- skipgram_probs_CA %>%
  filter(n > 20) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs_CA %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigram_probs_CA %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2)

## Collective Intentionality ---------------------------------------------------
# WE
we_CA <- normalized_prob_CA %>% 
  filter(word1 == "we") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "CA",
         pol = "Democrat", 
         p_together = normalize(p_together)) 

# OUR
our_CA <- normalized_prob_CA %>% 
  filter(word1 == "our") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "CA",
         pol = "Democrat", 
         p_together = normalize(p_together)) 

# People
people_CA <- normalized_prob_CA %>% 
  filter(word1 == "people") %>%
  arrange(-p_together) %>% 
  select(word1, word2, p_together) %>%
  mutate(state = "CA",
         pol = "Democrat", # democrat
         p_together = normalize(p_together))

# Californians
californians_CA <- normalized_prob_CA %>% 
  filter(word1 == "californians") %>%
  arrange(-p_together) %>% 
  select(word1, word2, p_together) %>%
  mutate(state = "CA",
         pol = "Democrat", # democrat
         p_together = normalize(p_together))


## Composite CI ----------------------------------------------------------------
CI_CA <- bind_rows(we_CA, our_CA, people_CA, californians_CA)


# Florida -----------------------------------------------------
covid19_FL_skip <- cov19_FL %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 8) %>%
  mutate(ngramID = row_number()) %>% 
  tidyr::unite(skipgramID, line, ngramID) %>%
  unnest_tokens(word, ngram) %>%
  filter(!str_detect(word, "[:punct:]|[:digit:]")) %>%
  anti_join(stopwords) 

unigram_probs_FL <- cov19_FL %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

skipgram_probs_FL <- covid19_FL_skip %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

normalized_prob_FL <- skipgram_probs_FL %>%
  filter(n > 20) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs_FL %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigram_probs_FL %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2)

## Collective Intentionality ----------------------------------------------------
we_FL <- normalized_prob_FL %>% 
  filter(word1 == "we") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "FL",
         pol = "Republican",
         p_together = normalize(p_together)) 

our_FL <- normalized_prob_FL %>% 
  filter(word1 == "our") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "FL",
         pol = "Republican",
         p_together = normalize(p_together)) 

people_FL <- normalized_prob_FL %>% 
  filter(word1 == "people") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "FL",
         pol = "Republican",
         p_together = normalize(p_together)) 

floridians_FL <- normalized_prob_FL %>% 
  filter(word1 == "floridians") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "FL",
         pol = "Republican",
         p_together = normalize(p_together)) 

florida_FL <- normalized_prob_FL %>% 
  filter(word1 == "florida") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "FL",
         pol = "Republican",
         p_together = normalize(p_together))

## Composite CI ----------------------------------------------------------------

CI_FL <- bind_rows(we_FL, our_FL, people_FL, floridians_FL) # no results, florida, floridians


# Maine -----------------------------------------------------
covid19_ME_skip <- cov19_ME %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 8) %>%
  mutate(ngramID = row_number()) %>% 
  tidyr::unite(skipgramID, line, ngramID) %>%
  unnest_tokens(word, ngram) %>%
  filter(!str_detect(word, "[:punct:]|[:digit:]")) %>%
  anti_join(stopwords)

unigram_probs_ME <- cov19_ME %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

skipgram_probs_ME <- covid19_ME_skip %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

normalized_prob_ME <- skipgram_probs_ME %>%
  filter(n > 20) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs_ME %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigram_probs_ME %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2)

## Collective Intentionality ---------------------------------------------------

we_ME <- normalized_prob_ME %>% 
  filter(word1 == "we") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "ME",
         pol = "Democrat",
         p_together = normalize(p_together))

our_ME <- normalized_prob_ME %>% 
  filter(word1 == "our") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "ME",
         pol = "Democrat",
         p_together = normalize(p_together))

people_ME <- normalized_prob_ME %>% 
  filter(word1 == "people") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "ME",
         pol = "Democrat",
         p_together = normalize(p_together))

mainers_ME <- normalized_prob_ME %>% 
  filter(word1 == "mainers") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "ME",
         pol = "Democrat",
         p_together = normalize(p_together))

## Composite CI -----------------------------------------------------------------
CI_ME <- bind_rows(we_ME, our_ME, people_ME, mainers_ME)


# Maryland ---------------------------------------------------------------------
covid19_MD_skip <- cov19_MD %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 8) %>%
  mutate(ngramID = row_number()) %>% 
  tidyr::unite(skipgramID, line, ngramID) %>%
  unnest_tokens(word, ngram) %>%
  filter(!str_detect(word, "[:punct:]|[:digit:]")) %>%
  anti_join(stopwords) 

unigram_probs_MD <- cov19_MD %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

skipgram_probs_MD <- covid19_MD_skip %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

normalized_prob_MD <- skipgram_probs_MD %>%
  filter(n > 20) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs_MD %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigram_probs_MD %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2)

## Collective Intentionality ---------------------------------------------------
we_MD <- normalized_prob_MD %>% 
  filter(word1 == "we") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "MD",
         pol = "Republican",
         p_together = normalize(p_together)) 

our_MD <- normalized_prob_MD %>% 
  filter(word1 == "our") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "MD",
         pol = "Republican",
         p_together = normalize(p_together)) 

people_MD <- normalized_prob_MD %>% 
  filter(word1 == "people") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "MD",
         pol = "Republican",
         p_together = normalize(p_together)) 

mary_MD <- normalized_prob_MD %>% 
  filter(word1 == "marylanders") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "MD",
         pol = "Republican",
         p_together = normalize(p_together)) 

## Composite CI ----------------------------------------------------------------
CI_MD <- bind_rows(we_MD, our_MD, people_MD, mary_MD)


# Nevada -----------------------------------------------------
covid19_NV_skip <- cov19_NV %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 8) %>%
  mutate(ngramID = row_number()) %>% 
  tidyr::unite(skipgramID, line, ngramID) %>%
  unnest_tokens(word, ngram) %>%
  filter(!str_detect(word, "[:punct:]|[:digit:]")) %>%
  anti_join(stopwords)

unigram_probs_NV <- cov19_NV %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

skipgram_probs_NV <- covid19_NV_skip %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

normalized_prob_NV <- skipgram_probs_NV %>%
  filter(n > 20) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs_NV %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigram_probs_NV %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2)

## Collective Intentionality ---------------------------------------------------
we_NV <- normalized_prob_NV %>% 
  filter(word1 == "we") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "NV",
         pol = "Democrat",
         p_together = normalize(p_together)) 

our_NV <- normalized_prob_NV %>% 
  filter(word1 == "our") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "NV",
         pol = "Democrat",
         p_together = normalize(p_together))

people_NV <- normalized_prob_NV %>% 
  filter(word1 == "people") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "NV",
         pol = "Democrat",
         p_together = normalize(p_together))

nevadan_NV <- normalized_prob_NV %>% 
  filter(word1 == "nevadan") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "NV",
         pol = "Democrat",
         p_together = normalize(p_together))

## Composite CI ----------------------------------------------------------------
CI_NV <- bind_rows(we_NV, our_NV, people_NV) # -> no results

# New Jersey -------------------------------------------------------------------
covid19_NJ_skip <- cov19_NJ %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 8) %>%
  mutate(ngramID = row_number()) %>% 
  tidyr::unite(skipgramID, line, ngramID) %>%
  unnest_tokens(word, ngram) %>%
  filter(!str_detect(word, "[:punct:]|[:digit:]")) %>%
  anti_join(stopwords)

unigram_probs_NJ <- cov19_NJ %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

skipgram_probs_NJ <- covid19_NJ_skip %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

normalized_prob_NJ <- skipgram_probs_NJ %>%
  filter(n > 20) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs_NJ %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigram_probs_NJ %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2)

## Collective Intentionality ---------------------------------------------------
we_NJ <- normalized_prob_NJ %>% 
  filter(word1 == "we") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "NJ",
         pol = "Democrat",
         p_together = normalize(p_together)) 

our_NJ <- normalized_prob_NJ %>% 
  filter(word1 == "our") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "NJ",
         pol = "Democrat",
         p_together = normalize(p_together))

people_NJ <- normalized_prob_NJ %>% 
  filter(word1 == "people") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "NJ",
         pol = "Democrat",
         p_together = normalize(p_together))

jerseyan_NJ <- normalized_prob_NJ %>% 
  filter(word1 == "jerseyan") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "NJ",
         pol = "Democrat",
         p_together = normalize(p_together))

## Composite CI ----------------------------------------------------------------
CI_NJ <- bind_rows(we_NJ, our_NJ, people_NJ, jerseyan_NJ)


# New York ---------------------------------------------------------------------
covid19_NY_skip <- cov19_NY %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 8) %>%
  mutate(ngramID = row_number()) %>% 
  tidyr::unite(skipgramID, line, ngramID) %>%
  unnest_tokens(word, ngram) %>%
  filter(!str_detect(word, "[:punct:]|[:digit:]")) %>%
  anti_join(stopwords)

unigram_probs_NY <- cov19_NY %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

skipgram_probs_NY <- covid19_NY_skip %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

normalized_prob_NY <- skipgram_probs_NY %>%
  filter(n > 20) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs_NY %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigram_probs_NY %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2)

## Collective Intentionality ---------------------------------------------------
we_NY <- normalized_prob_NY %>% 
  filter(word1 == "we") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "NY",
         pol = "Democrat",
         p_together = normalize(p_together)) 

our_NY <- normalized_prob_NY %>% 
  filter(word1 == "our") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "NY",
         pol = "Democrat",
         p_together = normalize(p_together))

people_NY <- normalized_prob_NY %>% 
  filter(word1 == "people") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "NY",
         pol = "Democrat",
         p_together = normalize(p_together))

yorker_NY <- normalized_prob_NY %>% 
  filter(word1 == "yorker") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "NY",
         pol = "Democrat",
         p_together = normalize(p_together))

## Composite CI ----------------------------------------------------------------
CI_NY <- bind_rows(we_NY, our_NY, people_NY, yorker_NY)


# Ohio -------------------------------------------------------------------------
covid19_OH_skip <- cov19_OH %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 8) %>%
  mutate(ngramID = row_number()) %>% 
  tidyr::unite(skipgramID, line, ngramID) %>%
  unnest_tokens(word, ngram) %>%
  filter(!str_detect(word, "[:punct:]|[:digit:]")) %>%
  anti_join(stopwords)

unigram_probs_OH <- cov19_OH %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

skipgram_probs_OH <- covid19_OH_skip %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

normalized_prob_OH <- skipgram_probs_OH %>%
  filter(n > 20) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs_OH %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigram_probs_OH %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2)

## Collective Intentionality ---------------------------------------------------
we_OH <- normalized_prob_OH %>% 
  filter(word1 == "we") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "OH",
         pol = "Republican",
         p_together = normalize(p_together)) 

our_OH <- normalized_prob_OH %>% 
  filter(word1 == "our") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "OH",
         pol = "Republican",
         p_together = normalize(p_together))


people_OH <- normalized_prob_OH %>% 
  filter(word1 == "people") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "OH",
         pol = "Republican",
         p_together = normalize(p_together))

ohian_OH <- normalized_prob_OH %>% 
  filter(word1 == "ohian") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "OH",
         pol = "Republican",
         p_together = normalize(p_together))

## Composite CI ----------------------------------------------------------------
CI_OH <- bind_rows(we_OH, our_OH, people_OH, ohian_OH)


# Texas ------------------------------------------------------------------------
covid19_TX_skip <- cov19_TX %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 8) %>%
  mutate(ngramID = row_number()) %>% 
  tidyr::unite(skipgramID, line, ngramID) %>%
  unnest_tokens(word, ngram) %>%
  filter(!str_detect(word, "[:punct:]|[:digit:]")) %>%
  anti_join(stopwords)

unigram_probs_TX <- cov19_TX %>%
  unnest_tokens(word, text) %>%
  count(word, sort = TRUE) %>%
  mutate(p = n / sum(n))

skipgram_probs_TX <- covid19_TX_skip %>%
  pairwise_count(word, skipgramID, diag = TRUE, sort = TRUE) %>%
  mutate(p = n / sum(n))

normalized_prob_TX <- skipgram_probs_TX %>%
  filter(n > 20) %>%
  rename(word1 = item1, word2 = item2) %>%
  left_join(unigram_probs_TX %>%
              select(word1 = word, p1 = p),
            by = "word1") %>%
  left_join(unigram_probs_TX %>%
              select(word2 = word, p2 = p),
            by = "word2") %>%
  mutate(p_together = p / p1 / p2)

## Collective Intentionality ---------------------------------------------------
we_TX <- normalized_prob_TX %>% 
  filter(word1 == "we") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "TX",
         pol = "Republican",
         p_together = normalize(p_together)) 

our_TX <- normalized_prob_TX %>% 
  filter(word1 == "our") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "TX",
         pol = "Republican",
         p_together = normalize(p_together))

people_TX <- normalized_prob_TX %>% 
  filter(word1 == "people") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "TX",
         pol = "Republican",
         p_together = normalize(p_together))

texans_TX <- normalized_prob_TX %>% 
  filter(word1 == "texans") %>%
  arrange(-p_together) %>%
  select(word1, word2, p_together) %>%
  mutate(state = "TX",
         pol = "Republican",
         p_together = normalize(p_together))

## Composite CI ----------------------------------------------------------------
CI_TX <- bind_rows(we_TX, our_TX, people_TX, texans_TX)


# Merge ------------------------------------------------------------------------
CI_full <- bind_rows(CI_CA, CI_FL, CI_MD, CI_ME, CI_NJ, CI_NV, CI_NY, 
                       CI_OH, CI_TX)

save(CI_full, file = "03_CI_corpus_data.R")

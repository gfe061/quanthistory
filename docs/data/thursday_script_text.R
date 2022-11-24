library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(tidytext)

setwd("P:/Work/Stellen/Innlandet/2022/Quantifying history/code_website/WorkshopMaterials")
nobel <- read_rds("nobel_cleaned.Rds")
Sys.setlocale("LC_ALL", "Norwegian")

## network text analysis using quanteda
# need fcm
nobel_fcm <- nobel %>%
  corpus(text_field = 'AwardSpeech') %>%
  tokens(remove_numbers = TRUE, remove_punc = TRUE) %>%
  tokens_remove(pattern = stop_words$word) %>%
  fcm(context = "window", tri = FALSE, window = 20)

top <- names(topfeatures(nobel_fcm, 50))
fcm_select(nobel_fcm, pattern = top) %>%
  textplot_network(min_freq = .95)

## POS (spacyr)
library("spacyr")
spacy_initialize(condaenv = "C:\\Users\\6041041\\Anaconda3")
spacy_parse(nobel$AwardSpeech[1], tag = TRUE, entity = FALSE, lemma = FALSE)

text <-spacy_parse(nobel$AwardSpeech[1], lemma = FALSE, entity = TRUE, nounphrase = TRUE)
# Recognizes multi-word phrases
entity_consolidate(text)
nounphrase_extract(text)
nounphrase_extract(parsedtxt)nounphrase_extract(parsedtxt)

# most frequent verbs and nouns
tokens <- as.tokens(text, include_pos = "pos")
nouns <- tokens_select(tokens, pattern = "*/NOUN") %>%
  dfm() %>%
  textstat_frequency(n = 10)
verbs <- tokens_select(tokens, pattern = "*/VERB") %>%
  dfm() %>%
  textstat_frequency(n=10)


## Word embedding

## Word2Vec

# install.packages("devtools")
# devtools::install_github("bmschmidt/wordVectors")
library(wordVectors)
library(magrittr)

write_lines(nobel$AwardSpeech, "nobel.txt")
prep_word2vec(origin="nobel.txt",destination="nobel_prep.txt",lowercase=TRUE,bundle_ngrams=2)
model <- train_word2vec("nobel_prep.txt","nobel_vectors.bin", vectors = 200, threads = 4 , 
    window = 10, iter = 5, negative_samples = 10, force = TRUE)

model %>% closest_to("peace", n = 15)
model %>% closest_to(~"king"+"woman"-"man")
model %>% closest_to(~"bomb" - "war" + "peace")
model %>% closest_to(~"war" - "peace" + "education")

peace <- model[rownames(model) == "peace"]
violence <- model[rownames(model) == "violence"]
pv_spectrum <- peace-violence
cosineSimilarity(pv_spectrum, model[[""]])

violation <- model %>% 
  closest_to(~ "violation"-"rights",n=Inf) 
war <- model %>% 
  closest_to(~ "war" - "peace", n=Inf)
politics <- model %>%
  closest_to("politics", n = 200)

politics %>%
  inner_join(violation) %>%
  inner_join(war) %>%
  ggplot() + 
  geom_text(aes(x=`similarity to "violation" - "rights"`,
                y=`similarity to "war" - "peace"`,
                label=word)) +
  labs(x = "Rights <----> Violation", y = "Peace <-----> War")

peacewords <- model %>% closest_to("peace", n = 50)
peace <- model[[peacewords$word,average=F]]
plot(peace,method="pca")

plot(model,perplexity=50)

## GloVe
library(text2vec)
glove <- GlobalVectors$new(rank = 50, x_max = 10)
wv_main <- glove$fit_transform(nobel_fcm, n_iter = 10, convergence_tol = 0.01, n_threads = 8)
word_vectors = wv_main + t(glove$components)

x <- word_vectors["war", , drop = FALSE] - 
  word_vectors["bomb", , drop = FALSE] + 
  word_vectors["peace", , drop = FALSE]
cos_sim = sim2(x = word_vectors, y = x, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 20)

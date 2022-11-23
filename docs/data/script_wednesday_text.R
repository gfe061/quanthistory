#### Wednesday, Nov. 23, quantitative text session

library(tidyverse)
library(tidytext)
library(stm)
library(topicmodels)
library(quanteda)
library(quanteda.textstats)

# load nobel
setwd("P:/Work/Stellen/Innlandet/2022/Quantifying history")
nobel <- read_rds("nobel_cleaned.Rds")

#### From yesterday

#### Looking for top frequency n-grams

nobel %>%
  unnest_tokens(twogram, AwardSpeech, token = "ngrams", n = 2)

## stopwording
nob <- nobel %>%
  mutate(Period = ifelse(Year <= 1945, "Pre-WWII", "Post-WWII")) %>% 
  unnest_tokens(twogram, AwardSpeech, token = "ngrams", n = 2) %>%
  separate(twogram, into=c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  unite(twogram, word1, word2, sep = ' ') %>%              # now putting word1 and word2 back into a single column called twogram
  group_by(Period) %>%
  count(twogram, sort=TRUE) %>%
  slice_max(order_by=n, n = 10)  

nob %>%
  mutate(twogram = reorder(twogram, n)) %>%
  ungroup() %>%
  ggplot(aes(reorder_within(x = twogram, by = n, within = Period), n, fill = Period)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +  
  coord_flip() +
  facet_wrap(~Period, ncol = 2, scales = "free") +
  ylab('n') +
  xlab("2-gram")

#### co-occurence
nobel %>%
  unnest_tokens(word, AwardSpeech) %>%
  filter(!word %in% stop_words$word) %>%
  pairwise_count(word, Year, sort = TRUE)


# tfidf by decade
tfidf_dec <- nobel %>%
  mutate(Period = Year %/% 10 * 10) %>%
  unnest_tokens(words, AwardSpeech) %>%
  count(words, Period, sort = TRUE) %>%
  bind_tf_idf(words, Period, n) %>%
  group_by(Period) %>%
  slice_max(tf_idf, n = 7) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(words, tf_idf), fill = Period)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Period, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

# sentiment by afinn
# using the afinn lexicon
nobel %>%
  unnest_tokens(word, AwardSpeech) %>%  ## we call our new column "word" which makes inner_joins easier 
  inner_join(get_sentiments("afinn")) %>%
  group_by(Year) %>%
  summarize(sentiment = sum(value)) %>%
  ggplot(aes(Year, sentiment)) +
  geom_line(show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = 2, alpha = .8)


####### Cosine distance

## Cosine distance, by decade
nobel_decade <- nobel %>%
  mutate(decade = Year %/% 10 * 10)
nobel_dfm <- nobel_decade %>%
  corpus(text_field = 'AwardSpeech') %>%
  tokens(remove_numbers = TRUE, remove_punc = TRUE) %>%
  dfm() %>%
  dfm_remove(pattern = stop_words$word) %>%
  dfm_group(groups = decade)
cosine_diff <- textstat_simil(nobel_dfm, method = "cosine")

# editing the dfm
dfm_small <- dfm_trim(nobel_dfm, min_termfreq = 3, min_docfreq = 2)

# clustering
eu_dist <- textstat_dist(dfm_weight(dfm_small, scheme = "prop"))
# hiarchical clustering using Euclidean distance -- hclust()
cluster <- hclust(as.dist(eu_dist))
cluster$labels <- docnames(nobel_dfm)
# plot as a dendrogram
plot(cluster, xlab = "", sub = "", main = "Clustered Euclidean Distance")

# Keyness
keyness <- textstat_keyness(nobel_decade, target = nobel_decade$decade >= 1945)
textplot_keyness(keyness)


##### TOPIC MODELS #########

nobel_stemmed <- nobel %>%
  unnest_tokens(output = words, input = AwardSpeech) %>%
  anti_join(stop_words, by = c("words" = "word")) %>%
  mutate(word_stem = wordStem(words))
  #rename(Year = Year, Laureate = Laureate, word = word_stem)

# transform dataframe to DTM
nobel_dtm <- nobel_stemmed %>%
  group_by(Year) %>%
  count(word_stem, sort = TRUE) %>%
  cast_dtm(Year, word_stem, n)

k = 15
alpha = 2

nobel_tm <- LDA(nobel_dtm, k = k, alpha = alpha)

terms(nobel_tm, 15)

# go back to tidy to look at this -- beta
terms <- tidy(nobel_tm, matrix = "beta")
words_in_topics <- terms %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)
words_in_topics %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

## -- theta dist
topics_in_documents <- tidy(nobel_tm, matrix = "gamma")
(auto_topics <- apply(terms(nobel_tm, 3), 2, paste, collapse = "-"))
(auto_topics <- tibble(old_topic = 1:k, new_topic = auto_topics))
(topics <- topics_in_documents %>%
    left_join(auto_topics, by=c("topic" = "old_topic")))

topics %>%
  filter(document %in% c(1977, 1985, 1996)) %>%  # the documents we want to compare
  ggplot(aes(new_topic, gamma, fill = document)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ document, ncol = 3)

topics %>%
  ggplot(aes(document, gamma)) +
  geom_col(aes(group = new_topic, fill = new_topic)) +
  scale_x_discrete(breaks = seq(1905, 2019, 10))

topics %>%
  #filter(str_detect(new_topic, "war")) %>%
  ggplot(aes(document, gamma)) +
  geom_line(aes(group = new_topic, color = new_topic)) +
  #geom_line(aes(group = new_topic, color = new_topic)) +
  scale_x_discrete(breaks = seq(1905, 2019, 10)) +
  facet_wrap(~new_topic)

#### STM

nobel_decade <- nobel %>%
  mutate(decade = Year %/% 10 * 10) %>%
  mutate(Period = ifelse(Year <= 1945, "Pre-WWII", "Post-WWII")) %>%
  corpus(text_field = 'AwardSpeech') %>%
  tokens(remove_numbers = TRUE, remove_punc = TRUE) %>%
  dfm() %>%
  dfm_remove(pattern = stop_words$word) %>%
  dfm_group(groups = decade)
fit10 <- stm(nobel_decade, K = 10, max.em.its = 5, init.type = "Spectral")
plot(fit10)

nobel_periods <- nobel %>%
  #mutate(decade = Year %/% 10 * 10) %>%
  mutate(Period = ifelse(Year <= 1945, "Pre-WWII", "Post-WWII")) %>%
  corpus(text_field = 'AwardSpeech') %>%
  tokens(remove_numbers = TRUE, remove_punc = TRUE) %>%
  dfm() %>%
  dfm_remove(pattern = stop_words$word)
# setting EM iterations to 10 for speed -- normally this should be many more (often set at 75)
fit10_period <- stm(nobel_periods, K = 10, content =~Period, prevalence =~ Period, max.em.its = 5, init.type = "Spectral")
plot(fit10_period)

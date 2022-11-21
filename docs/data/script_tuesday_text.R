### Session 2.2, Tuesday, Nov. 22

# Lets use the first speech in our Nobel corpus as an example
library(tidytext)
library(tidyverse)
library(stopwords)
library(SnowballC)
library(widyr)

## Tidy data

nobel %>%
  mutate(peace = str_count(AwardSpeech, "[Pp]eace")) %>%
  mutate(war = str_count(AwardSpeech, "[Ww]ar")) %>%
  mutate(thankyou = str_count(AwardSpeech, "[Tt]ank [Yy]ou")) %>%
  # ggplot() +
  # geom_line(aes(x = Year, y = peace), color = "green") +
  # geom_line(aes(x = Year, y = thankyou), color = "blue") +
  # geom_line(aes(x = Year, y = war), color = "red")
  pivot_longer(c("peace", "war", "thankyou"), names_to = "word", values_to = "counts") %>%
  ggplot(aes(x = Year, y = counts, color = word)) +
  geom_line()

## Not recode, but case_when

nobel %>%
  mutate(wc = str_count(AwardSpeech, "[\\w]+")) %>%
  mutate(period = case_when(
    Year <= 1945 ~ "Pre-cold war",
    Year > 1945 & Year <= 1991 ~ "Cold War",
    Year > 1991 ~ "Post-cold war",
  )) %>%
  group_by(period) %>%
  summarize(mean(wc))

## Other example, on the website, by decade


### Word frequency tables

# Load the data from wherever you have saved it -- here the example shows a "data" folder in my working drive
nobel <- read_rds("data/nobel_cleaned.Rds")

# TIDY TEXT

# unnest_tokens -- show on one example

stop_words

# for other languages see package stopwords.
  
nobel_tidy <- nobel %>%
  unnest_tokens(output = words, input = AwardSpeech) %>%
  anti_join(stop_words, by = c("words" = "word"))

my_words <- c("peace", "war")
custom_stop_words <- tibble(word = my_words, lexicon = "my_customization")
stop_words_custom <- rbind(stop_words, custom_stop_words)
tail(stop_words_custom) 

words_to_stem <- c("going", "represented", "wars", "similarity", "books")
# stem using wordStem

# stem the whole nobel dataframe

# top word frequencies (count words, set sort=TRUE)

nobel_tidy %>%
  count(words, sort=TRUE) %>%
  top_n(15) %>%                               # selecting to show only top 15 words
  #mutate(words = reorder(words,desc(n))) %>%  # this will ensure that the highest frequency words appear to the left
  ggplot(aes(words, n)) +
  geom_col()


nobel_tidy %>%
  mutate(Period = ifelse(Year <= 1945, "Pre-WWII", "Post-WWII")) %>%   
  mutate(Period = factor(Period, levels = c("Pre-WWII", "Post-WWII"))) %>%
  group_by(Period) %>%                                                # grouping by this column label so frequencies will be                                                                              calculated within group
  count(words, sort=TRUE) %>%
  mutate(proportion = n / sum(n) * 1000) %>%                     # perhaps we'd like word frequency per 1000 words rather than raw                                                                      counts?
  slice_max(order_by=proportion, n = 15) %>%                     # selecting to show only top 15 words within each group
  ggplot(aes(reorder_within(x = words, by = proportion, within = Period), y = proportion, fill = Period)) +    # reordering is a bit tricky, see                                                                                                     ?reorder_within()
  geom_col() +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~Period, ncol = 2, scales = "free") +
  labs(title = "Most frequent words in Nobel award speech corpus", subtitle = "Per 1000 words", x = "Word", 
       y = "Proportion")

library(wordcloud)
library(wordcloud2)
nobel_tidy %>%
  count(words, sort=TRUE) %>%
  with(wordcloud(words, n, max.words = 100))

dat <- nobel_tidy %>%
  count(words, sort=TRUE) %>%
  mutate(word = words) %>%
  mutate(freq = n) %>%
  select(word, freq) %>%
  top_n(200)
wordcloud2(dat, size = 1)

#### TF-IDF

# what is tf-idf

# find tf-idfs in R using: bind_tf_idf()

# arrange in descending order:  arrange(desc(tf_idf)) 

tf_idf %>%
  mutate(Period = factor(Period, levels = c("Pre-WWII", "Post-WWII"))) %>%
  group_by(Period) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(words, tf_idf), fill = Period)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Period, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL)

### POS

# copy-paste from website

library(NLP)
library(tm)  # load before openNLP
library(openNLP)

POStagger <- function(text, filter = NULL){
  # defining annotators 
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  # make sure input is a string
  text <- as.String(text)
  # defining a pipeline
  annotate_text <- NLP::annotate(text, Annotator_Pipeline(
    sent_token_annotator,
    word_token_annotator,
    pos_tag_annotator
  ))
  # generate tags
  tags <- sapply(subset(annotate_text, type=="word")$features, `[[`, "POS")
  # get tokens
  tokens <- text[subset(annotate_text, type=="word")]
  # filter out just one pos type
  if (!is.null(filter)) {
    tagged <- tokens[tags %in% filter]
  } else {
    # put tokens and pos tags in df
    tagged <- tibble(word=tokens, pos=tags) %>% 
      filter(!str_detect(pos, pattern='[[:punct:]]'))}
  return(tagged)
}

nobel <- read_rds("data/nobel_cleaned.Rds")
POStagger(nobel$AwardSpeech[1])

# find most frequent verbs


###### Sentiment analysis

get_sentiments("afinn")
tail(get_sentiments("afinn"))

# how do we see the range of spread of sentiment scale?

# summary stats

# same for nrc and bing

# calculating text sentiment by subtracting total positive sentiment words from total negative with bing lexicon
nobel %>%
  unnest_tokens(word, AwardSpeech) %>%  ## we call our new column "word" which makes inner_joins easier 
  inner_join(get_sentiments("bing")) %>%
  count(sentiment, year = Year) %>% 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative) %>%
  ggplot(aes(year, sentiment)) +
    geom_line(show.legend = FALSE) +
    geom_hline(yintercept = 0, linetype = 2, alpha = .8)

# how would we do the same with afinn sentiment dictionary?


#### Make our own custom dictionary

- create txt dictionary
- readLines
- make into tibble dictionary
- run on corp


#### Looking for top frequency n-grams

nobel %>%
  unnest_tokens(twogram, AwardSpeech, token = "ngrams", n = 2)

# stopwording
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
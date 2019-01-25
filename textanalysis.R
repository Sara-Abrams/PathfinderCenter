library(dplyr)
library(tidytext)
library (ggplot2)
library(purrr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(tidyr)
library(igraph)
library(ggraph)
library(topicmodels)
library(tidyr)

data("stop_words")

read_plus <- function(flnm){
  read.csv2(file=flnm, stringsAsFactors=FALSE, sep=",", na.strings=c("N/A")) %>%
    mutate(filename= flnm)  
}

InterviewData <- list.files(pattern="csv") %>% map_df(~read_plus(.))

researcherName <- c("Brandon Dorr", "Chris Frias", "Vasishta Somayaji")

myStopwords <- c('can', 'say','one','way','use',
                  'also','howev','tell','will',
                  'much','need','take','tend','even',
                  'like','particular','rather','said',
                  'get','well','make','ask','come','end',
                  'first','two','help','often','may',
                  'might','see','someth','thing','point',
                  'post','look','right','now','think',''ve ',
                  ''re ','anoth','put','set','new','good',
                  'want','sure','kind','larg','yes,','day','etc',
                  'quit','sinc','attempt','lack','seen','awar',
                  'littl','ever','moreov','though','found','abl',
                  'enough','far','earli','away','achiev','draw',
                  'last','never','brief','bit','entir','brief',
                  'great','lot', "like", "just", "really", "yes", "lot",
                  "really", "day", "know", "think", "things", "going",
                 "realli", "want", "get", "work", "yeah", "something", 
                 "well", "whatever", "whatev", "okay","monday","tuesday",
                 "wednesday", "thursday", "friday", "saturday", "sunday", "always", "alway",
                 "stuff", "maybe", "mayb", "got", "'ll", "'re", "'ve", "'ll", "'ll", "'re",
                "re", "'ve", "'ve", "week", "high", "feel")

ResponseData <- InterviewData[which(!(InterviewData$speakerName %in% researcherName)), "textContent"]

idNum <- unique(ResponseData$speakerName)]

TopicData <-

for(i in idNum){
  TopicData$ID <- i
  TopicData$content <-
}


qC <- VCorpus(VectorSource(InterviewData[which(!(InterviewData$speakerName %in% researcherName)), "textContent"]))

qCC <- qC %>% tm_map(., content_transformer(tolower)) %>%
              tm_map(., removeNumbers) %>%
              tm_map(., removeWords, stopwords("english")) %>%
              tm_map(., removePunctuation) %>%
              tm_map(., stripWhitespace) %>%
              tm_map(., stemDocument) %>%
              tm_map(., removeWords, myStopwords )

      
dtm <- TermDocumentMatrix(qCC)

m <- as.matrix(dtm)
v <-sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
head(d, 100)


wordcloud(d$word, d$freq, min.freq = 40)


#relationship between words? Ngram?

#response <- data.frame(text = d$word)

#r_bigram <- response %>% unnest_tokens(output = bigram, input = text, token = "ngrams", n = 2)

#r_bigram_graph <- r_bigram %>% separate(bigram, c("word1", "word2"), sep = " ")  %>%
#                  count(word1, word2, sort = TRUE) %>%
#                  unite(bigram, c("word1", "word2"), sep = " ") %>% graph_from_data_frame()


#ggraph(r_bigram_graph, layout = "fr") +
#  geom_edge_link() +
#  geom_node_point() +
#  geom_node_text(aes(label=name), vjust = 1, hjust = 1)
#trigram

#qB <- data_frame(text=InterviewData[which(!(InterviewData$speakerName %in% researcherName)), "textContent"])

                  
##Topic Modeling?

int_dtm <- DocumentTermMatrix(qCC)

ui <- unique(int_dtm$i)

int_dtm <- int_dtm[ui,]

int_lda <- LDA(int_dtm, k=2, control= list(seed=1234))

int_topics <-tidy(int_lda, matrix="beta")
int_topics

int_topic_terms <- int_topics %>% group_by(topic) %>%
                  top_n(10, beta) %>%
                  ungroup() %>%
                  arrange(topic, -beta)
int_topic_terms %>% 
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill=factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap( ~ topic, scales = "free") +
  coord_flip ()


beta_spread <- int_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate( log_ratio = log2(topic2/topic1))

beta_spread

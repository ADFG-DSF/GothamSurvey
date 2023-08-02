# Find the most common word and word pairs in the DSF Gotham Culture survey results.

# Author: Adam
# Version: 2023-05-05

# Packages
packs <- c("tidyverse", "stringr", "tidytext", "textdata", "wordcloud", "ggraph")
lapply(packs, require, character.only = TRUE)
  
# Parameters
# Not Applicable

# ============================================================================


# Import text -------------------------------------------------------------

input <- 
  xlsx::read.xlsx("H:\\My Documents\\DLT\\GothamSurvey\\Copy of Sport Fish survey comments_sg.xlsx", 1, as.data.frame = TRUE, header = FALSE) 

# format questions
questions <- 
  input[grepl("^Q\\d.*", input$X1), ] %>%
  sapply(function(x){gsub("\n", " ", x)}) %>%
  setNames(NULL)
q_short <- c("Q21: enhance safety/wellbeing", "Q31: increase retention", "Q35: positive job satisfaction factors", 
             "Q36: negative job satisfaction factors", 
             "Q40: reasons not to report", "Q41: disrespect impact", "Q48: biases oberved", "Q49: bias impact",
             "Q54: schedule, workload, work/life integration")
q_index <- c(which(grepl("^Q\\d.*", input$X1)), length(input$X1) + 1) #add one so that last row has a question
q_vector <- rep(questions, q_index[2:length(q_index)] - q_index[1:(length(q_index) - 1)])

gc <- 
  input %>%
  mutate(question = factor(q_vector, levels = questions, labels = q_short, ordered = TRUE))%>%
  filter(!grepl("^Q\\d.*", X1)) %>%
  tibble(response = seq_along(X1), text = X1) %>% 
  select(-X1) 

gc_word <- 
  gc %>%
  unnest_tokens(word, text)
  
gc_biword <- 
  gc %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)


# Single word analysis -----------------------------------------------------

# * Frequent words -----------------------------------------------------

gc_topwords <-
  gc_word %>%
  anti_join(stop_words) %>% #stop words is a built in data set of the most common words
  group_by(question) %>%
  count(word) %>%
  top_n(11) #11 so we can drop one for the stop criteria

gc_topwords %>%
  group_by(question) %>%
  mutate(stop = min(median(n), min(n))) %>%
  filter(n > stop) %>%
  ggplot(aes(x = n, y = reorder_within(word, n, question), fill = question)) +
    geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) +
    facet_wrap(~ question, ncol = 2, scales = "free_y") +
    xlab("Number of Occurrences") +
    ylab("Most Common Words") +
    scale_y_discrete(labels = function(x) gsub("__.+$", "", x))

# * tf_idf ------------------------------------------------------------------

tf_idf <- 
  gc_word %>%
  anti_join(stop_words) %>%
  count(question, word, sort = TRUE) %>%
  bind_tf_idf(word, question, n) %>%
  arrange(question, desc(tf_idf))


tf_idf %>%
  # We need to sort the data in descending order so we can create the factors for each term
  arrange(desc(tf_idf)) %>%
  group_by(question) %>%
  top_n(11) %>%
  mutate(stop = min(median(n), min(n))) %>%
  filter(n > stop) %>%
  ggplot(mapping = aes(x = tf_idf, y = reorder_within(word, tf_idf, question), fill = question)) +
    geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) +
    facet_wrap(~ question, scales = "free_y") +
    xlab("Weighted Freqency of Occurrence") +
    ylab("Most Common Words") +
    scale_y_discrete(labels = function(x) gsub("__.+$", "", x))

# * Cloud -------------------------------------------------------------------

gc_word %>% 
  anti_join(stop_words) %>% #stop words is a built in data set of the most common words
  group_by(question) %>%
  count(word) %>% 
  with(wordcloud(word, n, min.freq = 10, colors = brewer.pal(8, "Dark2")))

# * sentiment analysis -----------------------------------------------------

words_ncr <- 
  get_sentiments("nrc") %>% 
    pivot_wider(id_cols = word, names_from = sentiment, values_from = sentiment) %>%
    unite(., col = "ncr",  trust:anticipation, na.rm=TRUE, sep = ",")

tf_idf %>%
  group_by(question) %>%
  arrange(question, -tf_idf) %>%
  top_n(10) %>%
  ungroup() %>%
  select(word) %>%
  left_join(get_sentiments("afinn")) %>%
  setNames(c("word", "afinn")) %>%
  left_join(words_ncr) %>%
  filter(!(is.na(afinn) & is.na(ncr))) %>%
  print(n = 100)
  

#bing
gc_word %>%
  inner_join(get_sentiments("bing")) %>%
  count(question, index = response, sentiment) %>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative,
         question = factor(question, labels = q_short)) %>%
  ggplot(aes(index, sentiment, fill = question)) +
    geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) +
    geom_hline(aes(yintercept = 0)) +
    facet_wrap(~ question, ncol = 2, scales = "free")

#afinn
gc_word %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(question, response) %>%
  summarise(value = sum(value)) %>%
  ggplot(aes(response, value, fill = question)) +
  geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) +
  geom_hline(aes(yintercept = 0)) +
  facet_wrap(~ question, ncol = 2, scales = "free")

#ncr
q_shorter <- c("Q21: enhance safety", "Q31: increase retention", "Q35: positive job factors", 
             "Q36: negative job factors", 
             "Q40: reasons not to report", "Q41: disrespect impact", "Q48: biases oberved", 
             "Q49: bias impact",
             "Q54: schedule") #plots better
full_sentiments <-
  expand.grid(question = as.ordered(q_short), 
              sentiment = unique(get_sentiments("nrc")$sentiment), 
              n = 0) %>%
  filter(!(sentiment %in% c("positive", "negative")))
gc_word %>%
  inner_join(get_sentiments("nrc"), by = "word", multiple = "all") %>%
  filter(!(sentiment %in% c("positive", "negative"))) %>%
  group_by(question, sentiment) %>%
  summarize(n = n()) %>%
  full_join(full_sentiments, by = c("question", "sentiment")) %>%
  mutate(n = ifelse(is.na(n.x), n.y, n.x),
         percent = n/sum(n)) %>%
  ggplot(aes(as.numeric(question), percent, fill = sentiment)) +
    geom_area() +
    scale_x_continuous(breaks = 1:9, labels = q_shorter) +
    labs(x = "Question", y = "Percent", fill = "Sentiment") +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 25, vjust = 0.5, hjust=0.5))

# Frequent word pairs -----------------------------------------------------

GC_toppairs <- 
  gc_biword %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !is.na(word1)) %>%
  group_by(question) %>%
  count(word1, word2, sort = TRUE)  %>%
  top_n(6)

GC_toppairs %>%
  group_by(question) %>%
  mutate(word = paste0(word1, " ", word2),
         stop = min(median(n), min(n))) %>%
  filter(n > stop) %>%
  ggplot(aes(x = n, y = reorder_within(word, n, question), fill = question)) +
    geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) +
    facet_wrap(~ question, ncol = 2, scales = "free_y") +
    xlab("Number of Occurrences") +
    ylab("Most Common Word Pairs") +
    scale_y_discrete(labels = function(x) gsub("__.+$", "", x)) 

#for quarto
plot_toppairs <-
  lapply(q_short, function(x){
    GC_toppairs %>%
      filter(question == x) %>%
      mutate(word = paste0(word1, " ", word2),
             stop = min(median(n), min(n))) %>%
      filter(n > stop) %>%
      ggplot(aes(x = n, y = reorder(word, n))) +
        geom_bar(alpha = 0.5, stat = "identity", show.legend = FALSE) +
        xlab("Number of Occurrences") +
        ylab(NULL) +
        ggtitle("Word Pair Frequency")
  })

# * Network analysis --------------------------------------------------------

GC_network <- 
  gc_biword %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  drop_na(word1, word2) %>%
  group_by(question) %>%
  count(word1, word2, sort = TRUE)  %>%
  top_n(6) %>%
  group_by(question) %>%
  mutate(word = paste0(word1, " ", word2),
         stop = min(median(n), min(n))) %>%
  filter(n > stop) %>%
  select(question, word, n) %>%
  igraph::graph_from_data_frame() #function assumes edges are first 2 columns.

# draw a network graph
set.seed(10) # 76
ggraph(GC_network, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), show.legend = FALSE, alpha = .5) +
  geom_node_point(color = "#0052A5", size = 3, alpha = .5) +
  geom_node_text(aes(label = name), vjust = 2) +
  ggtitle("Word Network in DSF Gotham Culture survey Responses") +
  theme_void() 
  #theme(plot.title = element_markdown())


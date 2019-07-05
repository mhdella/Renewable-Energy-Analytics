################################################################################
#this code is written by D. Yang
#Singapore Institute of Manufacturing Technology (SIMTech)
#email: yangdazhi.nus@gmail.com
################################################################################
#clear the work space
rm(list = ls(all=TRUE))
libs <- c("dplyr", "tidytext", "ggplot2", "extrafont", "tidyr", "igraph", "ggraph", "ldatuning", "topicmodels")
invisible(lapply(libs, library, character.only = TRUE))

dir0 <- "...Your working directory..." 
setwd(dir0)
load("EmergingTech.RData")
files <- c("Advection_Model.pdf", "Error_Metric.pdf", "Hierarchical_Reconciliation.pdf", "Multi_Modeling.pdf", "QCPV.pdf", "Shadow_Camera.pdf")

################################################################################
#Application 1: Analyzing word and document frequency, Fig. 6 in the paper
################################################################################
mystopwords <- data_frame(word = c("japan", "yang", "ten", "march", "tier", "kuhn", "italy", "qatar", "sanfilippo", "month", "december", "year", "canberra", "killinger", "engerer", "freiburg", "mea")) #remove places and author names
topic <- data_frame(document = files, text = text)
topic <- topic %>% unnest_tokens(word, text, token = "words") %>% anti_join(mystopwords, by = "word") %>% count(document, word, sort = TRUE) %>% bind_tf_idf(word, document, n) 
list.size <- 20
data.plot <- NULL
for(i in unique(topic$document))
{
  words <- topic[which(topic$document == i),]
  words <- words[which(words$n>5),]
  words <-words[rev(order(words$tf_idf)),]
  tmp <- data_frame(uni = words$word[1:list.size], tf_idf.uni = words$tf_idf[1:list.size], group = i)
  data.plot <- rbind(data.plot, tmp)
}
data.plot <- data.plot %>% arrange(group, tf_idf.uni) %>% mutate(order = row_number())
p.size = 8
p1 <- data.plot%>%
  #ggplot(aes(order, tf_idf.uni, fill = factor(group))) +
  #geom_col(show.legend = FALSE) +
  ggplot(aes(order, tf_idf.uni)) +
  geom_col(show.legend = FALSE, fill = "grey80") +
  facet_wrap(~ group, scales = "free") +
  scale_x_continuous(breaks = data.plot$order, labels = data.plot$uni, expand = c(0,0)) +
  xlab(NULL) +
  ylab("tf-idf") +
  coord_flip() +
  theme_bw(base_size = p.size) +
  theme(plot.margin = unit(c(1,1,0,0), "lines"), text = element_text(size = p.size, family = "Times"))
p1

################################################################################
#Application 2: Analyzing relationships between words: Bigram, Fig. 7 in the paper
################################################################################
mystopbigrams <- data_frame(bigram = c("sanfilippo solar", "yang solar", "solar energy", "inage solar", "forecast forecast")) #remove places and author names
topic <- data_frame(document = files, text = text)
topic <- topic %>% unnest_tokens(bigram, text, token = "ngrams", n=2) %>% anti_join(mystopbigrams, by = "bigram")  %>% count(document, bigram, sort = TRUE) %>% bind_tf_idf(bigram, document, n) 
list.size <- 20
data.plot <- NULL
for(i in unique(topic$document))
{
  bigram <- topic[which(topic$document == i),]
  bigram <- bigram[which(bigram$n>3),]
  bigram <- bigram[rev(order(bigram$tf_idf)),]
  tmp <- data_frame(bi = bigram$bigram[1:list.size], tf_idf.bi = bigram$tf_idf[1:list.size], group = i)
  data.plot <- rbind(data.plot, tmp)
}
data.plot <- data.plot %>% arrange(group, tf_idf.bi) %>% mutate(order = row_number())
p.size = 8
p2 <- data.plot%>%
  ggplot(aes(order, tf_idf.bi)) +
  geom_col(show.legend = FALSE, fill = "grey80") +
  facet_wrap(~ group, scales = "free") +
  scale_x_continuous(breaks = data.plot$order, labels = data.plot$bi, expand = c(0,0)) +
  xlab(NULL) +
  ylab("tf-idf") +
  coord_flip() +
  theme_bw(base_size = p.size) +
  theme(plot.margin = unit(c(1,1,0,0), "lines"), text = element_text(size = p.size, family = "Times"))
p2

################################################################################
#Application 2: Analyzing relationships between words: Bigram network, Fig. 8 in the paper
################################################################################
mystopbigrams <- data_frame(bigram = c("sanfilippo solar", "yang solar", "solar energy", "inage solar", "forecast forecast")) #remove places and author names
topic <- data_frame(document = files, text = text)
topic <- topic %>% unnest_tokens(bigram, text, token = "ngrams", n=2) %>% anti_join(mystopbigrams, by = "bigram")
bigrams_separated <- topic %>% separate(bigram, c("word1", "word2"), sep = " ")  %>% count(word1, word2, sort = TRUE)

bigram_graph <- bigrams_separated %>% filter(n>7) %>% graph_from_data_frame()
set.seed(88)
a <- grid::arrow(type = "closed", angle = 15, length = unit(.05, "inches"))
p3 <- ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = sqrt(n)), show.legend = FALSE, arrow = a, end_cap = circle(.03, 'inches')) +
  geom_node_point(color = "lightblue", size = 1.8) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, family = "Times", size = 2.5, repel = TRUE, segment.alpha = 0.3) +
  theme_void()
p3

################################################################################
#Application 2: Analyzing relationships between words: correlation, OMITTED here
################################################################################
#p4, i.e., Fig. 9 in the paper is omitted since it requires somewhat different preprocessing, as described in the paper.

################################################################################
#Application 3: topic models, Figs. 10, 11 in the paper
################################################################################
mystopwords <- data_frame(word = c("japan", "yang", "ten", "march", "tier", "kuhn", "italy", "qatar", "sanfilippo", "month", "december", "year", "canberra", "killinger", "engerer", "freiburg", "mea", "irradiation")) #remove places and author names
topic <- data_frame(document = files, text = text)
topic <- topic %>% unnest_tokens(word, text) %>% anti_join(mystopwords, by = "word")

word_counts <- topic %>% count(document, word, sort = TRUE) #%>% separate(document, c("method", "document"), sep = "_", convert = TRUE)
topic_words <- word_counts%>% bind_tf_idf(word, document, n) %>% arrange(desc(tf_idf)) # %>% separate(document, c("method", "document"), sep = "_", convert = TRUE) 

topic_words <- topic_words[1:1000,]

chapters_dtm <- topic_words %>% cast_dtm(document, word, n)

result <- FindTopicsNumber(
  chapters_dtm,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

p5 <- FindTopicsNumber_plot(result)
p5

chapters_lda <- LDA(chapters_dtm, k = 6, control = list(seed = 77))

chapter_topics <- tidy(chapters_lda, matrix = "beta")
chapter_topics

top_terms <- chapter_topics %>% group_by(topic) %>% top_n(20, beta) %>% ungroup() %>% arrange(topic, beta)  %>% mutate(order = row_number())

p.size = 8
p6 <- top_terms%>%
  ggplot(aes(order, beta)) +
  geom_col(show.legend = FALSE, fill = "grey80") +
  facet_wrap(~ topic, scales = "free") +
  scale_x_continuous(breaks = top_terms$order, labels = top_terms$term, expand = c(0,0)) +
  xlab(NULL) +
  ylab("beta") +
  coord_flip() +
  theme_bw(base_size = p.size) +
  theme(plot.margin = unit(c(1,1,0,0), "lines"), text = element_text(size = p.size, family = "Times"))
p6


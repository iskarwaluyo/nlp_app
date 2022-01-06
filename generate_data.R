library(dplyr)
library(tm)
library(quanteda)
library(data.table)
library(ggplot2)

# PART I: DOWNLOADING AND UNZIPPING DATA
setwd("/media/iskar/archivos/R/DATA_SCIENCE_CAPSTONE/datos")
datos_url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
download.file(datos_url, "Coursera-SwiftKey.zip")
datos_local <- "Coursera-SwiftKey.zip"
options(timeout=1000000) # EXTENDS DOWNLOAD TIME LIMIT TO ALLOW LARGE FILE DOWNLOADS
dir <- getwd()

unzip(paste0(x, "/", datos_local), files = NULL, list = FALSE, overwrite = TRUE,
      junkpaths = FALSE, exdir = ".", unzip = "internal",
      setTimes = FALSE)

# PART II: READING en_US DATA FROM EXTRACTED FOLDERS AND CREATING CORPUS
setwd("/media/iskar/archivos/R/DATA_SCIENCE_CAPSTONE/datos/final/en_US")
twitter_us_en <- file("/media/iskar/archivos/R/DATA_SCIENCE_CAPSTONE/datos/final/en_US/en_US.twitter.txt", "r") 
blogs_us_en <- file("/media/iskar/archivos/R/DATA_SCIENCE_CAPSTONE/datos/final/en_US/en_US.blogs.txt", "r")
news_us_en <- file("/media/iskar/archivos/R/DATA_SCIENCE_CAPSTONE/datos/final/en_US/en_US.news.txt", "r")


directory <<- "/media/iskar/archivos/R/DATA_SCIENCE_CAPSTONE/datos/final/"
data_source <<- "twitter"
language_select <<- "en_US"

corpus_create <- function(directory, language_select, data_source){
  options(timeout=1000000) # EXTENDS TIME LIMIT TO ALLOW LARGE FILE USAGE
  filename <- paste0(directory, language_select, "/", language_select, ".", data_source, ".txt")
  text_file <- file(filename, "r")
  text_data <<- readLines(text_file)
  # SPLIT US-EN DATA INTO 15 "chunks"
  split_data <- split(text_data, sample(1:15, replace=F))
  new_names <<- c("chunk1", "chunk2", "chunk3", "chunk4", "chunk5", "chunk6", 
                  "chunk7", "chunk8", "chunk9", "chunk10", "chunk11", "chunk12", 
                  "chunk13", "chunk14", "chunk15")
  close.connection(text_file)
  sample_corpus <<- corpus(text_data)
}



# CREATE RDATA FILES OF SAMPLE CORPUS OBJECTS
setwd("/media/iskar/archivos/R/DATA_SCIENCE_CAPSTONE/datos/rdata")

save(txt_twitter_en_us, txt_blogs_en_us, txt_news_en_us, file = "texts.RData")

save(chunk1, chunk2, chunk3, chunk4, chunk5, chunk6, 
     chunk7, chunk8, chunk9, chunk10, chunk11, chunk12, 
     chunk13, chunk14, chunk15, file = "twitter_chunks.RData")

save(chunk1n, chunk2n, chunk3n, chunk4n, chunk5n, chunk6n, 
     chunk7n, chunk8n, chunk9n, chunk10n, chunk11n, chunk12n, 
     chunk13n, chunk14n, chunk15n, file = "news_chunks.RData")

save(chunk1b, chunk2b, chunk3b, chunk4b, chunk5b, chunk6b, 
     chunk7b, chunk8b, chunk9b, chunk10b, chunk11b, chunk12b, 
     chunk13b, chunk14b, chunk15b, file = "blog_chunks.RData")

save(sample_corpus, file = "corpus.RData")

setwd("/media/iskar/archivos/R/DATA_SCIENCE_CAPSTONE")

# PART III: NATURAL LANGUAGE PROCESSING WITH "quanteda" and "tm" PACKAGE

# CLEARING WORKSPACE AND LOADING RDATA
# THE AMOUNT OF DATA USED IN THE GLOBAL ENVIRONMENT WAS TOO MUCH
# THE GLOBAL ENVIRONMENT IS RESET AND ONLY NECESSARY RDATA IS LOADED

rm(list = ls()) # CLEAR WORKSPACE
load("/media/iskar/archivos/R/DATA_SCIENCE_CAPSTONE/datos/rdata/corpus.RData")

 
#  SIMPLE PATTERN SERACH 
head(kwic(sample_corpus_tokens, pattern = phrase("a pound of bacon")))
nrow(kwic(sample_corpus_tokens, pattern = phrase("immigrant")))

library("quanteda.textstats")

search1 <- sample_corpus_tokens %>% 
  tokens_select(pattern = "^[Bb]", valuetype = "regex", 
                case_insensitive = FALSE, padding = TRUE) %>% 
  textstat_collocations(min_count = 1, tolower = FALSE)

head(search1)


# CREATE RDATA FILES OF SAMPLE CORPUS OBJECTS AND TOKENS
setwd("/media/iskar/archivos/R/DATA_SCIENCE_CAPSTONE/datos/rdata")

save(sample_corpus_tokens, sample_corpus_tokens_clean, file = "corpus_tokens.RData")
save(sample_corpus, file = "corpus.RData")

setwd("/media/iskar/archivos/R/DATA_SCIENCE_CAPSTONE")

# PART IV: FREQUENCIES (BINARY WEIGHT, TERM FREQUENCY (TF), INVERSE DOCUMENT FREQUENCY (IDF) AND TF-IDF )

rm(list = ls()) # CLEAR WORKSPACE
gc() # FREE MEMORY
load("/media/iskar/archivos/R/DATA_SCIENCE_CAPSTONE/datos/rdata/corpus.RData")
load("/media/iskar/archivos/R/DATA_SCIENCE_CAPSTONE/datos/rdata/corpus_tokens.RData")


# BINARY WEIGHTS (COUNTS) 1 = YES 0 = NO
# CAN BE ESTIADED WITH A DTM/DFM 

dfm_samplecorpus <- dfm(sample_corpus_tokens_clean) # DFM QUANTEDA PACKAGE


# TERM FREQUENCE (TF) CALCULATES THE OCCURENCE OF TERMS IN A DOCUMENT
# TF(t) = c(t,d) 
# t = text, d = document

# N-GRAM TOKENIZATION (TOKENIZING FULL PHRASES)
ngram_1 <- tokens_ngrams(sample_corpus_tokens_clean, n = 1)
ngram_2 <- tokens_ngrams(sample_corpus_tokens_clean, n = 2)
ngram_3 <- tokens_ngrams(sample_corpus_tokens_clean, n = 3)

dfm_n1 <- dfm(ngram_1)
dfm_n2 <- dfm(ngram_2)
dfm_n3 <- dfm(ngram_3)

dfm_n1_trim <- dfm_trim(dfm_n1, 10)
dfm_n2_trim <- dfm_trim(dfm_n2, 10)
dfm_n3_trim <- dfm_trim(dfm_n3, 10)

# Create named vectors with counts of words 
sums_n1 <- colSums(dfm_n1)
sums_n2 <- colSums(dfm_n2)
sums_n3 <- colSums(dfm_n3)

# Create data tables with individual words as columns
n1gram_counts <- data.table(word1 = names(sums_n1), count = sums_n1)

n2gram_counts <- data.table(
  word1 = sapply(strsplit(names(sums_n2), "_", fixed = TRUE), '[[', 1),
  word2 = sapply(strsplit(names(sums_n2), "_", fixed = TRUE), '[[', 2),
  count = sums_n2)

n3gram_counts <- data.table(
  word1 = sapply(strsplit(names(sums_n3), "_", fixed = TRUE), '[[', 1),
  word2 = sapply(strsplit(names(sums_n3), "_", fixed = TRUE), '[[', 2),
  word3 = sapply(strsplit(names(sums_n3), "_", fixed = TRUE), '[[', 3),
  count = sums_n3)

setkey(n1gram_counts, word1)
setkey(n2gram_counts, word1, word2)
setkey(n3gram_counts, word1, word2, word3)

graph.data <- n1gram_counts[order(n1gram_counts$count, decreasing = T), ]
graph.data <- graph.data[1:20, ]
graph.data$word <- factor(graph.data$word1, levels = graph.data$word1)

ggplot(data=graph.data, aes(x=word, y=count), position = fill(reverse = TRUE)) + 
  geom_bar(stat="identity") +
  coord_flip() + 
  labs(title="Top 20 unigrams used in Sample Corpus", 
       subtitle="Data from 2019", 
       y="Number of unigrams", 
       x="Unigrams", 
       caption = "Data source: Courseara Data Science Specialization") +
  theme(legend.position = "bottom")

graph.data <- n2gram_counts[order(n2gram_counts$count, decreasing = T), ]
graph.data <- graph.data[1:20, ]
graph.data$word <- paste(graph.data$word1, graph.data$word2)
graph.data$word <- factor(graph.data$word, levels = graph.data$word)

ggplot(data=graph.data, aes(x=word, y=count), position = fill(reverse = TRUE)) + 
  geom_bar(stat="identity") +
  coord_flip() + 
  labs(title="Top 20 bigrams used in Sample Corpus", 
       subtitle="Data from 2019", 
       y="Number of bigrams", 
       x="Bigrams", 
       caption = "Data source: Courseara Data Science Specialization") +
  theme(legend.position = "bottom")

graph.data <- n3gram_counts[order(n3gram_counts$count, decreasing = T), ]
graph.data <- graph.data[1:20, ]
graph.data$word <- paste(graph.data$word1, graph.data$word2, graph.data$word3)
graph.data$word <- factor(graph.data$word, levels = graph.data$word)

ggplot(data=graph.data, aes(x=word, y=count), position = fill(reverse = TRUE)) + 
  geom_bar(stat="identity") +
  coord_flip() + 
  labs(title="Top 20 trigrams used in Sample Corpus", 
       subtitle="Data from 2019", 
       y="Number of trigrams", 
       x="Trigrams", 
       caption = "Data source: Courseara Data Science Specialization") +
  theme(legend.position = "bottom")

# CREATE RDATA FILES OF SAMPLE CORPUS OBJECTS AND TOKENS
setwd("/media/iskar/archivos/R/DATA_SCIENCE_CAPSTONE/datos/rdata")

save(sums_n1, sums_n2, sums_n3, file = "ngram_sums.RData")
save(n1gram_counts, n2gram_counts, n3gram_counts, file = "ngrams.RData")

setwd("/media/iskar/archivos/R/DATA_SCIENCE_CAPSTONE")

# PART IV: FREQUENCIES (BINARY WEIGHT, TERM FREQUENCY (TF), INVERSE DOCUMENT FREQUENCY (IDF) AND TF-IDF )

rm(list = ls()) # CLEAR WORKSPACE
gc() # FREE MEMORY

load("/media/iskar/archivos/R/DATA_SCIENCE_CAPSTONE/datos/rdata/corpus_tokens.RData")
load("/media/iskar/archivos/R/DATA_SCIENCE_CAPSTONE/datos/rdata/ngram_sums.RData")
load("/media/iskar/archivos/R/DATA_SCIENCE_CAPSTONE/datos/rdata/ngrams.RData")

# Kneser-Kney Smoothing

discount <- 0.75
######## Finding Bi-Gram Probability #################

# Finding number of bi-gram words
n2gram_number <- nrow(n2gram_counts[by = .(word1, word2)])

# Dividing number of times word 2 occurs as second part of bigram, by total number of bigrams.  
# ( Finding probability for a word given the number of times it was second word of a bigram)
ckn <- n2gram_counts[, .(Prob = ((.N) / n2gram_number)), by = word2]
setkey(ckn, word2)

# INVERSE DOCUMENT FREQUENCY (IDF) ASSIGNS HIGHER WEIGHTS TO UNUSUAL TERMS
# IDF(t) = 1 + log(N/df(t)) 
# N = number of documents, t = term, df(t) = number of documents with "t"

idf <- 1 + log(nrow(n1gram_counts)/n1gram_counts$count)

# TF-IDF AIMS TO BALANCE THE "UNIQUENESS" OF SOME TERMS (OUTLIERS) WITH THE ABUNDANCE OF COMMON ONES
# TF-IDF(t) = TF(t)*IDF(t)
# TF-IDF(t) = tf(t)*log(N/df(t))

tf_idf <- n1gram_counts$count*idf
tf_idf <- cbind(n1gram_counts, tf_idf)

library("quanteda.textstats")
search <- sample_corpus_tokens_clean %>% 
  tokens_remove(stopwords("en")) %>% 
  tokens_select(pattern = "^[immigrant]", valuetype = "regex", 
                case_insensitive = FALSE, padding = TRUE) %>% 
  textstat_collocations(min_count = 5, tolower = FALSE)
head(search)

# Assigning the probabilities as second word of bigram, to unigrams
n1gram_counts[, Prob := ckn[word1, Prob]]
n1gram_counts <- n1gram_counts[!is.na(n1gram_counts$Prob)]

# Finding number of times word 1 occurred as word 1 of bi-grams
n1wi <- n2gram_counts[, .(N = .N), by = word1]
setkey(n1wi, word1)

# Assigning total times word 1 occured to bigram cn1
n2gram_counts[, Cn1 := n1gram_counts[word1, count]]

# Kneser Kney Algorithm
n2gram_counts[, Prob := ((count - discount) / Cn1 + discount / Cn1 * n1wi[word1, N] * n1gram_counts[word2, Prob])]

######## End of Finding Bi-Gram Probability #################

######## Finding Tri-Gram Probability #################

# Finding count of word1-word2 combination in bigram 
n3gram_counts[, Cn2 := n2gram_counts[.(word1, word2), count]]

# Finding count of word1-word2 combination in trigram
n1w12 <- n3gram_counts[, .N, by = .(word1, word2)]
setkey(n1w12, word_1, word_2)

# Kneser Kney Algorithm
n3gram_counts[, Prob := (count - discount) / Cn2 + discount / Cn2 * n1w12[.(word1, word2), N] 
              * n2gram_counts[.(word1, word2), Prob]]

######## End of Finding Tri-Gram Probability #################

uni_words <- n1gram_counts[order(-Prob)][1:50]
bi_words <- n2gram_counts[order(-Prob)][1:50]


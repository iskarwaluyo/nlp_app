library(curl)
library(dplyr)
library(tm)
library(quanteda)
library(data.table)
library(ggplot2)
library(shiny)
options(timeout=1000000) # EXTENDS DOWNLOAD TIME LIMIT TO ALLOW LARGE FILE DOWNLOADS

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  # Return the requested dataset
  datasetInput <- reactive({
    switch(input$language_select,
           "English" = "en_US",
           "German" = "de_DE",
           "Finish" = "fi_FI",
           "Russian" = "ru_RU"
    )
  })
  
  output$language_select <- renderPrint({
    language_select <<- datasetInput()
    language_select
  })
  
  datasetInput2 <- reactive({
    switch(input$data_source,
           "Twitter" = "twitter",
           "News" = "news",
           "Blogs" = "blogs"
    )
  })
  
  output$data_source <- renderPrint({
    data_source <<- datasetInput2()
    data_source
  })
  
  output$sample_size <- renderPrint({
    sample_size <<- input$sample_size})
  
  output$corpus_create <- renderPrint({
    
    directory <- "https://magicmixtape.club/sigdata/nlp_data/data/"
    language_select <- datasetInput()
    data_source <- datasetInput2()
    
    corpus_create <- function(directory, language_select, data_soaurce){
      filename <- paste0(directory, language_select, "/", language_select, ".", data_source, ".txt")
      text_file <- file(filename, "r")
      text_data <<- readLines(text_file)
      close.connection(text_file)
    }
    corpus_create(directory, language_select, data_source)
    paste0("First 6 lines of dataset:")
    head(text_data)
  })
  
  output$corpus_sample <- renderPrint({
    language_select <- datasetInput()
    data_source <- datasetInput2()
    
    # CORPUS SAMPLE
    corpus_sample <- function(sample_size)
    {
      random_sampling <- sample(length(text_data), length(text_data) * sample_size)
      
      # SPLITS TEXT DATA INTO "N" CHUNKS
      split_data <<- text_data[random_sampling]
      # CREATES CORPUS OF SPLIT DATA
      split_corpus <<- corpus(split_data)
    } 
    corpus_sample(sample_size)
    
  })
  
  output$phrase <- renderText({input$phrase})
  
  datasetInput4 <- reactive({
    switch(input$remove_stopwords,
           "Yes" = "Yes",
           "No" = "No"
    )
  })
  
  output$remove_stopwords <- renderPrint({
    remove_stopwords <<- datasetInput4()
    remove_stopwords
  })  
  
  datasetInput5 <- reactive({
    switch(input$word_stemming,
           "Yes" = "Yes",
           "No" = "No"
    )
  })
  
  output$word_stemming <- renderPrint({
    word_stemming <<- datasetInput5()
    word_stemming
  })  
  
  output$tokenize <- renderPrint({
    
    remove_stepwords <- datasetInput4()
    word_stemming <- datasetInput5()
    
    corpus_tokeinze <- function(corpus_select, remove_stepwords, word_stemming){
      # corpus_n <<- corpus_n
      # corpus_select <- split_corpus[[corpus_n]]
      
      corpus_select <- split_corpus
      
      sample_corpus_tokens <<- tokens(corpus_select)
      
      sample_corpus_tokens_clean <<- tokens(
        x = tolower(corpus_select), remove_punct = TRUE, remove_twitter = TRUE, 
        remove_numbers = TRUE, remove_hyphens = TRUE, remove_symbols = TRUE,
        remove_url = TRUE
      )
      
      if (remove_stepwords == "Yes"){
        sample_corpus_tokens_clean <<- tokens_wordstem(sample_corpus_tokens_clean, language = "english")
      }
      else{sample_corpus_tokens_clean <- sample_corpus_tokens_clean}
      
      if (word_stemming == "Yes"){
        sample_corpus_tokens_clean <<- tokens_remove(sample_corpus_tokens_clean, c(stopwords("english")))
      }
      else{sample_corpus_tokens_clean <- sample_corpus_tokens_clean}
    }
    
    corpus_tokeinze(corpus_select, remove_stepwords, word_stemming)
    head(sample_corpus_tokens_clean)
    
  })
  
  
  output$ngrams <- renderPrint({
    
    ngram_create <- function(corpus_tokens){
      
      corpus_tokens <- sample_corpus_tokens_clean

      # PART III: NATURAL LANGUAGE PROCESSING WITH "quanteda" and "tm" PACKAGE
      
      dfm_corpus_tokens <<- dfm(corpus_tokens)
      # TERM FREQUENCE (TF) CALCULATES THE OCCURENCE OF TERMS IN A DOCUMENT
      # TF(t) = c(t,d) 
      # t = text, d = document
      
      # N-GRAM TOKENIZATION (TOKENIZING FULL PHRASES)
      ngram_1 <- tokens_ngrams(corpus_tokens, n = 1)
      ngram_2 <- tokens_ngrams(corpus_tokens, n = 2)
      ngram_3 <- tokens_ngrams(corpus_tokens, n = 3)
      
      dfm_n1 <- dfm(ngram_1)
      dfm_n2 <- dfm(ngram_2)
      dfm_n3 <- dfm(ngram_3)
      
      # dfm_n1_trim <- dfm_trim(dfm_n1, 10)
      # dfm_n2_trim <- dfm_trim(dfm_n2, 10)
      # dfm_n3_trim <- dfm_trim(dfm_n3, 10)
      
      # Create named vectors with counts of words 
      sums_n1 <- colSums(dfm_n1)
      sums_n2 <- colSums(dfm_n2)
      sums_n3 <- colSums(dfm_n3)
      
      # Create data tables with individual words as columns
      n1gram_counts <<- data.table(word1 = names(sums_n1), count = sums_n1)
      
      n2gram_counts <<- data.table(
        word1 = sapply(strsplit(names(sums_n2), "_", fixed = TRUE), '[[', 1),
        word2 = sapply(strsplit(names(sums_n2), "_", fixed = TRUE), '[[', 2),
        count = sums_n2)
      
      n3gram_counts <<- data.table(
        word1 = sapply(strsplit(names(sums_n3), "_", fixed = TRUE), '[[', 1),
        word2 = sapply(strsplit(names(sums_n3), "_", fixed = TRUE), '[[', 2),
        word3 = sapply(strsplit(names(sums_n3), "_", fixed = TRUE), '[[', 3),
        count = sums_n3)
      
      setkey(n1gram_counts, word1)
      setkey(n2gram_counts, word1, word2)
      setkey(n3gram_counts, word1, word2, word3)
    } 
    
    ngram_create(corpus_tokens)
    head(n1gram_counts)
    
  })
  
  output$plot1 <- renderPlot({
    
    dataset <<- datasetInput()
    
    library(sbo)
    p <- sbo_predictor(split_data[[corpus_n]], # 50k tweets, example dataset
                       N = 3, # Train a 3-gram model
                       dict = sbo::twitter_dict, # Top 1k words appearing in corpus
                       .preprocess = sbo::preprocess, # Preprocessing transformation
                       EOS = ".?!:;" # End-Of-Sentence characters
    )
    
  })
  # Show the first "n" observations
  output$data <- renderTable({
    head(datasetInput(), n = input$obs)
  })
})
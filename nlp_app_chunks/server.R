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
  
  # --------- INPUTS FROM UI ---------
  
  datasetInput <- reactive({
    switch(input$language_select,
           "English" = "en_US",
           "German" = "de_DE",
           "Finish" = "fi_FI",
           "Russian" = "ru_RU"
    )
  })
  
  # only used to display outputs. NOT NECESSARY 
  
#  output$language_select <- renderPrint({
#    language_select <<- datasetInput()
#   language_select
#  })
  
  datasetInput2 <- reactive({
    switch(input$data_source,
           "Twitter" = "twitter",
           "News" = "news",
           "Blogs" = "blogs"
    )
  })
  
  # only used to display outputs. NOT NECESSARY 
#  output$data_source <- renderPrint({
#    data_source <<- datasetInput2()
#  })
  
  datasetInputA <- reactive({
    input$number_of_chunks
  })
  
  output$number_of_chunks <- renderPrint({
    number_of_chunks <- datasetInputA()
  })  

  datasetInputB <- reactive({
    input$corpus_n
  })
  
  output$corpus_n <- renderPrint({
    corpus_n <<- datasetInputB()
  })  

  datasetInputC <- reactive({
    input$phrase_input
  })
  # only used to display outputs. NOT NECESSARY 
  
 # output$phrase <- renderText({input$phrase})
  
  datasetInput4 <- reactive({
    switch(input$remove_stopwords,
           "Yes" = "Yes",
           "No" = "No"
    )
  })
  
  output$remove_stopwords <- renderPrint({
    remove_stopwords <<- datasetInput4()
  })  
  
  datasetInput5 <- reactive({
    switch(input$word_stemming,
           "Yes" = "Yes",
           "No" = "No"
    )
  })
  
  output$word_stemming <- renderPrint({
    word_stemming <<- datasetInput5()
  })  
  
  datasetInput6 <- reactive({
    switch(input$ngram_select,
           "1" = 1,
           "2" = 2,
           "3" = 3
    )
  })
  
  # --------- END INPUTS FROM UI ---------
  
  
  # --------- INPUT PROCESSING ---------
  
  output$corpus_create <- renderPrint({
    
    language_select <- datasetInput()
    data_source <- datasetInput2()
    remove_stepwords <- datasetInput4()
    word_stemming <- datasetInput5()
    number_of_chunks <- datasetInputA()
    corpus_n <<- datasetInputB()
    phrase_input <<- datasetInputC()
    
    directory <- "/media/iskar/archivos/R/DATA_SCIENCE_CAPSTONE/datos/final"
    language_select <- datasetInput()
    data_source <- datasetInput2()
    
    corpus_create <- function(directory, language_select, data_soaurce){
      filename <- paste0(directory, language_select, "/", language_select, ".", data_source, ".txt")
      text_file <- file(filename, "r")
      text_data <<- readLines(text_file)
      close.connection(text_file)
    }
    corpus_create(directory, language_select, data_source)
    # head(text_data)
  })
  
  output$chunks <- renderPrint({
    
    language_select <- datasetInput()
    data_source <- datasetInput2()
    remove_stepwords <- datasetInput4()
    word_stemming <- datasetInput5()
    number_of_chunks <- datasetInputA()
    corpus_n <- datasetInputB()

    # SPLIT CORPUS IN "N" CHUNKS
    chunks <- function(number_of_chunks, corpus_n)
    {
      # SPLITS TEXT DATA INTO "N" CHUNKS
      split_data <<- split(text_data, sample(1:number_of_chunks, replace=F))
      # CREATES CORPUS OF SPLIT DATA
      split_corpus <<- lapply(split_data, corpus)
    } 
    chunks(number_of_chunks)
    head(split_data[[corpus_n]])
  })

  
  # CREATE TOKENS WITH QUANTEDA PACKAGE TOOLS
  
  output$tokenize <- renderPrint({
  # ADD ALL VARIABLES IN ORDER TO UPDATE WHEN VARIABLE CHANGES.
    
    language_select <- datasetInput()
    data_source <- datasetInput2()
    remove_stepwords <- datasetInput4()
    word_stemming <- datasetInput5()
    number_of_chunks <- datasetInputA()
    corpus_n <- datasetInputB()
    
    
    corpus_tokeinze <- function(corpus_n, remove_stepwords, word_stemming){
      corpus_select <- split_corpus[[corpus_n]]
      
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
    
    language_select <- datasetInput()
    data_source <- datasetInput2()
    remove_stepwords <- datasetInput4()
    word_stemming <- datasetInput5()
    number_of_chunks <- datasetInputA()
    
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
  
  output$ngram_plot <- renderPlot({
    
    language_select <- datasetInput()
    data_source <- datasetInput2()
    remove_stepwords <- datasetInput4()
    word_stemming <- datasetInput5()
    number_of_chunks <- datasetInputA()
    
    ngram_select <- datasetInput6()
  
  ngram_plot <- function(ngram_select){
    
    if (ngram_select == "1"){
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
    }
    
    else if (ngram_select == "2"){
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
    }
    
    else if (ngram_select == "3"){
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
    }
    else {print("Please select 1 to 3 only.")}
  }
    
  })

  
  output$phrase_search <- renderPrint({
    
    language_select <- datasetInput()
    data_source <- datasetInput2()
    remove_stepwords <- datasetInput4()
    word_stemming <- datasetInput5()
    number_of_chunks <- datasetInputA()
    
    ngram_select <- datasetInput6()
    phrase_input <- datasetInputC()
    
    head(kwic(split_data[[corpus_n]], pattern = phrase(phrase_input)))
    
  })
  
  output$nextword_predict <- renderPrint({
    
    language_select <- datasetInput()
    data_source <- datasetInput2()
    remove_stepwords <- datasetInput4()
    word_stemming <- datasetInput5()
    number_of_chunks <- datasetInputA()
    
    ngram_select <- datasetInput6()
    phrase_input <- datasetInputC()
    
    gram_length <- length(as.character(tokens(phrase_input)))
    
    library(sbo)
    p <- sbo_predictor(split_data[[corpus_n]], # 50k tweets, example dataset
                       N = gram_length, # Train a n-gram model
                       dict = sbo::twitter_dict, # Top 1k words appearing in corpus
                       .preprocess = sbo::preprocess, # Preprocessing transformation
                       EOS = ".?!:;" # End-Of-Sentence characters
    )
    
    p
    
    predict(p, phrase_input)
    
  })
  
})
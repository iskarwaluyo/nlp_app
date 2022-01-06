library(data.table)
library(curl)
library(devtools)
library(rio)

library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title.
  headerPanel("Natural Language Processing App"),
  
  sidebarPanel(
    
    selectInput("language_select", "Select dataset language:", 
                choices = c("English", "German", "Finish", "Russian")),
    
    selectInput("data_source", "Select dataset source:", 
                choices = c("Twitter", "News", "Blogs")),
    
   sliderInput("number_of_chunks", "Number of chunks to split corpus in:",
                min = 0, max = 50, value = 20),
    
   sliderInput("corpus_n", "Observations:", 10, min = 1, max = 50),
    
   selectInput("remove_stopwords", "Would you like to remove stopwords?", 
                choices = c("Yes", "No")),
    
   selectInput("word_stemming", "Would you like to stem words?", 
                choices = c("Yes", "No")),
    
    # helpText("Note:"),
    
    submitButton("Actualizar Vista"),
    
  ),
  
  # Show a summary of the dataset and an HTML table with the requested
  # number of observations. Note the use of the h4 function to provide
  # an additional header above each output section.
  mainPanel(
    
    h4("View of sample of selected corpus chunk and n-grams:"),
    verbatimTextOutput("corpus_create"),
    paste0("Sample of chunk: "),
    verbatimTextOutput("chunks"),

    selectInput("ngram_select", "Number of n-grams for plot:", 
                choices = c("1", "2", "3")),
    verbatimTextOutput("ngram_plot"),
    
    h4("Text search:"),
    textInput("phrase_input", "Enter text to search for:", "of bacon"),
    verbatimTextOutput("phrase"),
    verbatimTextOutput("phrase_search"),
    
    h4("Possible next words: "),
    verbatimTextOutput("nextword_predict")
    
  )
))
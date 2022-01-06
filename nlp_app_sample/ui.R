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
    
   selectInput("remove_stopwords", "Would you like to remove stopwords?", 
                choices = c("Yes", "No")),
    
   selectInput("word_stemming", "Would you like to stem words?", 
                choices = c("Yes", "No")),
    
    helpText("Note: This version works with a 10% sample of the original data set hosted in a free Github account and may not find all phrases."),
    
    submitButton("Submit input data"),
    
  ),
  
  # Show a summary of the dataset and an HTML table with the requested
  # number of observations. Note the use of the h4 function to provide
  # an additional header above each output section.
  mainPanel(
    
    h4("View of sample of selected corpus chunk and n-grams:"),
    verbatimTextOutput("corpus_create"),
    paste0("Sample of chunk: "),
    verbatimTextOutput("chunks"),
    
    h4("Text search:"),
    textInput("phrase_input", "Enter text to search for:", "of bacon"),
    verbatimTextOutput("phrase"),
    verbatimTextOutput("phrase_search"),
    
    h4("Possible next words: "),
    verbatimTextOutput("nextword_predict"),
    
    selectInput("ngram_select", "Number of n-grams for plot:", 
                choices = c("1", "2", "3")),
    plotOutput("ngram_plot")
    
  )
))
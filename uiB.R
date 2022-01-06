library(data.table)
library(curl)
library(devtools)
library(rio)

library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title.
  headerPanel("Natural Language Processing App"),
  
  # Sidebar with controls to select a dataset and specify the number
  # of observations to view. The helpText function is also used to 
  # include clarifying text. Most notably, the inclusion of a 
  # submitButton defers the rendering of output until the user 
  # explicitly clicks the button (rather than doing it immediately
  # when inputs change). This is useful if the computations required
  # to render output are inordinately time-consuming.
  
  sidebarPanel(
    
    selectInput("language_select", "Select dataset language:", 
                choices = c("English", "German", "Finish", "Russian")),
    
    selectInput("data_source", "Select dataset source:", 
                choices = c("Twitter", "News", "Blogs")),
    
    sliderInput("sample_size", "Sample size:",
                min = 0.1, max = 1.0, value = 0.2),
    
    # numericInput("corpus_n", "Choose chunk to analyze:", 10),
    
    selectInput("remove_stopwords", "Would you like to remove stopwords?", 
                choices = c("Yes", "No")),
    
    selectInput("word_stemming", "Would you like to stem words?", 
                choices = c("Yes", "No")),
  
    helpText("Note:"),
    
    submitButton("Actualizar Vista"),
    
  ),
  
  # Show a summary of the dataset and an HTML table with the requested
  # number of observations. Note the use of the h4 function to provide
  # an additional header above each output section.
  mainPanel(
    
    textInput("phrase", "Caption", "bacon"),
    verbatimTextOutput("phrase"),
    
    h4("Resumen Cajeros:"),
    verbatimTextOutput("number_of_chunks"),
    verbatimTextOutput("corpus_create"),
    verbatimTextOutput("corpus_sample"),
    verbatimTextOutput("tokenize"),
        
    h4("Datos Tickets:"),
    verbatimTextOutput("ngrams")
    
  )
))
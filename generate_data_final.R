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

directory <- "/media/iskar/archivos/R/DATA_SCIENCE_CAPSTONE/datos/final/"
language_select <- "en_US"
data_source <- "twitter"

corpus_create <- function(directory, language_select, data_source, sample_size){
  options(timeout=1000000) # EXTENDS TIME LIMIT TO ALLOW LARGE FILE USAGE
  filename <- paste0(directory, language_select, "/", language_select, ".", data_source, ".txt")
  text_file <- file(filename, "r")
  text_data <<- readLines(text_file)
  sampling_data <- sample(length(text_data), length(text_data) * sample_size)
  txt_sample <- text_data[sampling_data]
  
  setwd(paste0("/media/iskar/archivos/R/DATA_SCIENCE_CAPSTONE/app/sample_data/", language_select))
  
  write.table(txt_sample, paste0("/media/iskar/archivos/R/DATA_SCIENCE_CAPSTONE/app/sample_data/", 
                                 language_select, "/", language_select, ".", data_source, ".txt"), 
              sep=";",col.names = FALSE, row.names = FALSE)
}

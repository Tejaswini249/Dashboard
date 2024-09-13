##  Load and install libraries 
pacman::p_load(tidyverse,readxl,writexl,janitor,lubridate,syuzhet,tidytext,hms,SnowballC,textstem,tm, ggplot2,
               text2vec,textdata,quanteda,quanteda.textplots,quanteda.textstats,SentimentAnalysis, purrr, shiny, shinydashboard)



# load and map all data sets, Change your file path
file_paths <- list.files(path = "/Users/tejaswinimode/Desktop/Data",
                         pattern = "*.csv", full.names = TRUE)

predf<- map_dfr(file_paths, ~ read_csv(.x))

write_xlsx(predf,"rocky_masterdf.xlsx")

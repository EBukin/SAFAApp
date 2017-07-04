#' Readin and loading all data on the app usate into a data file for analysis.
#' All data is located in the foldes `data/appData` in the zipped csv files.
#' We will programmatically unzippzel all into the `TEMP` folder and clean it after reading the files.
#'
#'


#' ## Session setup
#'
#+
library(tidyverse)
library(stringr)
library(lubridate)

# Sourcing function form the R folder
# lapply(file.path("R", list.files("R", "(\\.[rR])$")), source , .GlobalEnv)
source("R/cleanTempFolder.R")
source("R/unzp2temp.r")

# Listing all files in a convenient way parcing all text into columns
folder <- "data/appData/"
files <-
  tibble(files = list.files(folder, ".zip$")) %>%
  mutate(
    Statistics = str_extract(files, "[[:alnum:]]+(?=\\_)"),
    App = str_extract(files, "(?<=\\_).+(?=\\_)"),
    Year = str_extract(files, "\\d{4}"),
    Month = str_extract(files, "(?<=\\d{4}).{3}")
  )

#
# folderPath <- folder
# fileData <- files[1, ]
# fileName <- fileData$files


# Function for reading one zip file
readAllCSV <-
  function(fileName, folderPath) {
    theTempFolder <- file.path(tempdir(), "SAFAAPPDATATEMP")
    
    unzp2temp(folderPath = folderPath,
              fileName = fileName,
              tempFolder = theTempFolder)
    
    csvFiles <-
      tibble(files = list.files(theTempFolder, ".csv$")) %>%
      mutate(type = str_extract(files, "(?<=\\_)[:alpha:]+(?=.csv$)")) %>%
      group_by(files)
    
    # oneFileName = csvFiles[8,]$files
    readOneCSV <-
      function(oneFileName, type, oneFolderPath = folderPath) {
        oneFilePath <- file.path(theTempFolder, oneFileName)
        read.csv(
          file = oneFilePath,
          fileEncoding = "UTF-16",
          stringsAsFactors = FALSE
        ) %>%
          tbl_df() %>%
          mutate(fileName = type)
      }
    
    # Read all files and clean data
    data <- csvFiles %>%
      group_by(files) %>%
      do(readOneCSV(.$files, .$type)) %>%
      ungroup() %>%
       select(-files) #%>%
      # filter(!is.na(Value))
    
    cleanTempFolder(theTempFolder)
    
    data
    
  }

# Reading all files 
appData <- 
  files %>% 
  group_by(files) %>% 
  do(readAllCSV(.$files, folder)) %>% 
  ungroup() %>% 
  select(-files)

write_csv(appData, "data/allHarvestedAppData.csv" )
save(appData, file = "data/allHarvestedAppData.Rdata" )

# Exploring data ----------------------------------------------------------

map(appData, .f = function(x) tibble(unique(x)))



# Filtering not NA variables 
vars <- 
  appData %>% 
  filter(fileName == "country") %>% 
  map_lgl(.x = ., .f = function(x) {!all(is.na(x))})

countryData <-
appData[vars] %>%
  mutate(Date = ymd(Date)) %>% 
  filter(fileName == "country") %>%
  select(Date, Country, Daily.Device.Installs, Daily.Device.Uninstalls) %>% 
  distinct() 
  


countryData %>% 
  group_by(Date = make_date(year(Date),month(Date))) %>% 
  summarise_at(c(3:4), sum) %>% 
  ungroup() %>% 
  gather(Var, Val, 2:3) %>% 
  ggplot(aes(x = Date, y = Val, fill = Var, colour = Var)) +
  # geom_histogram(binwidth = 1)
  geom_bar(stat="identity", position = "dodge") +
  scale_x_date(date_breaks = "6 month")


  
  


# Loading Brasilian data
# Resaving it in a better format.


# Setup -------------------------------------------------------------------


library(tidyverse)
library(readxl)
library(stringr)



# Loading data ------------------------------------------------------------

rootFolder <- "data-raw/Brazil/"

files <- c(
  "1. Campinas do Sul PISA Norte BR.xlsx",
  "2. Erechim PISA Norte BR.xlsx",
  "3. Getulio Vargas PISA Norte BR.xlsx",
  "4. Sertão PISA Norte BR.xlsx",
  "5. Três Arroios PISA Norte BR.xlsx",
  "6. Viadutos PISA Norte BR.xlsx"  
)

# Funciton for harvesting sheets ------------------------------------------

harvesOneSheet <- 
  function(oneSheet, pathToFile) {
  harvData <-
    read_excel(
      pathToFile,
      sheet = oneSheet,
      col_names = FALSE,
      col_types = "text", 
      range = "A1:J111"
    )
  
  
  extrGeneralInfo <- function(data, codeVal, rowN, colN, nextValue = FALSE, generaPattern = "\\:") {
    Value <- str_trim(str_extract(data[rowN, colN][[1]], str_c("(?<=", generaPattern, ")(.*)")))
    Value2 <- data[rowN, colN+1][[1]]
    Name <- codeVal
    if(nextValue | length(Value) < 2) {
      Value <- Value2
    } 
    out <- tibble(Value)
    names(out) <- Name
    out
  }
  
  # Harvesting gerenal information.
  generalData <-
    list(
      extrGeneralInfo(harvData, "001_Name_Interviewer", 2, 1, FALSE),
      extrGeneralInfo(harvData, "002_Organisation_Interviewer", 3, 1, FALSE),
      extrGeneralInfo(harvData, "003_Date_Evaluation", 4, 1, FALSE),
      extrGeneralInfo(harvData, "004_Name_Interviewee", 5, 1, TRUE),
      extrGeneralInfo(harvData, "005_Gender_Interviewee", 6, 1, TRUE),
      extrGeneralInfo(harvData, "006_Is_owner_of_property", 2, 2, FALSE, "\\?"),
      extrGeneralInfo(harvData, "007_Name_of_property", 2, 5, FALSE),
      extrGeneralInfo(harvData, "008_Location", 3, 5, FALSE),
      extrGeneralInfo(harvData, "009_Municipality", 4, 5, FALSE),
      extrGeneralInfo(harvData, "010_On_property_interview", 5, 5, FALSE, "\\?"),
      extrGeneralInfo(harvData, "011_GPS_property", 7, 1, TRUE),
      extrGeneralInfo(harvData, "012_GPS_interview", 3, 2, TRUE, "aqui"),
      extrGeneralInfo(harvData, "013_Tel", 8, 1, TRUE),
      extrGeneralInfo(harvData, "014_Email", 8, 1, TRUE),
      extrGeneralInfo(harvData, "0151_Proiduct_1", 7, 5, FALSE),
      extrGeneralInfo(harvData, "0152_Proiduct_2", 8, 5, FALSE),
      extrGeneralInfo(harvData, "0153_Proiduct_3", 9, 5, FALSE),
      extrGeneralInfo(harvData, "0154_Proiduct_4", 10, 5, FALSE),
      extrGeneralInfo(harvData, "017_Animal_production", 10, 1, FALSE, "\\?"),
      extrGeneralInfo(harvData, "018_Area Size", 10, 3, FALSE, "\\?")
    ) %>% 
      bind_cols()
  
  dataPart <- 
    harvData %>% 
    slice(12:nrow(.)) %>% 
    select(1,2,3,4,6,7,9)
  
  dataPart[1,1] <- "Dimension" 
  dataPart[1,2] <- "Subject" 
  dataPart[1,3] <- "Indicator" 
  dataPart[1,4] <- "Code" 
  dataPart[1,5] <- "Categorical"
  dataPart[1,6] <- "Score" 
  dataPart[1,7] <- "Comment" 
  names(dataPart) <- dataPart[1,]
  dataPart <- dataPart[-1,]
  
  dataPart %>% 
    select(Code, Dimension, Subject, Indicator, Score, Comment) %>% 
    gather(Var, Val, c("Score", "Comment")) %>% 
    mutate(Sheet = oneSheet) %>% 
    left_join(mutate(generalData, Sheet = oneSheet), by = "Sheet") %>% 
    mutate(file = pathToFile)
  
  }


harvesOneFile <- 
  function(pathToFile) {
    
    allSheets <-
      excel_sheets(pathToFile) %>% 
      .[!. %in% c("O que fazer", "BASE LINK",
                  "Interpretação", "Erechim BASE LINK", 
                  "Getulio Vargas BASE LINK", "Sertão BASE LINK",
                  "Interpretacao", "Três Arroios BASE LINK",
                  "VIADUTOS BASE LINK", "Plan1")]
    
    map_df(allSheets, function(x) harvesOneSheet(x, pathToFile = pathToFile))
  }


# Harvesting data ---------------------------------------------------------

pathToFile <- file.path(rootFolder, files[6])
safaBrazilData <-
  files %>%
  map_df(function(x) {
    pathToFile <- file.path(rootFolder, x)
    harvesOneFile(pathToFile)
  })

# Mapping categories


# Saving data -------------------------------------------------------------

write_csv(safaBrazilData, path = "output/SAFA_data_brazil.csv")
write_excel_csv(safaBrazilData, path = "output/SAFA_data_brazil_excel.csv")


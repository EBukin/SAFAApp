countryData


# install.packages("ggmap")
# install.packages("mapproj")
# install.packages("tmap")
# install.packages("countrycode")
  

library(tidyverse)
library(countrycode)
library(ggmap)
library(mapproj)
devtools::install_github('UNFAOstatistics/gisfao')
library(gisfao)
map <- get_map()
ggmap(map)



# Process of plotting FAO data on the map:

# Using package tmap for plotting
library(tmap)

# Using FAO world map shape datafile from the package: gisfao
devtools::install_github('UNFAOstatistics/gisfao')
library(gisfao)

# Loading FAO world shape data file
data(fao_world)

# Grid lines
data(graticule)

# Actual data with countrycodes
ActualData <- 
  countryData %>% 
  group_by(Country, Var) %>% 
  dplyr::summarise(Val = sum(Val)) %>% 
  ungroup() %>% 
  spread(Var, Val) %>% 
  dplyr::filter(Country != "", !is.na(Country)) %>% 
  dplyr::mutate(FAOST_CODE = countrycode(Country, "iso2c", "fao")) %>% 
  # dplyr::filter(is.na(FAOST_CODE)) %>% 
  dplyr::mutate(FAOST_CODE = ifelse(as.character(Country) == "PS", 299, FAOST_CODE),
         FAOST_CODE = ifelse(as.character(Country) == "PR", 177, FAOST_CODE),
         FAOST_CODE = ifelse(as.character(Country) == "RE", 182, FAOST_CODE)) %>% 
  right_join(tibble(FAOST_CODE = fao_world$FAO_CODE), by = "FAOST_CODE") %>% 
  select(-Country)

# Adding data variables
fao_world$Installs <- ActualData$Installs
fao_world$Uninstalls <- ActualData$Uninstalls

# Plotting the map
SafaMap <- 
  tm_shape(fao_world, projection="robin") +
  # tm_borders() +
  tm_polygons("Installs", palette="Blues", 
              style = "kmeans", 
              contrast = 1, 
              id="name", 
              title="SAFA App usage",
              legend.z = 1000) +
  tm_grid(projection="longlat", labels.size = 0, alpha = .8) + 
  tm_style_white() + 
  tm_format_World()

save_tmap(SafaMap, "output/Safa_World_map.png", width=1920, height=1080)




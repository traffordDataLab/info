library(shiny) ; library(tidyverse) ; library(sf) ; library(rmarkdown) ;
library(leaflet) ; library(plotly) ; library(DT)

wards <- st_read("https://www.traffordDataLab.io/spatial_data/ward/2017/trafford_ward_generalised.geojson", quiet = TRUE)
# load R packages  ---------------------------
library(shiny) ; library(tidyverse) ; library(sf) ;
library(DT)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

wards <- st_read("https://www.traffordDataLab.io/spatial_data/ward/2017/trafford_ward_generalised.geojson", quiet = TRUE)
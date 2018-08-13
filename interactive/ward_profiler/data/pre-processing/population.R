# Mid-Year Population Estimates (2016)
# Source: ONS
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates
# Licence: Open Government Licence

# load libraries ---------------------------
library(tidyverse) ; library(readxl)

# load data ---------------------------
url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/wardlevelmidyearpopulationestimatesexperimental/mid2016sape19dt8/sape19dt8mid2016ward2016syoaestimatesunformatted.zip"
download.file(url, dest = "sape19dt8mid2016ward2016syoaestimatesunformatted.zip")
unzip("sape19dt8mid2016ward2016syoaestimatesunformatted.zip", exdir = ".")
file.remove("sape19dt8mid2016ward2016syoaestimatesunformatted.zip")

# tidy data ---------------------------
total <- read_excel("SAPE19DT8-mid-2016-ward-2016-syoa-estimates-unformatted.xls", sheet = 2, skip = 3) %>% 
  filter(`Local Authority` == 'Trafford') %>% 
  select(-`Local Authority`) %>% 
  rename(area_code = `Ward Code 1`, area_name = `Ward Name 1`, All = `All Ages`, `90` = `90+`) %>% 
  mutate(period = "2016", gender = "Total") %>% 
  select(period, area_code, area_name, gender, everything())

female <- read_excel("SAPE19DT8-mid-2016-ward-2016-syoa-estimates-unformatted.xls", sheet = 4, skip = 3) %>% 
  filter(`Local Authority` == 'Trafford') %>% 
  select(-`Local Authority`) %>% 
  rename(area_code = `Ward Code 1`, area_name = `Ward Name 1`, All = `All Ages`, `90` = `90+`) %>% 
  mutate(period = "2016", gender = "Female") %>% 
  select(period, area_code, area_name, gender, everything())

male <- read_excel("SAPE19DT8-mid-2016-ward-2016-syoa-estimates-unformatted.xls", sheet = 3, skip = 3) %>% 
  filter(`Local Authority` == 'Trafford') %>% 
  select(-`Local Authority`) %>% 
  rename(area_code = `Ward Code 1`, area_name = `Ward Name 1`, All = `All Ages`, `90` = `90+`) %>% 
  mutate(period = "2016", gender = "Male") %>% 
  select(period, area_code, area_name, gender, everything())

population <- bind_rows(total, female, male) %>% 
  mutate(geography = "Ward") %>% 
  select(geography, period, area_code, area_name, everything()) %>% 
  select(-All) %>% 
  gather(age, n, -geography, -period, -area_code, -area_name, -gender) %>% 
  mutate(age = as.integer(age))

rm(total, female, male)

# write results ---------------------------
write_csv(population, "population.csv")

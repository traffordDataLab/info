## Languages: Main languages spoken in Clifford ward, 2011 ##

# source: 2011 Census
# publisher: nomis
# publisher URL: https://www.nomisweb.co.uk/census/2011/qs204ew

# load R packages  ---------------------------
library(tidyverse) ; library(stringr) ; library(waffle)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data  ---------------------------
df <- read_csv("https://www.nomisweb.co.uk/api/v01/dataset/nm_525_1.bulk.csv?time=latest&measures=20100&rural_urban=total&geography=TYPE295")

# manipulate data ---------------------------
df_tidy <- df %>% 
  select(-c(date, `Rural Urban`), 
         -contains("Total"),
         area_code = `geography code`, area_name = geography, `All languages` = `Main Language: All usual residents aged 3 and over; measures: Value`) %>% 
  filter(area_code %in% c("E05000819", "E05000820", "E05000821", "E05000822", "E05000823", "E05000824", 
                          "E05000825", "E05000826", "E05000827", "E05000828", "E05000829", "E05000830", 
                          "E05000831", "E05000832", "E05000833", "E05000834", "E05000835", "E05000836",
                          "E05000837", "E05000838", "E05000839")) %>% 
  gather(language, n, -area_code, -area_name) %>% 
  mutate(language = str_replace(language, "Main Language: ", "")) %>% 
  mutate(language = str_replace(language, "African Language: |Caribbean Creole: |East Asian Language: |Other European Language \\(EU\\): |Other European Language \\(non EU\\): |Other European Language \\(non-national\\): |Other Languages: |Other UK language: |Sign Language: |South Asian Language: |West/Central Asian Language: ", "")) %>% 
  mutate(language = str_replace(language, "; measures: Value", "")) %>% 
  spread(language, n) %>% 
  select(area_code, area_name, `All languages`, everything())

# plot data ---------------------------
results <- df_tidy %>% 
  gather(language, n, -c(area_code, area_name, `All languages`)) %>% 
  filter(area_name == "Clifford" & n != 0) %>% 
  mutate(percent = round(n/`All languages`*100, 1)) %>% 
  arrange(desc(percent)) %>% 
  select(-`All languages`)

languages <- c(`English`= 8464, `Gujarati` = 564, `Urdu` = 555, `Polish` = 273, 
               `Panjabi` = 264, `Arabic` = 216, `Somali` = 172,
               `Other languages (61)` = 746)

waffle(round((languages/sum(results$n)) * 100, 0), rows = 10, size = 1,
       xlab = "1 square = 1% of residents",
       legend_pos = "right")


# save plot / data  ---------------------------
ggsave(file = "output/figures/fig3.svg", width = 8, height = 6)
ggsave(file = "output/figures/fig3.png", width = 8, height = 6)

write_csv(results, "output/data/fig3.csv")

## Languages: Top ten main other languages in Trafford ##

# source: 2011 Census
# publisher: nomis
# publisher URL: https://www.nomisweb.co.uk/census/2011/qs204ew

# load R packages  ---------------------------
library(tidyverse) ; library(stringr)

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
  gather(language, n, -c(area_code, area_name)) %>% 
  filter(language != "All languages" & language != "English (English or Welsh if in Wales)" & n != 0) %>% 
  group_by(language) %>% 
  summarise(n = sum(n)) %>% 
  top_n(10) %>% 
  mutate(area_code = "E08000009", area_name = "Trafford") %>% 
  select(area_code, area_name, language, n) %>% 
  arrange(n) %>% 
  mutate(language = factor(language, levels = language)) 

ggplot(results, aes(n, language)) +
  geom_segment(aes(x = 0, y = language, xend = n, yend = language), color = "#f0f0f0") +
  geom_point(colour = "#fc6721", size = 6) +
  geom_text(aes(label = scales::comma(n), fontface = "bold"), color = "white", size = 2) + 
  scale_x_continuous(expand = c(0, 0), limits = c(0, max(results$n) * 1.1)) +
  labs(x = "Number of residents", y = NULL,
       title = NULL,
       caption = "Source: Census 2011  |  @traffordDataLab") +
  theme_lab() + 
  theme(panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(hjust = 0))

# save plot / data  ---------------------------
ggsave(file = "output/figures/fig2.svg", width = 8, height = 6)
ggsave(file = "output/figures/fig2.png", width = 8, height = 6)

write_csv(results, "output/data/fig2.csv")

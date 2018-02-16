## Languages: Top ten main 'Other' languages in Trafford ##

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
  filter(language %in% c("All languages", "English (English or Welsh if in Wales)") & n != 0) %>% 
  spread(language, n) %>% 
  mutate(`English` = (`English (English or Welsh if in Wales)`/`All languages`),
    `Other` = (`All languages`-`English (English or Welsh if in Wales)`)/`All languages`) %>% 
  select(-c(`All languages`, `English (English or Welsh if in Wales)`))

results %>% 
  gather(language, percent, -c(area_code, area_name)) %>% 
  ggplot(aes(x = fct_rev(area_name), y = percent, fill = language)) + 
  geom_col(position = "stack",  alpha = 0.8) +
  scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
  scale_fill_manual(values = c("English" = "#a6cee3", "Other" = "#1f78b4"),
                    guide = guide_legend(keyheight = unit(2, units = "mm"), 
                                         keywidth = unit(12, units = "mm"), 
                                         label.position = "top", 
                                         label.vjust = 1,
                                         nrow = 1,
                                         reverse = TRUE)) +
  coord_flip() +
  labs(x = NULL, y = NULL, fill = NULL, title = NULL,
       caption = "Source: 2011 Census  |  @traffordDataLab") +
  theme_lab() +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(hjust = 0),
        legend.position = "bottom")

# save plot / data  ---------------------------
ggsave(file = "output/figures/fig1.svg", width = 8, height = 6)
ggsave(file = "output/figures/fig1.png", width = 8, height = 6)

write_csv(results, "output/data/fig1.csv")

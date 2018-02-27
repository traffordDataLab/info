## Religion: Religious affiliation by ward, 2011 ##

# Source: Table KS209EW, 2011 Census
# Publisher: nomis
# Publisher URL: https://www.nomisweb.co.uk/census/2011/ks209ew
# Licence: Open Government Licence

# load R packages  ---------------------------
library(tidyverse)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data  ---------------------------
df <- read_csv("https://www.nomisweb.co.uk/api/v01/dataset/nm_616_1.bulk.csv?time=latest&measures=20100&rural_urban=total&geography=TYPE295")

# manipulate data ---------------------------
df_tidy <- df %>% 
  select(-c(date, `Rural Urban`), area_code = `geography code`, area_name = geography) %>% 
  filter(area_code %in% c("E05000819", "E05000820", "E05000821", "E05000822", "E05000823", "E05000824", 
                          "E05000825", "E05000826", "E05000827", "E05000828", "E05000829", "E05000830", 
                          "E05000831", "E05000832", "E05000833", "E05000834", "E05000835", "E05000836",
                          "E05000837", "E05000838", "E05000839")) %>% 
  gather(religion, n, -area_code, -area_name) %>% 
  mutate(religion = str_replace(religion, "Religion: ", "")) %>% 
  mutate(religion = str_replace(religion, "; measures: Value", ""))

# plot data ---------------------------
results <- df_tidy %>% 
  filter(religion %in% c("Buddhist", "Christian", "Hindu", "Jewish", "Muslim", "Other religion", "Sikh"))
bg <- df_tidy %>% 
  filter(religion == "Has religion") %>% 
  select(area_name, n)

ggplot(results, aes(x = area_name, y = n)) + 
  geom_col(data = bg, fill = "#757575", alpha = 0.2) +
  geom_col(fill = "#fc6721", alpha = 0.8) +
  geom_text(aes(label = scales::comma(n)), colour = "#212121", size = 2, nudge_y = 1100) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 10000)) +
  facet_wrap(~religion, nrow = 7, strip.position = "left") +
  labs(x = NULL, y = NULL, fill = NULL, title = NULL,
       caption = "Source: Census 2011, Office for National Statistics  |  @traffordDataLab") +
  theme_lab() +
  theme(panel.grid.major = element_blank(),
        strip.text.y = element_text(size = 10, hjust = 0, vjust = 0, angle = 180,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 10, hjust = 1, vjust = 0.1, angle = 90))

# save plot / data  ---------------------------
ggsave(file = "output/figures/fig3.svg", width = 8, height = 6)
ggsave(file = "output/figures/fig3.png", width = 8, height = 6)

write_csv(results, "output/data/fig3.csv")

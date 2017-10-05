
## Instructions for loading the custom [ggplot2](http://ggplot2.tidyverse.org/) Trafford Data Lab theme

**1. Load the ggplot2 package**
``` r
library(ggplot2)
```

**2. Provide a source path to the URL containing the custom theme**
``` r
source("https://raw.githubusercontent.com/traffordDataLab/information/master/R/theme/theme_lab.R")
```

**3. Run theme_set() to override the current theme with the Trafford Data Lab theme**
``` r
theme_set(theme_lab())
```

---

### An example 
``` r
# load packages
library(ggplot2) ; library(tidyverse) ; library(scales)

# load some data
df <- data.frame(
  religion = c("Christian", "Buddhist", "Hindu", "Jewish", "Muslim", "Sikh", "Other Religion", "No Religion", "Not Stated"),
  count = c(143639, 768, 2271, 2413, 12994, 1652, 566, 47968, 14307)
)

# create a ggplot object
plot <- df %>%  
  arrange(count) %>% 
  mutate(religion = factor(religion, levels = religion)) %>% 
  ggplot(aes(religion, count)) +
  geom_col(fill = "#fc6721", alpha = 0.8, show.legend = FALSE) +
  geom_text(aes(label = comma(count)), colour = "#212121", size = 3.3, hjust = 0, nudge_y = 2000) +
  scale_y_continuous(label = comma, limits=c(0, 170000), expand = c(0,0)) +
  coord_flip() +
  labs(x = NULL, y = "Residents",
       title = "A fifth of Trafford's residents have no religion",
       subtitle = "Religious affiliation, 2011",
       caption = "Source: Table KS209EW, Census 2011  |  @traffordDataLab")

# style with the Trafford Data Lab theme and tweak
plot + theme_lab() +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y = element_text(hjust = 0))

# save as png, svg or pdf
ggsave("religion.png", dpi = 300, scale = 1)
ggsave("religion.svg", dpi = 300, scale = 1)
ggsave("religion.pdf", device = cairo_pdf, scale = 1.2, width = 6, height = 6)
``` 

<br />

<img src="https://github.com/traffordDataLab/information/blob/master/R/theme/rligion.png" width="500">

## Bicycle theft: time series with trend ##

# load R packages  ---------------------------
library(tidyverse); library(ggplot2); library(svglite)

# load Lab's ggplot2 theme  ---------------------------
source("https://github.com/traffordDataLab/assets/raw/master/theme/ggplot2/theme_lab.R")

# load data  ---------------------------
df <- read_csv("https://github.com/traffordDataLab/open_data/raw/master/police_recorded_crime/data/trafford.csv") %>% 
  filter(category == "Bicycle theft")

# manipulate data ---------------------------
count <- df %>% group_by(month) %>% count()
ts_crime <- ts(count$n, start = c(2014, 12), end = c(2017, 11), frequency = 12)
ts_decomp = stl(ts_crime, "periodic")
plot(ts_decomp)

results <- data.frame(
  month = seq(as.Date("2014-12-01"), by = "month", length.out = 36),
  Observed = as.vector(ts_crime),
  Trend = ts_decomp$time.series[,2]
) %>% gather(type, value, -month) %>% 
  mutate(type = factor(type, levels = c("Observed", "Trend")))

# plot data ---------------------------
ggplot(data = results, aes(month, value)) +
  geom_line(colour = "#fc6721", size = 1) + 
  scale_x_date(date_labels = "%b-%y", date_breaks = "3 month", expand = c(0,0)) +
  scale_y_continuous(position = "right", limits = c(0, 70)) + # change axis limits
  labs(x = NULL, y = NULL,
       caption = "Source: data.police.uk  |  @traffordDataLab") +
  facet_grid(type ~ ., scales = "free", switch = "y") +
  theme_lab() +
  theme(axis.text.x  = element_text(angle = 90),
        panel.spacing = unit(2, "lines"),
        strip.text.y = element_text(angle = 0, vjust = 1, hjust = 0))

# save plot / data  ---------------------------
ggsave(file = "output/figures/fig2.svg", width = 6, height = 5)
ggsave(file = "output/figures/fig2.png", width = 6, height = 5)
write_csv(spread(results, type, value) %>% rename(observed = Observed, trend = Trend), 
          "output/data/fig2.csv")

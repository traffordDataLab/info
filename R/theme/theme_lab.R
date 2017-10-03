theme_lab <- function () { 
  theme_minimal(base_size = 11.5, base_family = "Roboto") %+replace% 
    theme(
      # add padding to the plot
      plot.margin = unit(rep(0.5, 4), "cm"),
      # remove the panel background and border
      panel.background = element_blank(),
      panel.border = element_blank(),
      # add a light grey background
      plot.background = element_rect(fill = "#f0f0f0", colour = NA), 
      # make the legend and key transparent
      legend.background = element_rect(fill = "transparent", colour = NA),
      legend.key = element_rect(fill = "transparent", colour = NA),
      # add light, dotted major grid lines only
      panel.grid.major = element_line(linetype = "dotted", colour = "#d9d9d9", size = 0.5),
      panel.grid.minor = element_blank(),
      # remove the axis ticks
      axis.ticks = element_blank(),
      # modify the bottom margins of both the title and subtitle
      plot.title = element_text(size = 18, colour = "#757575", hjust = 0, margin = margin(b = 15)),
      plot.subtitle = element_text(size = 12, colour = "#757575", hjust = 0, margin = margin(b = 5)),
      # add padding to the caption
      plot.caption = element_text(size = 10, colour = "#212121", hjust = 1, margin = margin(t = 15)),
      # change to Open Sans for axes titles, labels, legend, and strip 
      axis.title = element_text(family = "Open Sans", size = 11, colour = "#757575", face = "plain", hjust = 1),
      axis.text = element_text(family = "Open Sans", size = 10, colour = "#757575", face = "plain"),
      legend.title = element_text(size = 12, colour = "#757575"),
      legend.text = element_text(size = 10, colour = "#757575"),
      strip.text = element_text(family = "Open Sans",size = 12, colour = "#757575", face = "plain")
    )
}

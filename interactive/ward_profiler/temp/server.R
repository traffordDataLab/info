
tenure <- read_csv("/Users/henrypartridge/Documents/GitHub/info/interactive/ward_profiler/data/housing/tenure.csv")
house_prices <- read_csv("/Users/henrypartridge/Documents/GitHub/info/interactive/ward_profiler/data/housing/house_prices.csv")
house_prices_points <- st_read("/Users/henrypartridge/Documents/GitHub/info/interactive/ward_profiler/data/housing/house_prices.geojson") %>% 
  st_set_geometry(value = NULL)
housing_type <- read_csv("/Users/henrypartridge/Documents/GitHub/info/interactive/ward_profiler/data/housing/housing_type.csv")
housing_size <- read_csv("/Users/henrypartridge/Documents/GitHub/info/interactive/ward_profiler/data/housing/housing_size.csv")

shinyServer(function(input, output) {
   
  output$housing_text <- renderText({ 
    
    rented <- filter(tenure, area_name == input$ward) %>% 
      mutate(percent = round(n/sum(n)*100, 1)) %>% 
      filter(type == "Private rented") %>% 
      select(percent)
    owned <- filter(tenure, area_name == input$ward) %>% 
      mutate(percent = round(n/sum(n)*100, 1)) %>% 
      filter(type == "Owned") %>% 
      select(percent)
    social <- filter(tenure, area_name == input$ward) %>% 
      mutate(percent = round(n/sum(n)*100, 1)) %>% 
      filter(type == "Social rented") %>% 
      select(percent)
  
    text1 <- paste0("According to the 2011 Cenus, ", rented, "% of households were rented, ",
                    owned, "% were owned and ", social, "% were socially rented.")
    
    transactions <- prettyNum(select(filter(house_prices, area_name == input$ward), transactions), big.mark = ",", scientific = FALSE)
    median_house_price <- prettyNum(select(filter(house_prices, area_name == input$ward), median_price), big.mark = ",", scientific = FALSE)
    min_price <- prettyNum(select(filter(house_prices, area_name == input$ward), min_price), big.mark = ",", scientific = FALSE)
    max_price <- prettyNum(select(filter(house_prices, area_name == input$ward), max_price), big.mark = ",", scientific = FALSE)
    
    text2 <- paste0(transactions,  " properties were sold in ", input$ward, " during 2017 with a median value of £", median_house_price, 
                    ". The price of property ranged from £", min_price, " to £", max_price, ".")
    
    HTML(paste0("<h4 style='text-align: center;'>Summary</h4><br/><ul><li>", text1, '</li><li>',
                text2, "</li></ul>"))
    
  })
  
  output$housing_prices <- renderPlot({
    
    bands <- c("<=£100,000", 
               "£100,001 to £200,000",
               "£200,001 to £300,000",
               "£300,001 to £400,000",
               "£400,001 to £500,000",
               "£500,001 to £600,000",
               "£600,001 to £700,000",
               "£700,001 to £800,000",
               "£800,001 to £900,000",
               "£900,001 to £1,000,000",
               ">£1,000,000")
    
    ward <- filter(house_prices_points, area_name == input$ward) %>% 
      mutate(band = cut(amount,
                        breaks = c(0,100000,200000,300000,400000,500000,600000,700000,
                                   800000,900000,1000000,Inf),
                        labels = bands,
                        right = FALSE),
             band = factor(band, 
                           levels = bands)) %>% 
      count(band) %>% 
      complete(band, fill = list(n = 0)) %>% 
      mutate(percent = n/sum(n),
             geography = input$ward)
    
    la <- house_prices_points %>% 
      mutate(band = cut(amount,
                        breaks = c(0,100000,200000,300000,400000,500000,600000,700000,
                                   800000,900000,1000000,Inf),
                        labels = bands,
                        right = FALSE),
             band = factor(band, 
                           levels = bands)) %>% 
      count(band) %>% 
      mutate(percent = n/sum(n),
             geography = "Trafford")
    
    temp <- bind_rows(ward, la) %>% 
      mutate(geography = factor(geography, levels = c(input$ward, "Trafford"), ordered = TRUE))
    
    ggplot(temp, aes(percent, band)) +
      geom_line(aes(group = band), colour = "#212121", size = 0.2) +
      geom_point(aes(color = geography), alpha = 0.8, size = 3) +
      scale_colour_manual(values = c("#fc6721", "#757575"), drop = FALSE)+
      scale_x_continuous(expand = c(0.02, 0), labels = scales::percent) +
      labs(title = "",
           subtitle = paste0("Property transactions in ", input$ward, " by price band, 2017"),
           caption = "Source: Land Registry  |  @traffordDataLab",
           x = NULL,
           y = NULL,
           colour = NULL) +
      theme_lab() +
      theme(panel.grid.major.x = element_blank(),
            plot.subtitle = element_text(face = "bold", hjust = 0.5),
            legend.position = "bottom",
            legend.direction = "horizontal")
  })
  
  output$housing_type <- renderPlot({
    
    temp <- filter(housing_type, area_name == input$ward) %>% 
      mutate(type = factor(type, levels = c("Detached", "Semi-detached", "Terraced", "Flat", "Caravan", ordered = TRUE)),
             percent = n/sum(n))
    
    ggplot(temp, aes(x = type, y = percent, fill = type)) + 
      geom_col(alpha = 0.8) +
      geom_text(aes(label = scales::percent(percent)), colour = "#212121", fontface = "bold", size = 3.5, vjust = -0.5) +
      scale_fill_brewer(palette = "Set2") +
      scale_y_continuous(expand = c(0, 0), labels = scales::percent) +
      labs(title = "",
           subtitle = paste0("Housing types in ", input$ward, ", 2011"),
           caption = "Source: 2011 Census, ONS  |  @traffordDataLab",
           x = NULL,
           y = NULL) +
      theme_lab() +
      theme(panel.grid.major = element_blank(),
            axis.text.y = element_blank(),
            plot.subtitle = element_text(face = "bold", hjust = 0.5, vjust = 2),
            legend.position = "none") +
      expand_limits(y = max(temp$percent * 1.1))
    
  })
  
  output$housing_size <- renderPlot({
    
    temp <- filter(housing_size, area_name == input$ward) %>% 
      mutate(percent = n/sum(n),
             bedrooms = factor(bedrooms, levels = as.character(bedrooms)),
             position = (cumsum(c(0, percent)) + c(percent / 2, .01))[1:nrow(.)])
    
    ggplot(temp, aes(x = "", y = percent, fill = bedrooms)) +
      geom_col(width = 1, color = "#FFFFFF", alpha = 0.8,
               position = position_stack(reverse = TRUE)) +
      geom_text_repel(aes(x = 1.4, y = position, label = percent(round(percent, 3))), size = 3, nudge_x = 0.25, segment.color = "#212121", segment.size = 0.1) +
      coord_polar(theta = "y", start = 0, direction = 1) +
      scale_fill_brewer(palette = "Set1",
                        guide = guide_legend(label.position = "right",
                                             keyheight = unit(5, units = "mm"), 
                                             keywidth = unit(10, units = "mm"), 
                                             direction = "vertical")) +
      labs(title = NULL,
           subtitle = paste0("Housing size in ", input$ward, ", 2011"),
           caption = "Source: 2011 Census, ONS  |  @traffordDataLab",
           x = NULL, 
           y = NULL, 
           fill = NULL) +
      theme_void() +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
            plot.subtitle = element_text(size = 12, colour = "#757575", hjust = 0.5, margin = margin(b = 10),
                                         face = "bold", vjust = 2))
    
  })
})

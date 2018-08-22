
tenure <- read_csv("/Users/henrypartridge/Documents/GitHub/info/interactive/ward_profiler/data/housing/tenure.csv")
house_prices <- read_csv("/Users/henrypartridge/Documents/GitHub/info/interactive/ward_profiler/data/housing/house_prices.csv")
house_prices_points <- st_read("/Users/henrypartridge/Documents/GitHub/info/interactive/ward_profiler/data/housing/house_prices.geojson") %>% 
  st_set_geometry(value = NULL)

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
  
  
  output$housing_map <- renderPlot({
    
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
    
})

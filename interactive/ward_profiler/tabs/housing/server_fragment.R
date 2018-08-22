# Housing #

# load data ----------------------------------------
tenure <- read_csv("data/housing/tenure.csv")

# text ----------------------------------------
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
  
  HTML(paste0("<h4 style='text-align: center;'>Summary</h4><br/><ul><li>", text1, "</li></ul>"))

})
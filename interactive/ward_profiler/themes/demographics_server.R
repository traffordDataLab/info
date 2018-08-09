# Demographics #

# load data ----------------------------------------
# Mid-year population estimates (2016)
pop <- read_csv("https://www.traffordDataLab.io/open_data/mid-year_pop_estimates_2016/mid-2016_population_estimates.csv") %>% 
  filter(geography %in% c("Local Authority", "Ward")) %>% 
  select(-All) %>% 
  gather(age, count, -year, -area_code, -area_name, -geography, -gender) %>% 
  mutate(age = as.integer(age))

# text ----------------------------------------
output$demographics_text <- renderText({ 
  
  total_pop <- sum(select(filter(pop, area_name == input$ward, gender == "Total"), count))
  female_pop <- sum(select(filter(pop, area_name == input$ward, gender == "Female"), count))
  male_pop <- sum(select(filter(pop, area_name == input$ward, gender == "Male"), count))
  
  text1 <- paste0("The resident population of ", input$ward, " in 2016 was ", 
         prettyNum(total_pop, big.mark = ",", scientific = FALSE), ". ", 
         round((female_pop/total_pop)*100,1), "% of residents were female (", 
         prettyNum(female_pop, big.mark = ",", scientific = FALSE), ") and ",
         round((male_pop/total_pop)*100,1), "% (", 
         prettyNum(male_pop, big.mark = ",", scientific = FALSE), ") male.")
  
  child_pop <- sum(select(filter(pop, area_name == input$ward, gender == "Total",
                                 age %in% c(0:15)), count))
  working_pop <- sum(select(filter(pop, area_name == input$ward, gender == "Total",
                                   age %in% c(16:64)), count))
  older_pop <- sum(select(filter(pop, area_name == input$ward, gender == "Total",
                                 age %in% c(65:90)), count))
  
  text2 <- paste0("The proportion of residents aged between 0-15 years was ",
                  round((child_pop/total_pop)*100,1), "% (",
                  prettyNum(child_pop, big.mark = ",", scientific = FALSE), "). ", 
                  round((working_pop/total_pop)*100,1), "% (",
                  prettyNum(working_pop, big.mark = ",", scientific = FALSE),
                  ") were aged between 16-64 years and ",
                  round((older_pop/total_pop)*100,1), "% (",
                  prettyNum(older_pop, big.mark = ",", scientific = FALSE),
                  ") were aged 65 years or more.")
  
  HTML(paste0("<p>", text1, "</p><p>", text2, "</p>"))
  
})

# population pyramid ----------------------------------------
output$demographics_pyramid <- renderPlot({
  temp <- filter(pop, area_name %in% c("Trafford", input$ward), gender != "Total") %>% 
    mutate(age = as.integer(age),
           ageband = cut(age,
                         breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,120),
                         labels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
                                    "40-44","45-49","50-54","55-59","60-64","65-69","70-74",
                                    "75-79","80-84","85-89","90+"),
                         right = FALSE)) %>% 
    group_by(geography, gender, ageband) %>% 
    summarise(n = sum(count)) %>%
    mutate(percent = round(n/sum(n)*100, 1),
           percent = 
             case_when(
               gender == "Male" ~ percent*-1,
               TRUE ~ as.double(percent)))
  
  ggplot(temp, aes(x = ageband, colour = gender)) +
    geom_linerange(data = filter(temp, geography == "Ward", gender == "Female"), 
                   aes(ymin = 0.7, ymax = 0.7+percent), size = 5, alpha = 0.6) +
    geom_linerange(data = filter(temp, geography == "Ward", gender == "Male"), 
                   aes(ymin = -0.7, ymax = -0.7+percent), size = 5, alpha = 0.6) +
    geom_line(aes(ageband, 0.7+percent, group = gender),
              filter(temp, geography == "Local Authority", gender == "Female"),
              size = 1, colour = "#d8b365", alpha = 1) +
    geom_line(aes(ageband, -0.7+percent, group = gender),
              filter(temp, geography == "Local Authority", gender == "Male"),
              size = 1, colour = "#5ab4ac", alpha = 1) +
    geom_label(aes(x = ageband, y = 0, label = ageband), 
               label.size = 0, label.padding = unit(0.0, "lines"), label.r = unit(0.0, "lines"),
              size = 3.5, fill = "#FFFFFF", color = "#757575") +
    scale_y_continuous(breaks = c(c(-10, -8, -6, -4, -2, 0)+-0.7, c(0, 2, 4, 6, 8, 10)+0.7),
                       labels = c("10%", "8%", "6%", "4%", "2%", "0%", "0%", "2%", "4%", "6%", "8%", "10%"),
                       expand = c(0,0)) +
    scale_color_manual(name = NULL, 
                       values = c("#d8b365", "#5ab4ac"), 
                       labels = c("Female", "Male")) +
    coord_flip() +
    labs(x = NULL, y = NULL, 
         title = NULL,
         subtitle = paste0("Age composition of ", input$ward, "\nand Trafford, mid-2016"),
         caption = "Source: ONS 2016 Mid-Year estimates |  @traffordDataLab") +
    guides(colour = guide_legend(reverse = TRUE)) +
    theme_lab() +
    theme(panel.grid.major.y = element_blank(),
          axis.text.x = element_text(size = 12, hjust = 1),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.subtitle = element_text(size = 14, hjust = 0.5),
          legend.position = "bottom") 
  })

# raw data ----------------------------------------
output$demographics_data <- renderDataTable({
  
  filter(pop, area_name == input$ward, gender != "Total") %>% 
    select(Year = year,
           `Area code` = area_code,
           `Area name` = area_name,
           Geography = geography,
           Gender = gender,
           Age = age,
           Count = count) %>% 
    group_by(`Area code`) %>% 
    arrange(Age)
}, 
extensions= c('Buttons', "Scroller"), 
rownames = FALSE,  
options=list(
  dom = 'Blfrtip',
  deferRender = TRUE,
  scroller = TRUE, 
  scrollX = TRUE,
  scrollY = "300px",
  columnDefs = list(list(className = 'dt-left', targets = 0:6)), 
  buttons = list('copy', 'csv', 'pdf')))
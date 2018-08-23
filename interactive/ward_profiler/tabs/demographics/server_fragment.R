# Demographics #

# load data ----------------------------------------
population <- read_csv("data/demographics/population.csv")
languages <- read_csv("data/demographics/languages.csv")
ethnicity <- read_csv("data/demographics/ethnicity.csv")

# text ----------------------------------------
output$demographics_text <- renderText({ 
  
  total_pop <- sum(select(filter(population, area_name == input$ward, gender == "Total"), n))
  female_pop <- sum(select(filter(population, area_name == input$ward, gender == "Female"), n))
  male_pop <- sum(select(filter(population, area_name == input$ward, gender == "Male"), n))
  
  text1 <- paste0("The resident population of ", input$ward, " in 2016 was ", 
         prettyNum(total_pop, big.mark = ",", scientific = FALSE), ". ", 
         round((female_pop/total_pop)*100,1), "% of residents were female (", 
         prettyNum(female_pop, big.mark = ",", scientific = FALSE), ") and ",
         round((male_pop/total_pop)*100,1), "% (", 
         prettyNum(male_pop, big.mark = ",", scientific = FALSE), ") male.")
  
  child_pop <- sum(select(filter(population, area_name == input$ward, gender == "Total",
                                 age %in% c(0:15)), n))
  working_pop <- sum(select(filter(population, area_name == input$ward, gender == "Total",
                                   age %in% c(16:64)), n))
  older_pop <- sum(select(filter(population, area_name == input$ward, gender == "Total",
                                 age %in% c(65:90)), n))
  
  text2 <- paste0("The proportion of residents aged between 0-15 years was ",
                  round((child_pop/total_pop)*100,1), "% (",
                  prettyNum(child_pop, big.mark = ",", scientific = FALSE), "), ", 
                  round((working_pop/total_pop)*100,1), "% (",
                  prettyNum(working_pop, big.mark = ",", scientific = FALSE),
                  ") were aged between 16-64 years and ",
                  round((older_pop/total_pop)*100,1), "% (",
                  prettyNum(older_pop, big.mark = ",", scientific = FALSE),
                  ") were aged 65 years or more.")
  
  median_age <- filter(population, area_name == input$ward, gender == "Total") %>% 
    arrange(age) %>% 
    summarise(median_age = age[max(which(cumsum(n)/sum(n) <= 0.5))])
  
  text3 <- paste0("The median age in ", input$ward, " is ", median_age, ".")
  
  old_age_dependency <- filter(population, area_name == input$ward, gender == "Total") %>% 
    arrange(age) %>% 
    mutate(ageband = cut(age, breaks = c(15,65,91), labels = c("15-64","65+"), right = FALSE)) %>% 
    filter(!is.na(ageband)) %>% 
    group_by(ageband) %>% 
    summarise(n = sum(n)) %>% 
    spread(ageband, n) %>% 
    mutate(ratio = round((`65+`/`15-64`)*100,0)) %>% 
    select(ratio)
    
   text4 <- paste0("The number of older people (65+ years) in ",
                   input$ward, " is equivalent to ", old_age_dependency,
                   "% of the working age population (15-64 years).")
   
   non_english_spoken <- filter(languages, area_name == input$ward) %>% 
     summarise(percent = round(sum((n[language != "English (English or Welsh if in Wales)"]))/sum(n)*100,1))
   
   text5 <- paste0(non_english_spoken, "% of ", input$ward, 
                   "'s residents speak a language other than English as their main language. ")
   
  HTML(paste0("<h4 style='text-align: center;'>Summary</h4><br/><ul><li>", text1, "</li><li>", text2, "</li><li>", text3, 
              "</li><li>", text4, "</li><li>", text5, "</li></ul>"))
  
})

# population pyramid ----------------------------------------
output$demographics_pyramid <- renderPlot({
  
  trafford <- population %>% group_by(gender, age) %>% 
    summarise(n = sum(n)) %>% 
    mutate(geography = "Local Authority",
           period = 2016,
           area_code = "E08000009",
           area_name = "Trafford") %>% 
    select(geography, period, area_code, area_name, gender, age, n)
  
  temp <- bind_rows(population, trafford) %>% 
    filter(area_name %in% c("Trafford", input$ward), gender != "Total") %>% 
    mutate(age = as.integer(age),
           ageband = cut(age,
                         breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,120),
                         labels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
                                    "40-44","45-49","50-54","55-59","60-64","65-69","70-74",
                                    "75-79","80-84","85-89","90+"),
                         right = FALSE)) %>% 
    group_by(geography, gender, ageband) %>% 
    summarise(n = sum(n)) %>%
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
          plot.subtitle = element_text(face = "bold", hjust = 0.5),
          legend.position = "bottom") 
  })

# languages spoken ----------------------------------------
output$demographics_languages <- renderPlot({
 
  temp <- languages %>% 
    filter(area_name == input$ward, language != "English (English or Welsh if in Wales)", n != 0) %>%
    arrange(desc(n)) %>% 
    mutate(percent = n/sum(n)*100,
           cum.percent = cumsum(percent),
           dominant = case_when(cum.percent >= 50 ~ "Other", TRUE ~ language)) %>% 
    select(n, percent, dominant) %>% 
    group_by(dominant) %>% 
    summarise(n = sum(n),
              percent = round(sum(percent), 0)) %>% 
    arrange(desc(percent)) %>% 
    mutate(dominant = paste0(dominant, " (", percent, "%)"))
  
  parts <- magrittr::extract2(temp, 'percent') %>% 
    set_names(temp$dominant)
  
  waffle(round((parts/sum(temp$percent)) * 100, 0), rows = 10, size = 1,
         colors = (brewer.pal(length(parts), "Set3"))) +
    labs(title = NULL,
         subtitle = paste0("Main non-English languages spoken\nin ", input$ward, ", 2011"),
         subtitle = "1 square = 1% of residents",
         caption = "Source: 2011 Census  |  @traffordDataLab",
         x = NULL, 
         y = NULL, 
         fill = NULL) +
    theme_lab() +
    theme(axis.text = element_blank(),
          plot.subtitle = element_text(face = "bold", hjust = 0.5),
          legend.position = "bottom")

})

# ethnic groups ----------------------------------------
output$demographics_ethnicity <- renderPlot({ 
  
  temp <- ethnicity %>% 
    filter(area_name == input$ward) %>%
    mutate(percent = (n/sum(n)))
  
  pal <- c("Asian" = "#FF0000", "Black" = "#55FF00", "Mixed" = "#FFAA01", "Other" = "#8A5B47", "White" = "#82B3FF")
  
  ggplot(temp, aes(x = area_name, y = percent, fill = fct_reorder(group, percent))) + 
    geom_col(position = "stack", width = 0.3, alpha = 0.8) +
    scale_y_continuous(expand = c(0, 0), labels = percent) +
    scale_fill_manual(values = pal,
                      guide = guide_legend(keyheight = unit(2, units = "mm"), 
                                           keywidth = unit(12, units = "mm"), 
                                           label.position = "top", 
                                           nrow = 1, 
                                           reverse = TRUE)) +
    coord_flip() +
    labs(x = NULL, y = NULL, fill = NULL, 
         title = NULL,
         subtitle = paste0("Self-reported ethnicity of residents\nin ", input$ward, ", 2011"),
                           caption = "Source: 2011 Census  |  @traffordDataLab") +
           theme_lab() +
           theme(panel.grid.major.y = element_blank(),
                 axis.text.y = element_blank(),
                 plot.subtitle = element_text(face = "bold", hjust = 0.5),
                 legend.position = "bottom",
                 aspect.ratio = 1/8)
})
  
  
  
  
  




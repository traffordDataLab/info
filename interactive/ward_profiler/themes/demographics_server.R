# Demographics #

# load data ----------------------------------------
# Mid-year population estimates (2016)
pop <- read_csv("https://www.traffordDataLab.io/open_data/mid-year_pop_estimates_2016/mid-2016_population_estimates.csv") %>% 
  filter(geography %in% c("Local Authority", "Ward")) %>% 
  select(-All) %>% 
  gather(age, count, -year, -area_code, -area_name, -geography, -gender) %>% 
  mutate(age = as.integer(age))

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
                   aes(ymin = 0.5, ymax = 0.5+percent), size = 5, alpha = 0.6) +
    geom_linerange(data = filter(temp, geography == "Ward", gender == "Male"), 
                   aes(ymin = -0.5, ymax = -0.5+percent), size = 5, alpha = 0.6) +
    geom_line(aes(ageband, 0.5+percent, group = gender),
              filter(temp, geography == "Local Authority", gender == "Female"),
              size = 1, colour = "#d8b365", alpha = 1) +
    geom_line(aes(ageband, -0.5+percent, group = gender),
              filter(temp, geography == "Local Authority", gender == "Male"),
              size = 1, colour = "#5ab4ac", alpha = 1) +
    geom_text(aes(x = ageband, y = 0, label = ageband), 
              size = 3.5, color = "#757575") +
    scale_y_continuous(breaks = c(c(-10, -8, -6, -4, -2, 0)+-0.5, c(0, 2, 4, 6, 8, 10)+0.5),
                       labels = c("10%", "8%", "6%", "4%", "2%", "0%", "0%", "2%", "4%", "6%", "8%", "10%"),
                       expand = c(0,0)) +
    scale_color_manual(name = NULL, 
                       values = c("#d8b365", "#5ab4ac"), 
                       labels = c("Female", "Male")) +
    coord_flip() +
    labs(x = NULL, y = NULL, 
         title = NULL,
         subtitle = paste0("Population pyramid for ", input$ward, " and Trafford, mid-2016"),
         caption = "Source: ONS  |  @traffordDataLab") +
    guides(colour = guide_legend(reverse = TRUE)) +
    theme_lab() +
    theme(panel.grid.major.y = element_blank(),
          axis.text.x = element_text(size = 12, hjust = 1),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          plot.subtitle = element_text(size = 12, face = "bold", hjust = 0.5),
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
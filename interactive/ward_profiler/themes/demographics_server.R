pop <- read_csv("https://www.traffordDataLab.io/info/interactive/population_picker/data/population_estimates.csv") %>% 
  filter(geography == "Ward") %>% 
  select(-All) %>% 
  gather(age, count, -year, -area_code, -area_name, -geography, -gender) %>% 
  mutate(age = as.integer(age))

output$demographics_pyramid <- renderPlotly({
  temp <- filter(pop, area_name == input$ward, gender != "Total") %>% 
    mutate(age = as.integer(age),
           ageband = cut(age,
                         breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,120),
                         labels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
                                    "40-44","45-49","50-54","55-59","60-64","65-69","70-74",
                                    "75-79","80-84","85-89","90+"),
                         right = FALSE)) %>% 
    group_by(gender, ageband) %>% 
    summarise(n = sum(count)) %>%
    mutate(percent = round(n/sum(n)*100, 1),
           percent = 
             case_when(
               gender == "Male" ~ percent*-1,
               TRUE ~ as.double(percent)))
  
  plot_ly(temp, x = ~percent, y = ~ageband, color = ~gender) %>% 
    add_bars(orientation = 'h', hoverinfo = "none", colors = c('#d8b365', '#5ab4ac')) %>%
    layout(bargap = 0.1, barmode = 'overlay',
           xaxis = list(tickmode = 'array', tickvals = c(-5, -2.5, 0, 2.5, 5),
                        ticktext = c('5%', '2.5%', '0', '2.5%', '5%'),
                        title = ""),
           yaxis = list(title = ""),
           title = paste0("Population pyramid, mid-2016")) %>% 
    config(displayModeBar = F)
})

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
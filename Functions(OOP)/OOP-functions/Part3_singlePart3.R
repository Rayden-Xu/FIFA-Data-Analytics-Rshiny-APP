library(tidyverse)
library(plotly)

df_part3_5 <- read_csv('www/data/part3_part3.csv')


single_part3 <- function(y1, top, age){
  if(top == 'ALL'){
    top = 18207
  }
  world_map <- map_data("world")
  y2 <- gsub(' ', '', y1) %>% as.name()
  data <- arrange(df_part3_5, desc(get(y2)))[1:top,] %>% filter(Age %in% age[1]:age[2])
  data$Name <- factor(data$Name, levels = data$Name, ordered = T)
  
  numofplayers <- world_map %>% 
    mutate(region = as.character(region)) %>% 
    left_join((data %>% mutate(Nationality = as.character(Nationality),
                             Nationality = if_else(Nationality %in% "England", 
                                                   "UK", Nationality)) %>%
                 #filter(League == "Bundesliga") %>%
                 count(Nationality, name = "Number of Player") %>%
                 rename(region = Nationality) %>%
                 mutate(region = as.character(region))), by = "region")
  
  if(dim(data)[1] != 0){
    p1 <- ggplot(numofplayers, aes(long, lat, group = group, label = region))+
      geom_polygon(aes(fill = `Number of Player` ), color = "white", show.legend = TRUE)+
      scale_fill_viridis_c(option = "C")+
      theme_void()+
      labs(fill = "Number of Player",
           title = "Number of Player based on Nationality")
  }else{
    p1 <- ggplot() +
      theme(panel.background = element_rect(fill = 'transparent'))
  }
  return(ggplotly(p1, tooltip = c("label","fill")) %>% layout(height = 600, width = 900))
}

library(tidyverse)
library(plotly)
df_part1_3 <- read_csv('www/data/part1_part1.csv')


single_part1 <- function(y1, top, age){
  y2 <- gsub(' ', '', y1) %>% as.name()
  data <- arrange(df_part1_3, desc(get(y2)))[1:top,] %>% filter(Age %in% age[1]:age[2])
  data$Name <- factor(data$Name, levels = data$Name, ordered = T)
  
  if(dim(data)[1] != 0){
    p1 <- ggplot(data = data, 
                 mapping = aes(x = Name, y = get(y2))) +
      geom_point(size = 1, color = '#F8BD47', shape = 1) +
      geom_line(color = '#81C461', group = 1) +
      ylab(y1) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }else{
    p1 <- ggplot() +
      theme(panel.background = element_rect(fill = 'transparent'))
  }
  return(ggplotly(p1) %>% layout(height = 600, width = 900))
}

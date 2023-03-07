library(tidyverse)
library(plotly)

df_part1_2 <- read_csv('www/data/part1_part1.csv')

binary1 <- function(x1, y1, top, age, principal){
  if(y1 == 'International Reputation'){
    x2 <- x1 %>% as.name()
    y2 <- gsub(' ', '', y1) %>% as.name()
    prin <- gsub(' ', '', principal) %>% as.name()
    data <- arrange(df_part1_2, desc(get(prin)))[1:top,] %>% filter(Age %in% age[1]:age[2])
    data$Name <- factor(data$Name, levels = data$Name, ordered = T)
    
    if(dim(data)[1] != 0){
      p1 <- ggplot(data = data, mapping = aes(x = get(x2))) +
        geom_bar(mapping = aes(fill = as.character(get(y2)))) +
        xlab(x1) +
        ylab(y1) +
        scale_fill_discrete(name = y1) +
        theme_bw()
    }else{
      p1 <- ggplot() +
        theme(panel.background = element_rect(fill = 'transparent'))
    }
    
    return(ggplotly(p1) %>% layout(height = 600, width = 900))
  }else{
    x2 <- x1 %>% as.name()
    y2 <- y1 %>% as.name()
    prin <- principal %>% as.name()
    data <- arrange(df_part1_2, desc(get(prin)))[1:top,] %>% filter(Age %in% age[1]:age[2])
    data$Name <- factor(data$Name, levels = data$Name, ordered = T)
    
    if(dim(data)[1] != 0){
      p1 <- ggplot(data = data, mapping = aes(x = get(x2), y = get(y2))) + 
        geom_boxplot(mapping = aes(fill =as.character(get(x2)))) + 
        geom_smooth(se = FALSE) +
        xlab(x1) +
        ylab(y1) +
        scale_fill_discrete(name = x1) +
        theme_bw()
    }else{
      p1 <- ggplot() +
        theme(panel.background = element_rect(fill = 'transparent'))
    }
      
    return(ggplotly(p1) %>% layout(height = 600, width = 900))
  }
}



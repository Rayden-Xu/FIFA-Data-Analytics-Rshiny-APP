library(tidyverse)
library(plotly)

df_part1_1 <- read_csv('www/data/part1_part1.csv')


single_part1 <- function(x1, y1, top, age){
  if(x1 == 'Choose' & y1 == 'Choose'){
    return()
  }else if(x1 != 'Choose' & y1 == 'Choose'){
    x2 <- x1 %>% as.name()
    data <- arrange(df_part1_1, desc(get(x2)))[1:top,] %>% filter(Age %in% age[1]:age[2])
    data$Name <- factor(data$Name, levels = data$Name)
    p1 <- ggplot(data = data, 
                 mapping = aes(x = Name, y = get(x2))) +
      geom_point(size = 1, color = '#F8BD47', shape = 1) +
      geom_line(color = '#81C461', group = 1) +
      ylab(x1) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }else if(x1 == 'Choose' & y1 != 'Choose'){
    y2 <- gsub(' ', '', y1) %>% as.name()
    data <- arrange(df_part1_1, desc(get(y2)))[1:top,] %>% filter(Age %in% age[1]:age[2])
    data$Name <- factor(data$Name, levels = data$Name)
    p1 <- ggplot(data = data, 
                 mapping = aes(x = Name, y = get(y2))) +
      geom_point(size = 1, color = '#F8BD47', shape = 1) +
      geom_line(color = '#81C461', group = 1) +
      ylab(y1) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }else if(x1 != '' & y1 != ''){
    if(y1 != 'International Reputation'){
      x2 <- x1 %>% as.name()
      y2 <- y1 %>% as.name()
      data <- arrange(df_part1_1, desc(get(y2)))[1:top,] %>% filter(Age %in% age[1]:age[2])
      data$Name <- factor(data$Name, levels = data$Name, order = T)
      
      data7 <- arrange(df_part1_1, desc(Values))[1:100,]
      data7$Name <- factor(data7$Name, levels = c(data7$Name), ordered = T)
      
      p1 <- ggplot(data = data7, mapping = aes(x = Overall, y = Values)) + 
        geom_boxplot(mapping = aes(fill =as.character(Overall))) + 
        geom_smooth(se = FALSE)
    }
  }
  x2 <- x1 %>% as.name()
  y2 <- gsub(' ', '', y1) %>% as.name()
  data <- arrange(df_part1_1, desc(get(y2)))[1:top,] %>% filter(Age %in% age[1]:age[2])
  data$Name <- factor(data$Name, levels = data$Name)
  p1 <- ggplot(data = data, 
         mapping = aes(x = Name, y = get(y2))) +
    geom_point(size = 1, color = '#F8BD47', shape = 1) +
    geom_line(color = '#81C461', group = 1) +
    ylab(y1) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  return(ggplotly(p1) %>% layout(height = 600, width = 900))
}




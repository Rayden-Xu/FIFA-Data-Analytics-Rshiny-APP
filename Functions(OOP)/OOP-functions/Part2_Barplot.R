library(ggplot2)
library(tidyverse)
df_part3_1 <- read_csv('www/data/part2_part2.csv')

Barchart <- function(Namex, Namey) {
  if (!is.null(Namex) & is.null(Namey)) {
    if (Namex != 'Choose') {
      df_x <- df_part3_1[,c(1,5:33)] %>% filter(Name == Namex)
      df_new <- df_x %>% gather(key = "type", value = "points", -Name)
      ggplot(df_new) + 
        geom_bar(mapping = aes(x = type, y = points), stat="identity") +
        coord_flip() +
        geom_hline(yintercept = 0.5, color = "Yellow")
    }
  }
  else if (!is.null(Namey) & is.null(Namex)){
    if (Namey != 'Choose') {
      df_y <- df_part3_1[,c(1,5:33)] %>% filter(Name == Namey)
      df_new <- df_y %>% gather(key = "type", value = "points", -Name)
      ggplot(df_new) + 
        geom_bar(mapping = aes(x = type, y = points), stat="identity") +
        coord_flip() +
        geom_hline(yintercept = 0.5, color = "Yellow")
    }
    }
  else if (!is.null(Namey) & !is.null(Namex)){
    if (Namex != 'Choose' & Namey != 'Choose') {
      df_x <- df_part3_1[,c(1,5:33)] %>% filter(Name == Namex)
      df_y <- df_part3_1[,c(1,5:33)] %>% filter(Name == Namey)
      df_xy <- rbind(df_x, df_y)
      df_new <- df_xy %>% gather(key = "type", value = "points", -Name)
      ggplot(df_new) + 
        geom_bar(mapping = aes(x = type, y = points, fill = Name), position = 'fill', stat="identity") +
        coord_flip() +
        geom_hline(yintercept = 0.5, color = "Yellow")
    }
  }
}

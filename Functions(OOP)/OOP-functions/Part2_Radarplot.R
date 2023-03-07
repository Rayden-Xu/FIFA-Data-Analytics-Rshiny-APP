library(plotly)
library(tidyverse)
library(fmsb)

df_part2_5 <- read_csv('www/data/part2_part2.csv')
Raderplot <- function(Namex,Namey) {
  if (!is.null(Namex) & is.null(Namey)) {
    if (Namex != 'Choose') {
    df_x <- df_part2_5 %>% filter(Name == Namex) %>% select(Defending,General,Mental,Passing,Mobility,Power)
    plot_ly(
      type = 'scatterpolar',
      r = as.numeric(df_x[1,]),
      theta = c('Defending','General','Mental','Passing','Mobility','Power'),
      fill = 'toself'
    ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,100)
          )
        ),
        showlegend = F
      )
    }
  }
  else if (!is.null(Namey) & is.null(Namex)){
    if (Namey != 'Choose') {
    df_y <- df_part2_5 %>% filter(Name == Namey) %>% select(Defending,General,Mental,Passing,Mobility,Power)
    plot_ly(
      type = 'scatterpolar',
      r = as.numeric(df_y[1,]),
      theta = c('Defending','General','Mental','Passing','Mobility','Power'),
      fill = 'toself'
    ) %>% 
      layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0,100)
          )
        ),
        showlegend = F
      )
    }
  }
  else if (!is.null(Namey) & !is.null(Namex)){
    if (Namex != 'Choose' & Namey != 'Choose') {
    df_x <- df_part2_5 %>% filter(Name == Namex) %>% select(Defending,General,Mental,Passing,Mobility,Power)
    df_y <- df_part2_5 %>% filter(Name == Namey) %>% select(Defending,General,Mental,Passing,Mobility,Power)
    plot_ly(
      type = 'scatterpolar',
      fill = 'toself'
    ) %>%
      add_trace(
        r = as.numeric(df_x[1,]),
        theta = c('Defending','General','Mental','Passing','Mobility','Power'),
        name = Namex
      ) %>%
      add_trace(
        r = as.numeric(df_y[1,]),
        theta = c('Defending','General','Mental','Passing','Mobility','Power'),
        name = Namey
        
      ) %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0,100)
          )
        )
      )
    
  }
  }
}


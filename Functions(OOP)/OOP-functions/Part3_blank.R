library(tidyverse)
library(plotly)

blank <- function(){
  p <- ggplot() +
    theme(panel.background = element_rect(fill = 'transparent'))
  return(ggplotly(p))
}


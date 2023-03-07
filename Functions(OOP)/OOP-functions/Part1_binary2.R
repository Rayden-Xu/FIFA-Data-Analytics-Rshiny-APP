library(tidyverse)
library(plotly)

binary2 <- function(x1, y1){
  p <- ggplot() +
    theme(panel.background = element_rect(fill = 'transparent'))
  return(ggplotly(p) %>% layout(height = 600, width = 900))
}


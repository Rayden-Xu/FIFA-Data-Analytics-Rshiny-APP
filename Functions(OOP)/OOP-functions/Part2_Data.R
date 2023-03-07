library(tidyverse)

df_part2_1 <- read_csv('www/data/part2_part3.csv')


club_name <- df_part2_1[,c(2,4)]

df_part2_2 <- read_csv('www/data/part2_part1.csv')

############################## Binary Variables ##############################
textfunc1 <- function(x1, y1, top, age, principal){
  x2 <- x1 %>% as.name()
  y2 <- gsub(' ', '', y1) %>% as.name()
  prin <- gsub(' ', '', principal) %>% as.name()
  # prin1 <- gsub(' ', '', principal)
  data <- arrange(df_part1_2, desc(get(prin)))[1:top,] %>% filter(Age %in% age[1]:age[2])
  
  if(x2 == prin){
    select <- gsub(' ', '', y1)
  }else{
    select <- x1
  }
  
  name <- data[which(data[[select]] == max(data[[select]])), 'Name']
  # return(name)
  
  if(dim(data)[1] != 0){
    decision <- name$Name[1]
    if(dim(name)[1] > 1){
      for(i in 1:(dim(name)[1]-1)){
        decision <- paste(decision, 'and', name$Name[i+1])
      }
    }
    paste('NOW, BASED ON YOUR SETTING, THE OPTIMAL CHOICE FOR YOU IS', '\n',
          decision, '.', sep = '')
  }else{
    paste("SORRY, THERE ISN'T AN OPTIMAL CHOICE FOR YOU.")
  }
}



############################## Single Variable ##############################
textfunc2 <- function(y1, top, age){
  y2 <- gsub(' ', '', y1) %>% as.name()
  y3 <- gsub(' ', '', y1)
  data <- arrange(df_part1_2, desc(get(y2)))[1:top,] %>% filter(Age %in% age[1]:age[2])
  name <- data[which(data[[y3]] == max(data[[y3]])), 'Name']
  # return(name)
  
  if(dim(data)[1] != 0){
    decision <- name$Name[1]
    if(dim(name)[1] > 1){
      for(i in 1:(dim(name)[1]-1)){
        decision <- paste(decision, 'and', name$Name[i+1])
      }
    }
    paste('NOW, BASED ON YOUR SETTING, THE OPTIMAL CHOICE FOR YOU IS', '\n',
          decision, '.', sep = '')
  }else{
    paste("SORRY, THERE ISN'T AN OPTIMAL CHOICE FOR YOU.")
  }
}



################################## Default ##################################
textfunc3 <- function(){
  paste('CHOOSE A VARIABLE PLEASE.')
}

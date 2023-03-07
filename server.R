function(input, output, session) {
  #Part 1
  values <- reactiveValues(prin = NULL)

  observeEvent(c(input$variablex, input$variabley), {
    if(input$variablex != 'Choose' & input$variabley != 'Choose'){
      output$obs <- renderUI({
        radioGroupButtons(
          'Principle',
          label = 'Choose Principal Variable',
          choices = c('X', 'Y'),
          status = 'primary',
          checkIcon = list(
            yes = icon('ok',
                       lib = 'glyphicon'),
            no = icon('remove',
                      lib = 'glyphicon'))
        )
      })
    }else if(input$variablex != 'Choose' & input$variabley == 'Choose'){
      output$obs <- renderUI({NULL})

      output$text <- renderText({
        textfunc2(input$variablex, input$top1, input$age1)
      })

      output$distPlot <- renderPlotly({
        single_part1(input$variablex, input$top1, input$age1)
      })
    }else if(input$variablex == 'Choose' & input$variabley != 'Choose'){
      output$obs <- renderUI({NULL})

      output$text <- renderText({
        textfunc2(input$variabley, input$top1, input$age1)
      })

      output$distPlot <- renderPlotly({
        single_part1(input$variabley, input$top1, input$age1)
      })
    }else if(input$variablex == 'Choose' & input$variabley == 'Choose'){
      output$obs <- renderUI({NULL})

      output$text <- renderText({
        textfunc3()
      })

      output$distPlot <- renderPlotly({
        binary2(input$variablex, input$variabley)
      })
    }
  })

  observeEvent(input$Principle,{

    if (input$variablex != 'Choose'){
      if(input$variabley != 'Choose'){
        if(input$Principle == 'X'){
          values$prin = input$variablex
        }else{
          values$prin = input$variabley
        }
      }
    }

    output$text <- renderText({
      textfunc1(input$variablex, input$variabley, input$top1, input$age1, values$prin)
    })

    output$distPlot <- renderPlotly({
      binary1(input$variablex, input$variabley, input$top1, input$age1, values$prin)
    })
  })


  observeEvent(input$reset1, {
    values$prin <- NULL
    output$obs <- NULL
    output$text <- NULL
    output$distPlot <- NULL
    updateSelectInput(session, 'variablex',
                      label = 'Personal Evaluation',
                      choices = c(
                        'Choose',
                        'Overall',
                        'Potential',
                        'Height'
                      ),
                      selected = 'Choose')
    updateSelectInput(session, 'variabley',
                      label = 'General Evaluation',
                      choices = c(
                        'Choose',
                        'Value',
                        'Wage',
                        'International Reputation'
                      ),
                      selected = 'Choose')
    updateRadioGroupButtons(session, 'top1',
                            label = 'Top',
                            choices = c(10, 20, 30, 50),
                            selected = 20,
                            # individual = TRUE,
                            checkIcon = list(
                              yes = icon('check-circle', style = 'color: steelblue'),
                              no = icon('circle-o', style = 'color: #FB3008')
                            ))
    updateSliderInput(session, 'age1',
                      label = 'Age',
                      min = 16,
                      max = 45,
                      value = c(20, 30))
  })
  #Part 2
  values <- reactiveValues(dflist1 = NULL,
                           team_x = NULL,
                           team_y = NULL,
                           playerx = NULL,
                           playery = NULL,
                           Age = NULL,
                           Overall = NULL,
                           Value = NULL,
                           dflist2 = NULL
  )
  output$player1 <- renderText({
    'PLAYER 1'
  })
  output$player2 <- renderText({
    'PLAYER 2'
  })
  observeEvent(input$teamx, {
    values$team_x <- input$teamx
    if(values$team_x != 'Choose') {
      df_club <- club_name %>% filter(Club == values$team_x)
      values$dflist1 <- pull(df_club, Name)
      output$obs1 <- renderUI({
        selectInput(
          inputId = "memberx",
          label = "Select A Member",
          choices = values$dflist1
        )
      })
    }
    else{
      output$obs1 <- NULL
    }
  })
  observeEvent(input$teamy, {
    values$team_y <- input$teamy
    if(values$team_y != 'Choose') {
      df_club <- club_name %>% filter(Club == values$team_y)
      values$dflist2 <- pull(df_club, Name)
      output$obs2 <- renderUI({
        selectInput(
          inputId = "membery",
          label = "Select A Member",
          choices = values$dflist2
        )
      })
    }
    else{output$obs2 <- NULL}
  })
  observeEvent(input$memberx,{
    values$playerx <- input$memberx
    output$Age_x <- renderText({
      "Age: "
    })
    output$Agex <- renderText({
      df_age <- df_part2_2 %>% filter(Name == input$memberx)
      pull(df_age, Age)
    })
    output$Overall_x <- renderText({
      "Overall: "
    })
    output$Overallx <- renderText({
      df_overall <- df_part2_2 %>% filter(Name == input$memberx)
      pull(df_overall, Overall)
    })
    output$Value_x <- renderText({
      "Value(€): "
    })
    output$Valuex <- renderText({
      df_value <- df_part2_2 %>% filter(Name == input$memberx)
      paste('€', pull(df_value, Value), 'M', sep = '')
    })
  })
  observeEvent(input$membery,{
    values$playery <- input$membery
    output$Age_y <- renderText({
      "Age: "
    })
    output$Agey <- renderText({
      df_age <- df_part2_2 %>% filter(Name == input$membery)
      pull(df_age, Age)
    })
    output$Overall_y <- renderText({
      "Overall: "
    })
    output$Overally <- renderText({
      df_overall <- df_part2_2 %>% filter(Name == input$membery)
      pull(df_overall, Overall)
    })
    output$Value_y <- renderText({
      "Value(€): "
    })
    output$Valuey <- renderText({
      df_value <- df_part2_2 %>% filter(Name == input$membery)
      paste('€', pull(df_value, Value), 'M', sep = '')
    })
  })
  observeEvent(input$Graphtype1, {
    if (input$Graphtype1 == 'Radar') {
      output$Plotchart <- renderPlotly({
        Raderplot(values$playerx, values$playery)
      })
    }
    else if(input$Graphtype1 =='Bar') {
      output$Plotchart <- renderPlotly({
        Barchart(values$playerx, values$playery)
        
      })
    }
    # else if(input$Graphtype == 'Line') {
    #   output$plotChart <- 
    # }
  })
  observeEvent(input$resetx, {
    values$playerx <- NULL
    output$obs1 <- NULL
    output$Age_x <- NULL
    output$Agex <- NULL
    output$Overall_x <- NULL
    output$Overallx <- NULL
    output$Value_x <- NULL
    output$Valuex <- NULL
    updateSelectInput(session, "teamx",
                      label = "Select A Club",
                      choices = c('Choose',
                                  'Real Madrid',
                                  'FC Barcelona', 
                                  'Manchester United', 
                                  'Paris Saint-Germain',
                                  'Juventus',
                                  'FC Bayern München',
                                  'Arsenal','Liverpool',
                                  'Milan','Atlético Madrid'),
                      selected = 'Choose'
    )
    
  })
  observeEvent(input$resety, {
    values$playery <- NULL
    output$obs2 <- NULL
    output$Age_y <- NULL
    output$Agey <- NULL
    output$Overall_y <- NULL
    output$Overally <- NULL
    output$Value_y <- NULL
    output$Valuey <- NULL
    updateSelectInput(session, "teamy",
                      label = "Select A Club",
                      choices = c('Choose',
                                  'Real Madrid',
                                  'FC Barcelona', 
                                  'Manchester United', 
                                  'Paris Saint-Germain',
                                  'Juventus',
                                  'FC Bayern München',
                                  'Arsenal','Liverpool',
                                  'Milan','Atlético Madrid'),
                      selected = 'Choose'
    )
  })

  
  
    #Part3
  values <- reactiveValues(
    var = NULL,
    gra = NULL,
    #
    # = NULL,
    # age = NULL
  )

  observeEvent(c(input$variable, input$Graphtype2), {
    var = input$variable
    gra = input$Graphtype2
    # top = input$top
    # age = input$age
    if(var != 'Choose' & gra == 'Nationality'){

      output$distPlot1 <- renderPlotly({
        single_part3(var, input$top2, input$age2)
      })

    }else if(var != 'Choose' & gra == 'Club'){

      output$distPlot1 <- renderPlotly({
        single2(var, input$top2, input$age2)
      })

    }else if(var == 'Choose'){
      output$distPlot1 <- renderPlotly({
        blank()
      })
    }
  })



  observeEvent(input$reset2, {
    values$var <- NULL
    values$gra <- NULL
    # values$top <- NULL
    # values$age <- NULL
    output$distPlot1 <- NULL
    updateSelectInput(session,'variable',
                      label = 'Setting Your Choice',
                      choices = c(
                        'Choose',
                        'Overall',
                        'Potential',
                        'Height',
                        'Value',
                        'Wage',
                        'International Reputation'
                      ),
                      selected = 'Choose')
    updateRadioGroupButtons(session, 'top2',
                            label = 'Top',
                            choices = c(100, 1000, 5000, 'ALL'),
                            selected = 'ALL',
                            # individual = TRUE,
                            checkIcon = list(
                              yes = icon('check-circle', style = 'color: steelblue'),
                              no = icon('circle-o', style = 'color: #FB3008')
                            ))
    updateSliderInput(session, 'age2',
                      label = 'Age',
                      min = 16,
                      max = 45,
                      value = c(20, 30))
    updateRadioGroupButtons(session,
                            inputId = "Graphtype2",
                            label = "Distribution Map",
                            choices = c("Nationality","Club"),
                            selected = 'Nationality',
                            status = 'primary',
                            # justified = TRUE,
                            # width = '100%'
    )
  })
}
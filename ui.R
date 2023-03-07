library(markdown)

navbarPage("FIFA for Future",
           tabPanel("Evaluation",
                    fluidPage(
                      titlePanel(h3('Intelligent Decision System')),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            'variablex',
                            label = 'Personal Evaluation',
                            choices = c(
                              'Choose',
                              'Overall',
                              'Potential',
                              'Height'
                            ),
                            selected = 'Choose'
                          ),

                          selectInput(
                            'variabley',
                            label = 'General Evaluation',
                            choices = c(
                              'Choose',
                              'Value',
                              'Wage',
                              'International Reputation'
                            ),
                            selected = 'Choose'
                          ),


                          radioGroupButtons(
                            'top1',
                            label = 'Top',
                            choices = c(10, 20, 30, 50),
                            selected = 20,
                            individual = TRUE,
                            checkIcon = list(
                              yes = icon('check-circle', style = 'color: steelblue'),
                              no = icon('circle-o', style = 'color: #FB3008')
                            )
                          ),

                          sliderInput(
                            'age1',
                            label = 'Age',
                            min = 16,
                            max = 45,
                            value = c(20, 30)
                          ),

                          uiOutput('obs'),

                          actionButton(
                            'reset1',
                            label = 'Reset Data',
                            icon = icon('refresh'),
                            width = "100%"
                          )
                        ),

                        mainPanel(fluidPage(
                          fluidRow(
                            verbatimTextOutput('text'),
                            plotlyOutput('distPlot')
                          )
                        ))
                      )
                    )
           ),
           tabPanel("Comparison",
                    fluidPage(
                      tags$head(
                        tags$style(HTML("
      pre {
        color: white;
        background-color: #e1849a;
      }
      .myclass pre {
        color: white;
        background-color: #79acd2;
        }
      }"))
                      ),
                      titlePanel(h3('Player Comparison 
                                 in Europa League')),
                      fluidRow(
                        column(3,
                               fluidRow(
                                 column(6,
                                        div(class = "myclass",
                                            verbatimTextOutput(
                                              outputId = 'player1',
                                            )
                                        ))
                               ),
                               selectInput(width = "100%",
                                           inputId = "teamx",
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
                               ),
                               uiOutput("obs1"),
                               actionButton(
                                 inputId = "resetx",
                                 label = "Reset Data",
                                 icon = icon("refresh"),

                                 width = "100%"
                               )
                        ),
                        column(6, fluidRow(
                          column(4,
                                 textOutput("Age_x"),
                                 div(class = "myclass",
                                     verbatimTextOutput("Agex")
                                 )
                          ),
                          column(4,
                                 textOutput("Overall_x"),
                                 div(class = "myclass",
                                     verbatimTextOutput("Overallx")
                                 )
                          ),
                          column(4,
                                 textOutput("Value_x"),
                                 div(class = "myclass",
                                     verbatimTextOutput("Valuex")
                                 )
                          )
                        ),
                        fluidRow(
                          column(4,
                                 textOutput("Age_y"),
                                 verbatimTextOutput("Agey")
                          ),
                          column(4,
                                 textOutput("Overall_y"),
                                 verbatimTextOutput("Overally")
                          ),
                          column(4,
                                 textOutput("Value_y"),
                                 verbatimTextOutput("Valuey")
                          )
                        )
                        ),
                        column(3,
                               fluidRow(
                                 column(6,
                                        verbatimTextOutput(
                                          outputId = 'player2',
                                        ))
                               ),
                               selectInput("teamy",
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

                               ),

                               uiOutput("obs2"),
                               actionButton(
                                 inputId = "resety",
                                 label = "Reset Data",
                                 icon = icon("refresh"),
                                 width = "100%"
                               )

                        )
                      ),
                      hr(),
                      fluidRow(
                        sidebarLayout(
                          fluidRow(
                            sidebarPanel(
                              width = 4,
                              radioGroupButtons(
                                inputId = "Graphtype1",
                                label = "Choose a graph :",
                                choices = c("Radar","Bar"),
                                justified = TRUE,
                                width = '100%',
                                status = 'primary'
                              )
                            )
                          ),
                          fluidRow(
                            column(
                              width = 10,
                              offset = 2,
                              mainPanel(
                                width = '100%',
                                plotlyOutput('Plotchart', width = '80%')
                              )
                            )
                          )
                        )
                      )
                    )
           ),
           tabPanel("Distribution",
                      fluidPage(
                        titlePanel(h3('Cosmopolitan Distribution')),
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(
                              'variable',
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
                              selected = 'Choose'
                            ),

                            radioGroupButtons(
                              'top2',
                              label = 'Top',
                              choices = c(100, 1000, 5000, 'ALL'),
                              selected = 'ALL',
                              individual = TRUE,
                              checkIcon = list(
                                yes = icon('check-circle', style = 'color: steelblue'),
                                no = icon('circle-o', style = 'color: #FB3008')
                              )
                            ),

                            sliderInput(
                              'age2',
                              label = 'Age',
                              min = 16,
                              max = 45,
                              value = c(20, 30)
                            ),

                            actionButton(
                              'reset2',
                              label = 'Reset Data',
                              icon = icon('refresh'),
                              width = "100%"
                            )
                          ),

                          mainPanel(fluidPage(
                            fluidRow(
                              radioGroupButtons(
                                inputId = "Graphtype2",
                                label = "Distribution Map",
                                choices = c("Nationality","Club"),
                                selected = 'Nationality',
                                justified = TRUE,
                                width = '100%',
                                status = 'primary'
                              )
                            ),

                            fluidRow(
                              # verbatimTextOutput('text'),
                              plotlyOutput('distPlot1')
                            )
                        )
                          )
                        )
                      )
           )
)
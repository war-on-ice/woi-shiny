
## hextally-player

shinyUI(
    fluidPage(
        tags$head(tags$style(".container-fluid { font-size: 12px;}"),
                  tags$style(type="text/css",
                             ".shiny-output-error { visibility: hidden; }",
                             ".shiny-output-error:before { visibility: hidden; }")),

        h2 ("Player Hextally"),
        wellPanel(
            fluidRow(
                column(3, #textInput("nameSearch1", "Search For Team", ""),
                       htmlOutput("onload.nameSearch1"),
                       htmlOutput("nameSelect1")),  # manstrength
                column(3, htmlOutput ("onload.startseason"),
                       htmlOutput ("onload.endseason"),
                       checkboxInput(inputId="include.playoffs", label="Include Playoffs", value = FALSE)), #Dates 
                column(3, htmlOutput ("onload.manchoice"),
                       checkboxInput ("superbins", "Use Only Three Blocks", TRUE),
                       checkboxInput ("numbersonplot", "Numbers on Plot", TRUE),
                       checkboxInput ("stabilize", "Stabilize Rate Estimates", TRUE),
                       checkboxInput ("zscoresonplot", "Color By Statistical Significance", TRUE)), # home/away
                column(3, selectInput ("homeawayall", "Select Home/Away Shots",
                                       c("Home", "Away", "All"), "All"),
                       selectInput ("shotclass", "Choose Shot Type", shottype.options, "All Shots"))  # teamsearch
                ),
            fluidRow(
                column(3, conditionalPanel (condition = "2 == 3",
                                            checkboxInput ("useimputedlocations", "Use Imputed Shot Locations", TRUE))), # 
                column(3), # htmlOutput ("onload.6")
                column(3),  
                column(3)  # 
                )
            ),
            
#        helpText("Results in each zone, per 60 minutes at even strength")

        
        h4 ("Shot Rate Differential"),

        div(#title=alt.text()[1],
            plotOutput("ratediff", height="480px", width="920px")),
        helpText("Numbers indicate shot differential in each zone, per 60 minutes at even strength, with respect to league average rates (so that 0 is equality. Higher zones coloured in red, lower in blue, roughly equal in green"),
        h5 ("Relative Goals/60 Differences By Zone"),
        tableOutput("netresult"),

        
        h4 ("Team Shot Rate For (Relative)"),
        div(#title=alt.text()[2],
            plotOutput("ratefor", height="480px", width="920px")),
        helpText("Numbers indicate team shot rates relative to league average (so that 1 is equality). Higher zones coloured in red, lower in blue, roughly equal in green"),
        tableOutput("rateminutesO"),

        h4 ("Team Shot Rate Against (Relative)"),
        div(#title=alt.text()[3],
            plotOutput("rateagainst", height="480px", width="920px")),
        helpText("Numbers indicate team shot rates relative to league average (so that 1 is equality). Higher zones coloured in red, lower in blue, roughly equal in green"),
        tableOutput("rateminutesD"),

        htmlOutput("successlabel"),  #h4 ("Player Shooting % For Goal"),
        div(#title=alt.text()[4],
            plotOutput("shootingsuccess", height="480px", width="920px")),

        
#        h4 ("Shooting % For Goal (Absolute)"),
#        div(title="Left: Shooting Percentage For Team, Absolute\nRight: Shooting Percentage Against Team",
#            plotOutput("absolutesuccessplots", height="480px", width="920px")),
#        tableOutput("rateminutes"),
        conditionalPanel (condition = "2 == 3",
                          sliderInput("borrowedstrength",
                                      label="Added Mean Minutes", min=0, value=300, max=1000, step=1)),

        hr(),
        textOutput ("shareDestination"),
        wellPanel(
            
            fluidRow(
                column(4,         
                       div(title="Zone Names",
                           plotOutput(outputId = "guidepostnames", height="250px", width="275px"))),
                column(4,         
                       div(title="League-Wide Success Probability",
                           plotOutput(outputId = "shootingcount", height="250px", width="275px"))),
                column(4,         
                       div(title="League-Wide Shot and Goal Counts",
                           plotOutput(outputId = "guidepostgrand", height="250px", width="275px")))
                )
            )
        ##, verbatimTextOutput("Errorchecker")   

        
        )

                   
#                   conditionalPanel (condition = "input.righttabs == 'Player Shooting % For Goal'",
#                                     selectInput ("shotclass", "Choose Shot Type", shottype.options, "All Shots")),
                   

#                   conditionalPanel (condition = "2 == 3",
#                                     checkboxInput ("useimputedlocations", "Use Imputed Shot Locations", TRUE))   #
                   
                 #,div(title="Shot and Goal Counts By Team Under Analysis", plotOutput(outputId = "guidepost", height="250px", width="275px"))
#                 ,conditionalPanel (condition = "input.righttabs == 'Shot Rate Differential'",
#                                    div(title="Groups", plotOutput(outputId = "guidepostnames", height="250px", width="275px")))
#                 ,verticalLayout(div(
#                     div(style="display:inline-block", actionButton("sharePage", "Share This Page")),
#                     div(style="display:inline-block", htmlOutput ("shareDestination"))
#                     ))
                 #,verbatimTextOutput("Errorchecker")   
#                 ),
    

  )





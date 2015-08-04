
## hextally-goalies

shinyUI(
    fluidPage(
        tags$head(tags$style(".container-fluid { font-size: 12px;}"),
                  tags$style(type="text/css",
                             ".shiny-output-error { visibility: hidden; }",
                             ".shiny-output-error:before { visibility: hidden; }")),

        h2 ("Goaltender Hextally", align="center"),
        wellPanel(
            fluidRow(
                column(3, #textInput("nameSearch1", "Search For Team", ""),
                       htmlOutput("onload.nameSearch1"),
                       htmlOutput("nameSelect1")),  # manstrength
                column(3, htmlOutput ("onload.startseason"),
                       htmlOutput ("onload.endseason"),
                       checkboxInput(inputId="includeplayoffs", label="Include Playoffs", value = FALSE)), #Dates 
                column(3, selectInput ("mansituation", "Man Situation",
                                       c("Even Strength", "Power Play", "Four On Four"),
                                       "Even Strength"),
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

        htmlOutput("successlabel"),  #h4 ("Player Shooting % For Goal"),
        div(#title=alt.text()[4],
            plotOutput("shootingsuccess", height="480px", width="920px")),
        
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
        )

  )






source("app.R")

shinyUI(
    fluidPage(
        tags$head(tags$style(".container-fluid { font-size: 12px;}"),
                  tags$style(type="text/css",
                             ".shiny-output-error { visibility: hidden; }",
                             ".shiny-output-error:before { visibility: hidden; }")),

        h2 ("Team Hextally", align="center"),
        wellPanel(
            fluidRow(
                column(3, #textInput("nameSearch1", "Search For Team", ""),
                       htmlOutput("nameSelect1"),
                       checkboxInput ("includeblockmiss", label="Include Missed/Blocked Shots", value=TRUE),
                       conditionalPanel (condition = "2 == 3",
                                         checkboxInput ("useimputedlocations", "Use Imputed Shot Locations", TRUE)
                       )),  # manstrength
                column(3, htmlOutput ("onload.startseason"),
                       htmlOutput ("onload.endseason"),
                       checkboxInput(inputId="includeplayoffs", label="Include Playoffs", value = FALSE)), #Dates 
                column(3, selectInput ("mansituation", "Man Situation",
                                       c("Even Strength", "Power Play", "Four On Four"),
                                       "Even Strength"),
                       checkboxInput ("superbins", "Use Only Three Blocks", TRUE),
                       checkboxInput ("numbersonplot", "Numbers on Plot", TRUE),
                       checkboxInput ("zscoresonplot", "Color By Statistical Significance", TRUE)), # home/away
                column(3, selectInput ("homeawayall", "Select Home/Away Shots",
                                       c("Home", "Away", "All"), "All"),
                       selectInput ("shotclass", "Choose Shot Type", shottype.options, "All Shots"))  # teamsearch
                )
            ),
            
#        helpText("Results in each zone, per 60 minutes at even strength")

        h4 ("Shooting Rate (Relative)"),
        div(title="Left: Shooting Rates For Team, Compared To League Average\nRight: Shooting Rates Against Team",
            plotOutput("rateplots", height="480px", width="920px")),

        h4 ("Shooting % For Goal (Relative)"),
        div(title="Left: Shooting Percentage For Team, Compared To League Average\nRight: Shooting Percentage Against Team",
            plotOutput("relativesuccessplots", height="480px", width="920px")),

        h4 ("Shooting % For Goal (Absolute)"),
        div(title="Left: Shooting Percentage For Team, Absolute\nRight: Shooting Percentage Against Team",
            plotOutput("absolutesuccessplots", height="480px", width="920px")),
        tableOutput("rateminutes"),

        hr(),
        textOutput ("shareDestination"),
        wellPanel(
            
            fluidRow(
                column(4,         
                       div(title="Shot and Goal Counts By Team Under Analysis",
                           plotOutput(outputId = "guidepost", height="250px", width="275px"))),
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
    
    )

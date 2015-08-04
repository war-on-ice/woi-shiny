## playertable ui
## acthomas, 8-20-14

source("global.R")

shinyUI(
    fluidPage(        
        tags$head(tags$style(".container-fluid { font-size: 12px;}")),
        tags$head(tags$style(".control-label { font-size: 12px;}")),
        tags$head(tags$style(".selectize-input { font-size: 12px;}")),   #shiny-bound-input
        
        tags$style(type="text/css", ".shiny-output-error { visibility: hidden; }", ".shiny-output-error:before { visibility: hidden; })"),

        tags$head( tags$link(rel = "stylesheet", type = "text/css", href = "css/woi-charts.css") ),
        tags$head(tags$script(src="js/lib/d3.min.js")),
        tags$head(tags$script(src="js/woi-charts.js")),
        
        tags$head(tags$link(rel = "stylesheet", type = "text/css", id ='twentytwelve-fonts-css', href = "http://fonts.googleapis.com/css?family=Open+Sans:400italic,700italic,400,700&#038;subset=latin,latin-ext", media="all")),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "http://war-on-ice.com/css/capcheck.css")),


        h2('Season Series Comparison', align="center"),


            
        wellPanel(fluidRow(
            column(3,
                   selectInput (inputId="season", label="Season",
                                selected = "20142015", choices=seasons),
                   selectInput (inputId="psession", label="Session",
                                selected = "Regular/Playoffs",
                                choices = c("Regular/Playoffs","Regular","Playoffs")),
                   downloadButton("downloadData", "Download Table")
                   ),
            column(3,
                   selectInput (inputId="team1", label="Select Team 1",
                                selected = "TOR", choices = teams),
                   selectInput (inputId="team2", label="Select Team 2",
                                selected = "PIT", choices = teams)

                   ,conditionalPanel(condition = "2 === 3",
                       checkboxInput ("legacynames", "Use Legacy Naming", value=TRUE))
                   ),
            column(3,
                   selectInput(inputId="manchoice", label="Team Strengths",
                               selected = "Even Strength 5v5", choices=split.man.choices),
                   selectInput(inputId="scorechoice", label="Score Situation",
                                  selected = "All", choices=split.score.choices)
                   ),

            column(3,
                   conditionalPanel(condition = "2 === 3", htmlOutput("onload.seasongcode")),
                   selectInput(inputId="whichPosLoc", label="Columns To Display",
                               selected = "Prime", choices=game.screening.choices),

                   htmlOutput("dynamic.period")                   
                   )
            )),
                  
        htmlOutput ("GamesInfo"),
        
        verticalLayout(
            ##h4('Game Record'),

            tabsetPanel(
                id="tabset",
                tabPanel("Event Tables",
                         h4('Team Statistics'),
                         dataTableOutput('teamtable'), 

                         hr(),
                         h4('Goalie Statistics'),
                         dataTableOutput('goalietable'),

                         hr(),
                         h4('Team 1 Player Statistics'),
                         dataTableOutput('awaytable'), 
                         hr(),
                         h4('Team 2 Player Statistics')
                         , dataTableOutput('hometable')#,
                        
                         ),
                tabPanel("Head-To-Head Matchups",

                         checkboxInput ("toi-toggle","Show Time On Ice", FALSE)
                         ,   div(htmlOutput('toiMatchups'))

                         ),
                
                tabPanel("Event Charts",
#                         h4('Running Event Count'),
#                         plotOutput(outputId="gameprog", height="600px", width="900px"),
                         
#                         h4('Running Scoring Chance Count'),
#                         plotOutput(outputId="gameprog.sc", height="600px", width="900px"),
                         
                         
#                         h4('Shift Charts'),
                         ## Shift chart by time.
#                         div (plotOutput(outputId="shiftchart", height="600px", width="900px"),
#                              title = "Bright Green bars: Players On-Ice for Goals Against\nPlayers Ordered By Forward/Defense, Then Decreasing Time On Ice"),


                         h4('Player Corsi Events')
                         ,   #Score/Home Adjusted
                         div(htmlOutput ('newSHAC')),
                         div(htmlOutput ('newShotPlot'))
                         )
                

            )),
        
        
        verticalLayout(div(
            div(style="display:inline-block", actionButton("sharePage", "Share This Page")),
            div(style="display:inline-block", textOutput ("shareDestination"))
            ))
        
        )
    )

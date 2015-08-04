## team server ui
## game-by-game results and plots
## acthomas, 8-31-14

source("global.R")

shinyUI(
    fluidPage(
     ##   verbatimTextOutput("fancy"),
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        tags$head(tags$style(".container-fluid { font-size: 12px;}")),

        tags$head(tags$link(rel = "stylesheet", type = "text/css", id ='twentytwelve-fonts-css', href = "http://fonts.googleapis.com/css?family=Open+Sans:400italic,700italic,400,700&#038;subset=latin,latin-ext", media="all")),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "http://war-on-ice.com/css/capcheck.css")),

        h2('Team Statistics, By Game and Season', align="center"),

        wellPanel(
            fluidRow(
                column(3, htmlOutput ("onload.1")), # manstrength
                column(3, htmlOutput ("onload.2")), # score
                column(3, htmlOutput ("onload.3")), # home/away
                column(3, htmlOutput ("onload.4"))  # teamsearch
                ),

            fluidRow(
                column(3, htmlOutput ("onload.5"), checkboxInput ("legacynames", "Use Legacy Names", value=TRUE)), # page-view
                column(3, dateRangeInput('daterange',
                                         label = 'Date range', 
                                         start = "2002-10-01", end= Sys.Date(),
                                         min = "2002-10-01", max = Sys.Date()+10,
                                         separator = " - ",
                                         format = "yyyy-mm-dd", startview = 'year', language = 'en', weekstart = 1)),  
                column(3,
                       downloadButton("downloadSData", "Download Season Table"),
                       downloadButton("downloadGData", "Download Game Table")
                       ),
                column(3,  htmlOutput("onload.playoffs"))
                
            )
            ),
        
        htmlOutput ("onload.tabset"),
        h6("Note: Because the NHL didn't collect missed and blocked shots before October 2005, Corsi and Fenwick statistics from those years are calculated with shots and goals only."),
     #   htmlOutput ("twittershare"),
        verticalLayout(div(
            div(style="display:inline-block", actionButton("sharePage", "Share This Page")),
            div(style="display:inline-block", textOutput ("shareDestination"))
            ))

        )
    )


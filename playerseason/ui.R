## playerseason ui
## acthomas, 8-20-14

source("global.R")

#getGET <- function(inputId, selected = "") {
#  tagList(
#    singleton(tags$head(tags$script(src = "js/getBins.js"))),
#    tags$input(id = inputId,
#                class = "n_breaks",
#                value = selected),
#	tags$style(type='text/css', "#n_breaks { display:none; }")
#  )
#}

shinyUI(
    fluidPage(

        tags$head(tags$link(rel = "stylesheet", type = "text/css", id ='twentytwelve-fonts-css', href = "http://fonts.googleapis.com/css?family=Open+Sans:400italic,700italic,400,700&#038;subset=latin,latin-ext", media="all")),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "http://war-on-ice.com/css/capcheck.css")),

        tags$head(tags$style(".container-fluid { font-size: 12px;}")),
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        
    #      tags$head(
    #          tags$style(type="text/css", "select { font-size: 80%;}"),
    #          ),
    #    getGET(inputId="n_breaks"),
   
        h2('Detailed Individual Player Statistics', align="center"),

        wellPanel(
        fluidRow(
            column(3,
                   htmlOutput("onload.manchoice"), htmlOutput("onload.homechoice"),
                   downloadButton("downloadSData", "Download Season Table")),
            column(3,
                   htmlOutput("onload.scorechoice"), htmlOutput("onload.whichPosLoc"),
                   downloadButton("downloadGData", "Download Game Table")),
            column(3,
                   htmlOutput ("onload.name1"), htmlOutput ("nameSelect1"),
                   checkboxInput ("legacynames", "Use Legacy Names", value=TRUE))
                       ,
            column(3, htmlOutput ("onload.playoffs"), htmlOutput ("onload.daterange")
                   )
            )),
        
        hr(),
        htmlOutput ("player.info"),
        hr(),
        
        htmlOutput("onload.tabset"), #tabset
        h6("Note: Because the NHL didn't collect missed and blocked shots before October 2005, Corsi and Fenwick statistics from those years are calculated with shots and goals only."),

        div(style="display:inline-block", actionButton("sharePage", "Share This Page")),
        div(style="display:inline-block", textOutput ("shareDestination"))
        
        )
    )

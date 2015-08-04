## goalieseason ui
## acthomas, 8-20-14

source("global.R")

shinyUI(
    fluidPage(
        tags$head(tags$style(".container-fluid { font-size: 12px;}")), 
        tags$style(type="text/css", ".shiny-output-error { visibility: hidden; }", ".shiny-output-error:before { visibility: hidden; }" ),

        tags$head(tags$link(rel = "stylesheet", type = "text/css", id ='twentytwelve-fonts-css', href = "http://fonts.googleapis.com/css?family=Open+Sans:400italic,700italic,400,700&#038;subset=latin,latin-ext", media="all")),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "http://war-on-ice.com/css/capcheck.css")),
        

#        tags$head(tags$style(".control-label { font-size: 12px;}")),
#        tags$head(tags$style(".selectize-input { font-size: 12px;}")),   #shiny-bound-input
    #      tags$head(
    #          tags$style(type="text/css", "select { font-size: 80%;}"),
    #          ),
    
        h2('Individual Goaltender Statistics', align="center"),
    #helpText('Sortable Player Statistics'),
    #checkboxInput ("detail", "Show All Columns", FALSE),

        wellPanel(
        fluidRow(
            column(3, htmlOutput("onload.manchoice"),
                   htmlOutput("onload.scorechoice")
                   #htmlOutput("onload.whichPosLoc")
                   ),
            column(3, htmlOutput("onload.homechoice")
                   , htmlOutput ("onload.playoffs")
                   , htmlOutput ("onload.showRawStats")
                   ),
            column(3, htmlOutput ("onload.name1"),
                   htmlOutput ("nameSelect1")), 
            column(3, htmlOutput ("onload.daterange"),
                   downloadButton("downloadGData", "Download Game Table"),
                   downloadButton("downloadSData", "Download Season Table"))
            )
            ),
    
    #verbatimTextOutput ("diag"),

        hr(),
        htmlOutput ("player.info"),
        hr(),
        

        htmlOutput("onload.tabset"), #tabset
    
        div(style="display:inline-block", actionButton("sharePage", "Share This Page")),
        div(style="display:inline-block", textOutput ("shareDestination"))
        
        )
    )

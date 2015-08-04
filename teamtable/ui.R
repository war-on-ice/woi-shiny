## teamtable ui
## acthomas, 8-20-14

source("global.R")

shinyUI(
    fluidPage(
        tags$head(tags$style(".container-fluid { font-size: 12px;}")),
        tags$style(type="text/css", ".shiny-output-error { visibility: hidden; }", ".shiny-output-error:before { visibility: hidden; }" ),

        tags$head(tags$link(rel = "stylesheet", type = "text/css", id ='twentytwelve-fonts-css', href = "http://fonts.googleapis.com/css?family=Open+Sans:400italic,700italic,400,700&#038;subset=latin,latin-ext", media="all")),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "http://war-on-ice.com/css/capcheck.css")),

        h2('Team Comparisons', align="center"),
        #helpText('Sortable Team Statistics'),

        wellPanel(
        fluidRow(
            column(3, htmlOutput("onload.1"),
                   htmlOutput("onload.4")                
                   ,checkboxInput ("legacynames", "Use Legacy Names", value=TRUE)
                       ),
            column(3, htmlOutput("onload.2"),
                   htmlOutput("dynamic.period")
                   ,conditionalPanel("input.splitseasons",
                                     checkboxInput ("sepgame", "Split by Game", value=FALSE))

                   #,checkboxInput ("rinkbias", "Adjust For Rink Count Bias", value=FALSE)
                   #,checkboxInput ("fixhomead", "Adjust For Home Advantage", value=FALSE)
                   #,checkboxInput ("scoread", "Adjust For Score Differential", value=FALSE)
                   ),
            column(3, htmlOutput("onload.3"),
                   htmlOutput("onload.playoffs"),
                   htmlOutput("onload.splitseasons"),
                   
                   ##checkboxInput(inputId="playoffs", label="Include Playoffs", value = FALSE),
                   
                   downloadButton("downloadData", "Download Table")),
            column(3, conditionalPanel(condition="!input.datemode",
                                       htmlOutput ("onload.startseason"),
                                       htmlOutput ("onload.endseason")),
                   conditionalPanel(condition="input.datemode",
                                    htmlOutput ("onload.daterange")),
                   htmlOutput("onload.datemode")
                   )
            )
            ),

        hr(),
        htmlOutput("onload.tabset"),
        #dataTableOutput('mytable'),

        #
        
          
          
        h6("Note: Because the NHL didn't collect missed and blocked shots before October 2005, Corsi and Fenwick statistics from those years are calculated with shots and goals only."),
        verticalLayout(div(
            div(style="display:inline-block", actionButton("sharePage", "Share This Page")),
            div(style="display:inline-block", textOutput ("shareDestination"))
            ))
      
      
      )
    )

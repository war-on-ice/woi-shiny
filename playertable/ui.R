## playertable ui
## acthomas, 8-20-14

source("global.R")

shinyUI(
    fluidPage(
        tags$head(tags$style(".container-fluid { font-size: 12px;}")),
        tags$style(type="text/css", ".shiny-output-error { visibility: hidden; }", ".shiny-output-error:before { visibility: hidden; }" ),

        ##<link rel='stylesheet' id=  href='http://fonts.googleapis.com/css?family=Open+Sans:400italic,700italic,400,700&#038;subset=latin,latin-ext' type='text/css' media='all' />

        tags$head(tags$link(rel = "stylesheet", type = "text/css", id ='twentytwelve-fonts-css', href = "http://fonts.googleapis.com/css?family=Open+Sans:400italic,700italic,400,700&#038;subset=latin,latin-ext", media="all")),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "http://war-on-ice.com/css/capcheck.css")),
        
        h2('Player Comparisons', align="center"),
        wellPanel(
            fluidRow(
                column(3,
                       htmlOutput("onload.manchoice"),
                       htmlOutput("onload.homechoice"),
                       downloadButton("downloadData", "Download Table"),
                       htmlOutput ("onload.mintoi")),
                column(3,
                       htmlOutput ("onload.scorechoice"),
                       htmlOutput ("onload.whichPosLoc")
                       ,checkboxInput ("legacynames", "Use Legacy Names", value=TRUE)
                       ,selectInput ("FAscreen", "Screen For Pending Free Agents",
                                     choices = c("All","RFA","UFA"), selected="All")
                       ##checkboxInput ("FAscreen", "Screen For Pending Free Agents", value=FALSE)
                       ,conditionalPanel("input.splitseasons",
                                         checkboxInput ("sepgame", "Split by Game", value=FALSE))
                       #,checkboxInput ("rinkbias", "Adjust For Rink Count Bias", value=FALSE)
                       #,checkboxInput ("fixhomead", "Adjust For Home Advantage", value=FALSE)
                       #,checkboxInput ("scoread", "Adjust For Score Differential", value=FALSE)
                       ),
                column(3,
                       htmlOutput ("onload.whichPosition"),
                       htmlOutput ("onload.teamsearch"),
                       htmlOutput ("onload.playersearch")),
                
                column(3,
                       conditionalPanel(condition="!input.datemode",
                                        htmlOutput ("onload.start0"),
                                        htmlOutput ("onload.end0")),
                       conditionalPanel(condition="input.datemode",
                                        htmlOutput ("onload.daterange")),
                       htmlOutput("onload.datemode"),
                       htmlOutput ("onload.playoffs"),
                       htmlOutput ("onload.splitseasons"),
                       conditionalPanel("input.splitseasons",
                                        sliderInput ("agerange", "Age Range", min=18, max=50, value=c(18,50))
                                        )
                       )
                
                )
            
            ),
        
        fluidRow(
            column(12,
                   htmlOutput("onload.tab"), #tabset
                   h6("Note: Because the NHL didn't collect missed and blocked shots before October 2005, Corsi and Fenwick statistics from those years are calculated with shots and goals only."))),
        
        verticalLayout(div(
            div(style="display:inline-block", actionButton("sharePage", "Share This Page")),
            div(style="display:inline-block", textOutput ("shareDestination"))
            ))
    
    
    
  )
)

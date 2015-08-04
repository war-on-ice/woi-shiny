## goalie server ui
## acthomas, 8-20-14

source("global.R")

shinyUI(
    fluidPage(
        tags$head(tags$style(".container-fluid { font-size: 12px;}")),
        h2(' Goaltender Comparisons ', align="center"),

        tags$head(tags$link(rel = "stylesheet", type = "text/css", id ='twentytwelve-fonts-css', href = "http://fonts.googleapis.com/css?family=Open+Sans:400italic,700italic,400,700&#038;subset=latin,latin-ext", media="all")),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "http://war-on-ice.com/css/capcheck.css")),

        wellPanel(
        fluidRow(
            column(3,
                   div(title = "Choose a Man Strength Situation (5-on-5, power play, etc)",
                       htmlOutput("onload.manchoice")),
                   div(title = "Home/Away Splits",
                       htmlOutput("onload.homechoice"))
                   ,conditionalPanel("input.splitseasons",
                                     checkboxInput ("sepgame", "Split by Game", value=FALSE))
                   ),
            column(3,
                   div(title = "Choose a Score Situation",
                       htmlOutput("onload.scorechoice")),
                   div(title = "Show All Statistics in the Table",
                       htmlOutput("onload.playoffs")),
##                   div(checkboxInput(inputId="showRawStats", label="Show Extended Table", value = FALSE), title = "Show All Statistics in the Table"),
                   div(htmlOutput("onload.mintoi"),
                       title = "Filter by Minimum Time On Ice")
                   ),
            column(3,
                   div(title = "Choose a Specific Team",
                       htmlOutput("onload.teamsearch")),
                   div(title = "Type Any Part of a Player's Name to Find a Specific Player\nFilter Multiple Players by Separating Names with Commas",
                       textInput (inputId="playersearch", label="Filter Players"))
                   ),


            column(3,
                   div(title = "Specify a Custom Date Range",
                       htmlOutput("onload.daterange")),
                   div(downloadButton("downloadData", "Download Table"),
                       title = "Download This Table to .csv"),
                   htmlOutput ("onload.splitseasons"),

                   selectInput ("FAscreen", "Screen For Pending Free Agents",
                                choices = c("All","RFA","UFA"), selected="All")
                   ##checkboxInput ("FAscreen", "Screen For Pending Free Agents", value=FALSE)
                       )
            ),
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        )),

        fluidRow(column(12, dataTableOutput('mytable'))),

        fluidRow(
            column(3,
                   div(title = "Select an X-Axis Variable to Compare Goalies",
                       selectInput(inputId="xaxis", label="X Axis Variable",
                                   selected = "Unadjusted Save Percentage", choices=goalie.var.choices))),
            column(3,
                   div(title = "Select a Y-Axis Variable to Compare Goalies",
                       selectInput(inputId="yaxis", label="Y Axis Variable",
                                   selected = "Adjusted Save Percentage", choices=goalie.var.choices))),
            column(3,
                   div(title = "Select a Color Variable to Compare Goalies",
                       selectInput(inputId="caxis", label="Color Variable",
                                   selected = "Shots Faced, Per 60 Minutes", choices=goalie.var.choices))),
            column(3,
                   div(title = "Select a Size Variable to Compare Goalies",
                       selectInput(inputId="saxis", label="Size Variable",
                                   selected = "Time On Ice (Minutes)", choices=goalie.var.choices)))),
        plotOutput(outputId = "scatter", height="660px", width="800px"),
        verticalLayout(div(
            div(style="display:inline-block", actionButton("sharePage", "Share This Page")),
            div(style="display:inline-block", textOutput ("shareDestination"))
            ))
        )
    )

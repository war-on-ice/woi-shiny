## GAB ui
## acthomas, 8-20-14

shinyUI(
    basicPage(
        h2('Shooting Goals Above Baseline for NHL Teams, 2008-2014'),
        helpText('Estimated Expected Goals Above the average team due to frequency and location of unblocked shots'),
        checkboxInput ("detail", "Show Extended Stats", FALSE),
        dataTableOutput('mytable'),
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        )
        )
    )

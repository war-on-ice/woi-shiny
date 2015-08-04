## playerseason server
## acthomas, 8-20-14
library(shiny)
#load("season1314.RData")

source("global.R")

##sqlite.link <- "~/Documents/nhlr/war-on-ice.com/common-data/waronice.sqlite"
sqlite.link <- "../common-data/waronice.sqlite"

oneplayer.query <- function (player.name,
                             this.playoffs=TRUE,
                             this.regseason=TRUE,
                             daterange=c("2002-10-01",as.character(Sys.Date())),
                             my.gamestate = split.man.list[[5]],
                             my.scorediffcat = split.score.list[[1]],
                             my.home = homeaway.list[[1]]) {
    ## player.name = "Sidney Crosby"; playoffs=TRUE; daterange=c("2013-10-01","2014-04-01"); my.gamestate = split.man.list[[3]];  my.scorediffcat = split.score.list[[8]];  my.home = homeaway.list[[1]]

    woi.id <- roster.unique$woi.id[match(player.name, roster.unique$firstlast)]
    statement <- paste0('SELECT * FROM playerrun WHERE ID = "', woi.id,
                        '" AND scorediffcat = ',
                        my.scorediffcat,
                        ' AND gamestate = ',
                        my.gamestate,
                        ' AND period = 0 AND Date >= "', daterange[1],
                        '" AND Date <= "', daterange[2],'"',
                        collapse="")
    if (length(my.home) == 1) statement <- paste (statement, " AND home = ", my.home, collapse="")

    con <- dbConnect(SQLite(), dbname=sqlite.link)
    primer <- dbSendQuery(con, statement)
    this.subset <- fetch(primer, n = -1) %>%
        mutate (Name = roster.unique$firstlast[match(ID, roster.unique$woi.id)],
                label = as.character(paste0(season,"-",substr(gcode,1,1)))) %>% rename (thisdate = Date)
    dbDisconnect(con)
    
    if (!this.playoffs) this.subset <- subset(this.subset, substr(gcode,1,1)==2)
    if (!this.regseason) this.subset <- subset(this.subset, substr(gcode,1,1)==3)
    
    return (this.subset)
    
}






shinyServer(function(input, output, session){
  
  ## input <- list(manchoice="All", scorechoice="All", homechoice="All", minTOI=0, datemode=FALSE, playersearch="", teamsearch="", startdate="20132014 Regular", enddate="20132014 Regular", daterange=c("2013-01-01","2014-09-01"), player1="SIDNEY CROSBY"); m1=21; m2=21; teampick=""
    #GETterms <- reactive({g1 <- split.GET.to.frame(input$n_breaks); print(g1); g1 })
    GETterms <- reactive({g1 <- split.GET.to.frame(session$clientData$url_search); print(g1); g1})
    
    #print1 <- reactive(message(GETterms()$name1))
    ## Searchable pieces.
    
    onload.woiid <- reactive (if (!is.null(GETterms()$woiid)) GETterms()$woiid else "crosbsi87")
    onload.name1 <- reactive (if (!is.null(GETterms()$name1)) gsub("%27","'",gsub("0"," ",GETterms()$name1)) else
                              roster.unique$firstlast[match(onload.woiid(), roster.unique$woi.id)])
    
    output$onload.name1 <-
        renderUI (textInput (inputId="nameSearch1", label="Filter Players", value = onload.name1()))
    onload.name2 <- reactive (if (!is.null(GETterms()$name2)) unlist(strsplit(gsub("0"," ",GETterms()$name2), "\\+")) else NULL)
    #output$onload.name2 <-
    #    renderUI (textInput (inputId="nameSearch2", label="Filter Players", value = onload.name2()))
     
    searchResult1 <- reactive({
        ru.sub <- subset (roster.unique, pos != "G")
        pl.list <- ru.sub$firstlast[order(ru.sub$last)]
        pl.list <- pl.list[nchar(pl.list) > 3]
        pl.list[grep(tolower(input$nameSearch1), tolower(pl.list))]
    })
    output$nameSelect1 <- renderUI(selectInput ("player1", "", searchResult1()))
    
    searchResult2 <- reactive({
        message ("Shared names:")
        print (onload.name2())
        subset(roster.unique, pos != "G")$firstlast #ru.sub <- 
        #pl.list <- ru.sub$firstlast[order(ru.sub$last)]
        #pl.list <- pl.list[nchar(pl.list) > 3]
        #pl.list[grep(tolower(input$nameSearch2), tolower(pl.list))]
    })
    output$nameSelect2 <- renderUI(selectInput ("player2", "", searchResult2(), selected=onload.name2(), multiple=TRUE))

    
    onload.a <- reactive (if (!is.null(GETterms()$mansit) && (GETterms()$mansit %in% 1:length(split.man.choices)))
                              split.man.choices[as.numeric(GETterms()$mansit)] else "Even Strength 5v5")
    output$onload.manchoice <- renderUI(selectInput(inputId="manchoice", label="Team Strengths", selected = onload.a(), choices=split.man.choices))

    onload.b <- reactive (if (!is.null(GETterms()$scoresit) && (GETterms()$scoresit %in% 1:length(split.score.choices)))
                              split.score.choices[as.numeric(GETterms()$scoresit)] else "All")
    output$onload.scorechoice <- renderUI(selectInput(inputId="scorechoice", label="Score Situation", selected = onload.b(), choices=split.score.choices))

    onload.c <- reactive (if (!is.null(GETterms()$homeawaysit) && (GETterms()$homeawaysit %in% 1:length(homeaway.choices)))
                          homeaway.choices[as.numeric(GETterms()$homeawaysit)] else "All")
    output$onload.homechoice <- renderUI(selectInput(inputId="homechoice", label="Home/Away Situation", selected = onload.c(), choices=homeaway.choices))

    onload.d <- reactive (if (!is.null(GETterms()$tablegroup) &&
                                     (GETterms()$tablegroup %in% 1:length(screening.choices.pl)))
                                 screening.choices[as.numeric(GETterms()$tablegroup)] else "Prime")
    output$onload.whichPosLoc <-
        renderUI(selectInput(inputId="whichPosLoc", label="Columns To Display",
                             selected = onload.d(), choices=screening.choices.pl))

    
    #onload.srs <- reactive (if (!is.null(GETterms()$srs) && (GETterms()$srs %in% 0:1))
    #                        as.numeric(GETterms()$srs) else 0)
    #output$onload.showRawStats <- renderUI(checkboxInput(inputId="showRawStats",
    #                                                     label="Show Extended Table",
    #                                                     value = as.logical(onload.srs())))

    onload.playoffs <-
        reactive (if (!is.null(GETterms()$playoffs) && (GETterms()$playoffs %in% session.choices))
                            GETterms()$playoffs else session.choices[1])
    output$onload.playoffs <- renderUI(selectInput(inputId="playoffs",
                                                   label="Regular/Playoffs",
                                                   choices = session.choices,
                                                   selected = onload.playoffs()))


    onload.datestart <- reactive (if (!is.null(GETterms()$datestart)) # && (GETterms()$playoffs %in% 0:1)
                                  as.Date(GETterms()$datestart) else as.Date("2002-01-01"))
    onload.dateend <- reactive (if (!is.null(GETterms()$dateend)) # && (GETterms()$playoffs %in% 0:1)
                                  as.Date(GETterms()$dateend) else Sys.Date())
    output$onload.daterange <-
        renderUI(dateRangeInput('daterange',
                                label = 'Date range',
                                min = "2002-10-01", max = Sys.Date()+10,
                                start = onload.datestart(), end= onload.dateend(),
                                separator = " - ",
                                format = "yyyy-mm-dd", startview = 'year', language = 'en', weekstart = 1))
    ## Tabset objects.
    
    player.3.variables <- reactive(if (input$legacynames) player.variables[,1:3] else player.variables[,c(4,5,3)])
    player.short.match <- reactive(if (input$legacynames) player.variables[,c(1,1)] else player.variables[,c(1,4)])
    player.plot.subset <- reactive({
        out <- player.3.variables()[-(1:6),]; print(head(out)); out
    })
    player.var.choices <- reactive(player.plot.subset()[,2])


    onload.xaxis <- reactive (if (!is.null(GETterms()$xaxis) && (as.numeric(GETterms()$xaxis) %in%
                                                                 1:length(player.var.choices())))
                              as.numeric(GETterms()$xaxis) else which (player.variables[,2] == "Fraction of Off vs Def Zone Starts, Relative"))
    output$onload.xaxis <- renderUI(selectInput(inputId="xaxis", label="X Axis Variable",
                                                selected = player.var.choices()[onload.xaxis()], choices=c("Time",player.var.choices())))
    onload.yaxis <- reactive (if (!is.null(GETterms()$yaxis) && (as.numeric(GETterms()$yaxis) %in%
                                                                 1:length(player.var.choices())))
                              as.numeric(GETterms()$yaxis) else which (player.variables[,2] == "TOI/60 Of Competition"))
    output$onload.yaxis <- renderUI(selectInput(inputId="yaxis", label="Y Axis Variable",
                                                selected = player.var.choices()[onload.yaxis()], choices=player.var.choices()))
    onload.caxis <- reactive (if (!is.null(GETterms()$caxis) && (as.numeric(GETterms()$caxis) %in%
                                                                 1:length(player.var.choices())))
                              as.numeric(GETterms()$caxis) else which (player.variables[,2] == "Relative Corsi For Percentage of Total"))
    output$onload.caxis <- renderUI(selectInput(inputId="caxis", label="Color Variable",
                                                selected = player.var.choices()[onload.caxis()], choices=player.var.choices()))
    onload.saxis <- reactive (if (!is.null(GETterms()$saxis) && (as.numeric(GETterms()$saxis) %in%
                                                                 1:length(player.var.choices())))
                              as.numeric(GETterms()$saxis) else which (player.variables[,2] == "Time On Ice Per Game"))
    output$onload.saxis <- renderUI(selectInput(inputId="saxis", label="Size Variable",
                                                selected = player.var.choices()[onload.saxis()], choices=player.var.choices()))


    onload.xaxisg <- reactive (if (!is.null(GETterms()$xaxisg) && (as.numeric(GETterms()$xaxisg) %in%
                                                                 1:length(player.var.choices())))
                              as.numeric(GETterms()$xaxisg) else which (player.variables[,2] == "Corsi%"))
    output$onload.xaxisg <- renderUI(selectInput(inputId="xaxis.g", label="X Axis Variable",
                                                selected = player.var.choices()[onload.xaxisg()], choices=player.var.choices()))
    onload.yaxisg <- reactive (if (!is.null(GETterms()$yaxisg) && (as.numeric(GETterms()$yaxisg) %in%
                                                                 1:length(player.var.choices())))
                              as.numeric(GETterms()$yaxisg) else which (player.variables[,2] == "TOI/60 Of Competition"))
    output$onload.yaxisg <- renderUI(selectInput(inputId="yaxis.g", label="Y Axis Variable",
                                                selected = player.var.choices()[onload.yaxisg()], choices=player.var.choices()))

    
#    onload.xaxisg <- reactive (if (!is.null(GETterms()$xaxisg) && (GETterms()$xaxisg %in% player.var.choices))
#                              GETterms()$xaxisg else "Corsi%")
#    output$onload.xaxisg <- renderUI(selectInput(inputId="xaxis.g", label="Primary Variable",
#                                                 selected = onload.xaxisg(), choices=player.var.choices))
#    onload.yaxisg <- reactive (if (!is.null(GETterms()$yaxisg) &&
#                                   (as.numeric(GETterms()$yaxisg) %in% 0:length(player.var.choices)))
#                               c("(none)", player.var.choices)[as.numeric(GETterms()$yaxisg) + 1] else "(none)")
#    output$onload.yaxisg <- renderUI(selectInput(inputId="yaxis.g", label="Secondary Variable",
#                                                 selected = onload.yaxisg(), choices=c("(none)", player.var.choices)))

    
    ## Tabset panel.
    ## tab.choices <- c("Table By Season", "Season Plot",  "Career Plot", "Table By Game", "Game Plot")
    
    tab.choices <- c("Graphical By Season", "Tabular By Season",
                     "Graphical By Game", "Tabular By Game")
    onload.tabpanelchoice <-
        reactive (if (!is.null(GETterms()$panel) &&
                      (GETterms()$panel %in% 1:length(tab.choices)))
                  tab.choices[as.numeric(GETterms()$panel)] else tab.choices[1])
    
    output$onload.tabset <- renderUI(tabsetPanel(id="tabset",
                                                 
        tabPanel("Graphical By Season",
                 verticalLayout(
                     fluidRow(
                         column(3, htmlOutput("onload.xaxis")),
                         column(3, htmlOutput("onload.yaxis")),
                         column(3, htmlOutput("onload.caxis")),
                         column(3, htmlOutput("onload.saxis"))),
                     plotOutput(outputId = "scatter", height="600px", width="900px")) #,
            ),
        tabPanel("Tabular By Season",
                 verticalLayout(
                     dataTableOutput('mytable'))
            ),
        tabPanel("Graphical By Game",
                 verticalLayout(
                     fluidRow(
                         column(3, htmlOutput("onload.xaxisg")),
                         column(3, htmlOutput("onload.yaxisg")),
                         column(3, htmlOutput("nameSelect2")),
                         column(3, sliderInput(inputId="windowsize",
                                               "Number of Games in Moving Average",
                                               min = 1, max = 40, value = 20))
                     ,plotOutput(outputId = "careerbygame", height="900px", width="900px")
                     ))),
        tabPanel("Tabular By Game",
                 verticalLayout(
                     dataTableOutput('mytable.game'))),
                                                 selected = onload.tabpanelchoice()
        ))


    

    outdest <- reactive(paste0 ("http://war-on-ice.com/playerseason.html?",
                                "mansit=",which(input$manchoice == split.man.choices),
                                "&scoresit=",which(input$scorechoice == split.score.choices),
                                "&homeawaysit=",which(input$homechoice == homeaway.choices),
                                "&name1=",gsub(" ","0",input$player1),
                                "&name2=",gsub(" ","0",paste(input$player2, collapse="+")),
                                "&tablegroup=",which(input$whichPosLoc == shotatt.choices),
                                #"&srs=",as.numeric(input$showRawStats),
                                "&playoffs=",input$playoffs,
                                "&datestart=",input$daterange[1],
                                "&dateend=",input$daterange[2],
                                "&xaxis=",which(input$xaxis == player.var.choices()),
                                "&yaxis=",which(input$yaxis == player.var.choices()),
                                "&caxis=",which(input$caxis == player.var.choices()),
                                "&saxis=",which(input$saxis == player.var.choices()),
                                #"&yaxis2=",which(input$yaxis2 == var.choices),
                                #"&caxis2=",which(input$caxis2 == var.choices),
                                #"&saxis2=",which(input$saxis2 == var.choices),
                                "&xaxisg=",which(input$yaxisg == player.var.choices()),
                                "&yaxisg=",which(input$yaxisg == c("(none)", player.var.choices()))-1,
                                "&panel=",which(input$tabset == tab.choices)))
                        
    output$shareDestination <- renderText({ 
        input$sharePage
#        message(GETterms()$name1)
        outdest()
    })

    woiid.1 <- reactive(roster.unique$woi.id[match(input$player1, roster.unique$firstlast)])
    oid <- reactive({
        id <- match(input$player1, roster.unique$firstlast)
        list(roster.unique$firstlast[id], roster.unique$DOB[id],
             roster.unique$Shoots[id], roster.unique$Height[id],
             roster.unique$Weight[id], roster.unique$pos[id])
    })
    player.position <- reactive({
        ppos <- unlist(strsplit(oid()[[6]],""))
        c("Center", "Right Wing", "Left Wing", "Defense", "Goaltender")[match(ppos, c("C","R","L","D","G"))]
    })
    
    output$player.info <- renderUI(list(
        fluidRow(
            column(3, h4(oid()[[1]]), h6(paste("Position:", paste(player.position(), collapse=", ")))),
            column(3, h6(paste("Date of Birth:", oid()[[2]])),
                   h6(HTML(paste0("<a href=\"http://war-on-ice.com/cap/", woiid.1(),".html\" target=\"_blank\">Ask Me About My Cap Hit</a>")))),
            column(2, h6(paste("Shoots:", oid()[[3]]))),
            column(2, h6(paste("Height:", oid()[[4]]))),
            column(2, h6(paste("Weight:", oid()[[5]])))
            )
        ))

    queried.data.all <-
        reactive (lapply(c(input$player1, input$player2),
                         function(thisname)
                         oneplayer.query (thisname,
                                          this.playoffs = input$playoffs %in% c("All", "Playoffs"),
                                          this.regseason = input$playoffs %in% c("All", "Regular"),
                                          daterange = input$daterange,
                                          my.gamestate=split.man.list[[match(input$manchoice, split.man.choices)]],
                                          my.scorediffcat=split.score.list[[match(input$scorechoice, split.score.choices)]],
                                          my.home=homeaway.list[[match(input$homechoice, homeaway.choices)]]) ) )

    
    ## For making seasonal everything.
    season.data <- reactive ({
        season.out <- queried.data.all()[[1]] %>%
            swapnames %>% 
                group_by (ID, season) %>%
                    compress.player.game %>% add.rates.players

        rowmatch <- match (paste0(season.out$ID, season.out$season),
                           paste0(contracts$woiid, contracts$Year-1, contracts$Year))
            #print(rowmatch)
            #print(contracts$TotalComp[rowmatch])
            #print(contracts$AAV[rowmatch])
        season.out$Salary <- sprintf("%.3f", contracts$TotalComp[rowmatch]/1E6)
        season.out$AAV <- sprintf("%.3f", contracts$AAV[rowmatch]/1E6)

        season.out$G60 <- round(season.out$G / season.out$TOI * 60, 2)
        season.out$A60 <- round((season.out$A1 + season.out$A2) / season.out$TOI * 60, 2)
        season.out$P60 <- round((season.out$G + season.out$A1 + season.out$A2) / season.out$TOI * 60, 2)

        season.out$Age <- ageAsOfSep15 (season.out$season, as.character(roster.unique$DOB[match(season.out$ID, roster.unique$woi.id)]))

        
        season.out
    })

    gamedata <- reactive (lapply(queried.data.all(), function (qd) {
        big.out <- qd %>% swapnames %>%
            #make.count.adjustments.player (do.rink=input$rinkbias,
            #                               do.home=input$fixhomead,
            #                               do.scorestate=input$scoread) %>%
                                               group_by (ID, season, gcode) %>% compress.player.game %>% add.rates.players
        big.out$gameid <- paste0(big.out$season, big.out$gcode)
        big.out$Date <- gamestest$date[match(big.out$gameid, paste0(gamestest$season, gamestest$gcode))]

        big.out$G60 <- round(big.out$G / big.out$TOI * 60, 2)
        big.out$A60 <- round((big.out$A1 + big.out$A2) / big.out$TOI * 60, 2)
        big.out$P60 <- round((big.out$G + big.out$A1 + big.out$A2) / big.out$TOI * 60, 2)
        
        big.out <- big.out[order(big.out$Date),]

        ##message("gamedata")
        ##print(head(big.out,2))
        big.out
    }))


    seasontable.data <- reactive ({
      
        out <- season.data()
        out2 <- player.pick.columns (out,
                                     data.frame(Season=paste(paste(substr(out$season, 1, 4), substr(out$season, 7, 8), sep = "."),
                                                    ifelse(substr(out$season, 10, 10) == "3", ".Playoffs", ""), sep = ""),
                                                Age=out$Age,
                                                Salary=out$Salary,
                                                AAV=out$AAV,
                                                Team=out$Team, Gm=out$Gm),  
                                     group=input$whichPosLoc)

        return(out2[rev(order(out2$Season)),])
        
    })
  
    output$mytable = renderDataTable({
        out <- seasontable.data()
        out$Team <- teamtime.url(out$Team)

        colnames(out) <- swapout.oldnew (colnames(out), player.short.match()) %>% augment.html.div (player.3.variables())
        out
    }, options = list(searching = FALSE,
           scrollX = "100%", scrollY = "100%",
           lengthChange = FALSE, pageLength=20)
        )
    output$downloadSData <- downloadHandler (
        filename = paste0("war-on-ice-", Sys.time(),".csv"),
        content = function (file) write.csv ({
            out <- seasontable.data()
            out
        }, file)
        )



    gametable.data <- reactive ({
      
        out <- gamedata()[[1]]

        opponent.team <- rep("", length(out$Team))
        hometeam <- gamestest$hometeam[match(out$gameid, paste0(gamestest$season, gamestest$gcode))]
        awayteam <- gamestest$awayteam[match(out$gameid, paste0(gamestest$season, gamestest$gcode))]
        opponent.team[out$Team != hometeam] <- hometeam[out$Team != hometeam]
        opponent.team[out$Team != awayteam] <- awayteam[out$Team != awayteam]
    
        out2 <- player.pick.columns (out,
                                     data.frame(Date=out$Date,
                                                Team=out$Team,
                                                Opponent=opponent.team),
                                     group=input$whichPosLoc)

        out2 <- out2[order(out2$Date),]

        out2 <- out2[,unique(colnames(out2))]
        out2
    })


    output$mytable.game = renderDataTable({
        out <- gametable.data()
        out$Date <- woigame.url(out$Date, paste0(gamedata()[[1]]$season, gamedata()[[1]]$gcode)) 
        out$Team <- teamtime.url(out$Team)
        out$Opponent <- teamtime.url(out$Opponent)


        colnames(out) <- swapout.oldnew (colnames(out), player.short.match()) %>% augment.html.div (player.3.variables())
        #colnames(out) <- player.short.match()[match(colnames(out), player.short.match()[,1]), 2]
        #colnames(out) <- augment.html.div (colnames(out), player.3.variables())

        out <- out[rev(order(gametable.data()$Date)),]
        out
    }, options = list(searching = FALSE,
           scrollX = "100%", scrollY = "100%",
           lengthChange = FALSE, pageLength=20)
        )

    output$downloadGData <- downloadHandler (
        filename = paste0("war-on-ice-", Sys.time(),".csv"),
        content = function (file) write.csv ({out2 <- gametable.data();
                                              out2$Date <- un.url(out2$Date)
                                              out2$Team <- un.url(out2$Team)
                                              out2$Opponent <- un.url(out2$Opponent)
                                              out2[order(out2$Date),]}, file)
        )
  

    
    
    output$scatter = renderPlot ({
        out <- season.data()
        out <- out[order(out$season),]
        myplan <- layout (matrix(c(2,1), nrow=1, ncol=2), widths=c(1, 6.5), heights=c(6))

        pieces.1 <- player.3.variables()[match(c(input$xaxis, input$yaxis, input$caxis, input$saxis),
                                               player.3.variables()[,2]), 1]
        print(pieces.1)
        pieces.2 <- player.short.match()[match(pieces.1, player.short.match()[,2]), 1]
        subdata <- out[,pieces.2];

        
#        pieces <- player.variables[match(c(input$xaxis, input$yaxis, input$caxis, input$saxis),
#                                         player.variables[,2]), 1]        ##print(pieces)
#        subdata <- out[,pieces]
        
        #print(datacat)
        ## Main plot

        ## Get the color profiles.
        legend.length <- 25
        colprimer = subdata[[3]]
        #datacat <- player.variables[match(c(input$xaxis, input$yaxis, input$caxis, input$saxis),
        #                                 player.variables[,2]), 3]
        datacat <- player.3.variables()[match(c(input$xaxis, input$yaxis, input$caxis, input$saxis),
                                         player.3.variables()[,2]), 3]
        
        if (datacat[3] == "diff") {
            data.extremes <- c(-max(abs(colprimer), na.rm=TRUE), max(abs(colprimer), na.rm=TRUE))
            colrange <- c("#FF8888","white","#8888FF")
        } else if (datacat[3] == "100") {
            data.extremes <- 100+c(-max(abs(colprimer-100), na.rm=TRUE), max(abs(colprimer-100), na.rm=TRUE))
            colrange <- c("#FF8888","white","#8888FF")
        } else if (datacat[3] == "frac") {
            data.extremes <- c(0, 100) # -max(abs(colprimer)), max(abs(colprimer)))
            colrange <- c("#FF8888","#FF8888","#FF8888","white","#8888FF","#8888FF","#8888FF")
        } else {    ## "count"
            data.extremes <- range(colprimer, na.rm=TRUE)
            colrange <- c("#FF8888","white","#8888FF")
        }

        this.col <- crp(colrange,(colprimer - data.extremes[1])/(data.extremes[2] - data.extremes[1]))
        d.range <- seq(data.extremes[1], data.extremes[2], length=legend.length)


        ## Sizes.
        this.cex <- 15*(subdata[[4]]/max(subdata[[4]]))
        xvar <- if (input$xaxis == "Time") 1:length(out[["season"]]) else subdata[[1]]
        
        ## Actual plotting.
        plot (xvar, subdata[[2]],
              xlab=input$xaxis, ylab=input$yaxis, xaxt='n',
              cex=this.cex, col=this.col, pch=16,
              main=paste(input$player1, input$yaxis, "vs.", input$xaxis))
        points (xvar, subdata[[2]], cex=this.cex)
        if (input$xaxis == "Time") {
            axis(1, at = 1:length(out[["season"]]), labels = fixSeasons(out[["season"]]))
            lines (xvar, subdata[[2]])  
        } else axis(1)
        text (xvar, subdata[[2]], fixSeasons(out$season))

        ## Branding.
        legend ("bottomright", "war-on-ice.com", bty="n", cex=0.8)
        
        ## Legend.
        par(mar=c(3,3,3,0))
        xlim1 <- 16*c(-0.5, 0.5)/7/1.7
        plot (rep(0, legend.length), d.range,
              xlim=xlim1, axes=FALSE, cex=4, main=input$caxis, cex.main=0.8,
              pch=16, col=crp(colrange, seq(0, 1, length=legend.length)))
        points (rep(0, legend.length), d.range, cex=4)
        axis(2)
        
    })
    
    
    
    output$careerbygame = renderPlot ({

        par(mfrow = c(2,1))

        pieces.1 <- player.3.variables()[match(c(input$xaxis.g, input$yaxis.g), player.3.variables()[,2]), 1]
        pieces.2 <- player.short.match()[match(pieces.1, player.short.match()[,2]), 1]
        
        
        matchpiece <- pieces.2[1] ##player.plot.variables[match(input$xaxis.g, player.plot.variables[,2]), 1]
        matchpiece.2 <- pieces.2[2] ## player.plot.variables[match(input$yaxis.g, player.plot.variables[,2]), 1]
        ##print(matchpiece); print(head(gamedata()[[1]]))

        data.pieces <- gamedata()
        
        outputs <- lapply(gamedata(), function(tm) rolling.mean(tm[[matchpiece]], count = input$windowsize))

        
        #################################################################################
        ## Rolling mean plot.

        main <- if (length(input$player2) > 0) paste (input$player1, "vs.", paste(input$player2, collapse=", "), "over time") else paste (input$player1, "over time")
        ##message("Game data:")
        ##print(head(gamedata()[[1]]))
        
        plot (range(as.Date(gamedata()[[1]]$Date), na.rm=TRUE),
              range(unlist(outputs), na.rm=TRUE), main = main,
              xaxt="n",
              ty="n", xlab="Date", ylab=input$xaxis.g)

        date.diff <- max(as.Date(gamedata()[[1]]$Date)) - min(as.Date(gamedata()[[1]]$Date))
        axis.Date (side=1, at = seq (min(as.Date(gamedata()[[1]]$Date)), max(as.Date(gamedata()[[1]]$Date)),
                               if (date.diff > 730) "years" else "months"))

        abline (h=seq(-300,300,by=10), col=8, lwd=2, lty=2)
        
        if (matchpiece %in% c("ZSO%", "GF%", "FF%", "CF%", "SF%")) abline (h=50, col=8)
        if (matchpiece %in% c("G+/-", "F+/-", "C+/-", "S+/-")) abline (h=0, col=8)
        if (matchpiece %in% c("PDO")) abline (h=100, col=8)

        for (tm in 1:length(outputs)) {
            lines (as.Date(gamedata()[[tm]]$Date), outputs[[tm]], col=tm, lwd=3)
        }
        for (kk in 1:nrow(season.breaks)) rect (season.breaks[kk,1], -1000, season.breaks[kk,2], 1000, col="#EEEEEE", border="#EEEEEE")
        legend ("bottomright", "war-on-ice.com", bty="n", cex=1) ## Legend.

        player <- c(input$player1, input$player2)
        legend ("topleft", player, 
                col=1:length(player),
                text.col=1, #team.colors$color[match(teams, team.colors$team)],
                lwd=c(rep(12, length(player))),
                lty = c(rep(1, length(player))))


        
        #################################################################################
        ## Density plot piece.

        densities <- lapply(gamedata(), function(tm) if (input$yaxis.g == "(none)") {
            if (matchpiece %in% c("ZSO%", "GF%", "FF%", "CF%", "SF%")) {
                density(tm[[matchpiece]], na.rm = T, from = 0, to = 100)
            } else if (matchpiece %in% c("PDO")){
                density(tm[[matchpiece]], na.rm = T, from = 0, to = 200)
            } else {
                density(tm[[matchpiece]], na.rm = T)
            }
        } else {
            bits <- cbind(tm[[matchpiece]], tm[[matchpiece.2]])
            bkde2D (bits[complete.cases(bits),], 50)
        })

        if (input$yaxis.g == "(none)") {

            main2 <- if (length(input$player2) > 0) paste ("Distribution of Game-by-Game", input$xaxis.g, "\n", input$player1, "vs.", paste(input$player2,collapse=", ")) else paste ("Distribution of", input$xaxis.g, "for", input$player1)
        
            plot (range(unlist(lapply(densities, function(dd) dd$x)), na.rm=TRUE),
                  range(unlist(lapply(densities, function(dd) dd$y)), na.rm=TRUE),
                  ty="n", xlab=paste("Game-by-Game", input$xaxis.g), ylab="Density", main=main2)
            
            if (matchpiece %in% c("ZSO%", "GF%", "FF%", "CF%", "SF%")) abline (h=50, col=8)
            if (matchpiece %in% c("G+/-", "F+/-", "C+/-", "S+/-")) abline (h=0, col=8)
            if (matchpiece %in% c("PDO")) abline (h=100, col=8)
            
            for (tm in 1:length(outputs)) {
                abline (v=mean(gamedata()[[tm]][[matchpiece]], na.rm = TRUE), col=tm, lty = 3, lwd = 3)
            }
            
            for (tm in 1:length(outputs)) {
                lines (densities[[tm]], col=tm, lwd=8)
                                        #            lines (densities[[tm]], col=team.colors$color2[match(teams[tm], team.colors$team)], lwd=3)
            }
            
            ## Legends.
            legend ("bottomright", "war-on-ice.com", bty="n", cex=1)
            
            player <- c(input$player1, input$player2)
            legend ("topleft", c(player, paste(player, "average")), 
                    col=rep(1:length(player),2),
                    text.col=1, #team.colors$color[match(teams, team.colors$team)],
                    lwd=c(rep(12, length(player)), rep(3, length(player))),
                    lty = c(rep(1, length(player)), rep(3, length(player))))

        } else {

            d1 <- cbind(data.pieces[[1]][[matchpiece]], data.pieces[[1]][[matchpiece.2]])
            d1 <- d1[complete.cases(d1),]
            print(head(d1))
            
            main2 <- if (length(input$player2) > 0) paste ("Distribution of Game-by-Game", matchpiece.2, "vs", matchpiece, "\n", input$player1, "vs.", paste(input$player2, collapse=",")) else paste ("Distribution of", matchpiece.2, "vs", matchpiece, "for", input$player1)
      
            range.x <- range(sapply(data.pieces, function(oo) oo[[matchpiece]]), na.rm=TRUE)
            range.y <- range(sapply(data.pieces, function(oo) oo[[matchpiece.2]]), na.rm=TRUE)

            plot (data.pieces[[1]][[matchpiece]],
                  data.pieces[[1]][[matchpiece.2]], main=main2,
                  xlab=matchpiece, ylab=matchpiece.2, xlim=range.x, ylim=range.y, pch=19)

            if (length(input$player2) > 0) d1 <- lapply(1:(length(densities)-1), function(ii) {
                print(head(data.pieces[[ii+1]]))
                print(data.pieces[[ii+1]][[matchpiece]])
                print(data.pieces[[ii+1]][[matchpiece.2]])
                
                points (data.pieces[[ii+1]][[matchpiece]], 
                        data.pieces[[ii+1]][[matchpiece.2]],
                        col=ii+1, pch=19)
            })
            
            for (ii in 1:(length(densities))) {
                contour (densities[[ii]]$x1, densities[[ii]]$x2, densities[[ii]]$fhat, col=ii, nlevels=5, add=TRUE)
            }

            ## Legends.
            legend ("bottomright", "war-on-ice.com", bty="n", cex=1)




        }



        
    })

  
  # output$Errorchecker <- renderPrint ({
  #     input$daterange
  # })
  
})

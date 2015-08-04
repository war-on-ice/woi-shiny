## goalieseason server
## acthomas, 8-20-14
library(shiny)
library(KernSmooth)
##load("season1314.RData")

source("global.R")

load ("../common-data/woi-common.RData")
season.breaks <- read.csv("../common-data/seasonbreaks.csv"); season.breaks[,1] <- as.Date(season.breaks[,1]); season.breaks[,2] <- as.Date(season.breaks[,2])

sqlite.link <- "../common-data/waronice.sqlite"
##sqlite.link <- "~/Documents/nhlr/war-on-ice.com/common-data/woi3.sqlite"


remake.baselines <- function () {

    con <- dbConnect(SQLite(), dbname=sqlite.link)
    statement <- "SELECT scorediffcat, gamestate, period, home, SUM(`goals.0`), SUM(`goals.1`), SUM(`goals.2`), SUM(`goals.3`), SUM(`goals.4`),  SUM(`shots.0`), SUM(`shots.1`), SUM(`shots.2`), SUM(`shots.3`), SUM(`shots.4`) FROM goalierun GROUP BY scorediffcat, gamestate, period, home"  
    primer <- dbSendQuery(con, statement)  ##'SELECT * FROM goalierun WHERE scorediffcat = 7 AND gamestate = 7 AND period = 0 GROUP BY season, gcode')
    conditions <- fetch(primer, n = -1)
    colnames(conditions)[-(1:4)] <- c("goals.0","goals.1","goals.2","goals.3","goals.4",
                                      "shots.0","shots.1","shots.2","shots.3","shots.4")
                                      
    save(conditions, file="../common-data/conditions.RData")

}


oneplayer.query <- function (player.name,
                             playoffs=TRUE,
                             regseason=TRUE,
                             daterange=c("2002-10-01",as.character(Sys.Date())),
                             my.gamestate = split.man.list[[5]],
                             my.scorediffcat = split.score.list[[1]],
                             my.home = homeaway.list[[1]]) {
    ## player.name = "Corey Crawford"; playoffs=TRUE; daterange=c("2002-10-01","2015-07-01"); my.gamestate = split.man.list[[3]];  my.scorediffcat = split.score.list[[8]];  my.home = homeaway.list[[1]]

    woi.id <- roster.unique$woi.id[match(player.name, roster.unique$firstlast)]
    statement <- paste0('SELECT * FROM goalierun WHERE ID = "', woi.id, ##'"')
                        #,
                        '" AND scorediffcat = ',
                        my.scorediffcat,
                        ' AND gamestate = ',
                        my.gamestate,
                        ' AND period = 0 AND Date >= "', daterange[1], '" AND Date <= "', daterange[2],'"',
                        collapse="")
    if (length(my.home) == 1) statement <- paste (statement, " AND home = ", my.home, collapse="")

    con <- dbConnect(SQLite(), dbname=sqlite.link)
    primer <- dbSendQuery(con, statement)
    this.subset <- fetch(primer, n = -1) %>%
        mutate (Name = roster.unique$firstlast[match(ID, roster.unique$woi.id)],
                label = as.character(paste0(season,"-",substr(gcode,1,1)))) %>% rename (thisdate = Date)
    dbDisconnect(con)

    if (!playoffs) this.subset <- subset(this.subset, substr(gcode,1,1)==2)
    if (!regseason) this.subset <- subset(this.subset, substr(gcode,1,1)==3)
    
    return (this.subset)
    
}




shinyServer(function(input, output, session) {

    
    GETterms <- reactive({g1 <- split.GET.to.frame(session$clientData$url_search); g1})

    onload.woiid <- reactive (if (!is.null(GETterms()$woiid)) GETterms()$woiid else "quickjo86")
    onload.name1 <- reactive (if (!is.null(GETterms()$name1)) gsub("%27","'",gsub("0"," ",GETterms()$name1)) else
                              roster.unique$firstlast[match(onload.woiid(), roster.unique$woi.id)])

    ##onload.name1 <- reactive (if (!is.null(GETterms()$name1)) gsub("0"," ",GETterms()$name1) else "Quick")
    output$onload.name1 <-
        renderUI (textInput (inputId="nameSearch1", label="Filter Players", value = onload.name1()))

    ##onload.name2 <- reactive (if (!is.null(GETterms()$name2)) gsub("0"," ",GETterms()$name2) else "Lundqvist")
    ##output$onload.name2 <- renderUI (textInput (inputId="nameSearch2", label="Filter Players", value = onload.name2()))
    
    searchResult1 <- reactive({      
        ru.sub <- subset (roster.unique, pos == "G")
        pl.list <- ru.sub$firstlast[order(ru.sub$last)]
        pl.list <- pl.list[nchar(pl.list) > 3]
        pl.list[grep(tolower(input$nameSearch1), tolower(pl.list))]
    })
    output$nameSelect1 <- renderUI(selectInput ("player1", "", searchResult1()))

    goalieroster <- reactive({      
        ru.sub <- subset (roster.unique, pos == "G")
        pl.list <- ru.sub$firstlast[order(ru.sub$last)]
        pl.list[nchar(pl.list) > 3]
        ##pl.list[grep(tolower(input$nameSearch2), tolower(pl.list))]
    })
    output$nameSelect2 <- renderUI(selectInput ("player2", "Comparison Players", selected=NULL,
                                                choices = goalieroster(), multiple=TRUE))
    

    
    onload.a <- reactive (if (!is.null(GETterms()$mansit) && 
                              (GETterms()$mansit %in% 1:length(split.man.choices)))
                          split.man.choices[as.numeric(GETterms()$mansit)] else "Even Strength 5v5")
    output$onload.manchoice <- renderUI(selectInput(inputId="manchoice", 
                                                    label="Team Strengths", selected = onload.a(), 
                                                    choices=split.man.choices))
  
    onload.b <- reactive (if (!is.null(GETterms()$scoresit) && 
                              (GETterms()$scoresit %in% 1:length(split.score.choices)))
                          split.score.choices[as.numeric(GETterms()$scoresit)] else "All")
    output$onload.scorechoice <- renderUI(selectInput(inputId="scorechoice", 
                                                      label="Score Situation", selected = onload.b(), 
                                                      choices=split.score.choices))
    
    onload.c <- reactive (if (!is.null(GETterms()$homeawaysit) && 
                              (GETterms()$homeawaysit %in% 1:length(homeaway.choices)))
                          homeaway.choices[as.numeric(GETterms()$homeawaysit)] else "All")
    output$onload.homechoice <- renderUI(selectInput(inputId="homechoice", 
                                                     label="Home/Away Situation", selected = onload.c(), 
                                                     choices=homeaway.choices))
    
    onload.srs <- reactive (if (!is.null(GETterms()$srs) && (GETterms()$srs %in% 0:1))
                            as.numeric(GETterms()$srs) else 0)
    output$onload.showRawStats <- renderUI(checkboxInput(inputId="showRawStats",
                                                         label="Show Extended Table",
                                                         value = as.logical(onload.srs())))


    onload.playoffs <-
        reactive (if (!is.null(GETterms()$playoffs) && (GETterms()$playoffs %in% session.choices))
                  GETterms()$playoffs else session.choices[1])
    output$onload.playoffs <- renderUI(selectInput(inputId="playoffs",
                                                   label="Regular/Playoffs",
                                                   choices = session.choices,
                                                   selected = onload.playoffs()))

    #onload.playoffs <- reactive (if (!is.null(GETterms()$playoffs) && (GETterms()$playoffs %in% 0:1))
    #                             as.numeric(GETterms()$playoffs) else 0)
    #output$onload.playoffs <- renderUI(checkboxInput(inputId="playoffs",
    #                                                 label="Include Playoff Stats",
    #                                                 value = as.logical(onload.playoffs())))
    
    
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
    
    onload.xaxis <- reactive (if (!is.null(GETterms()$xaxis) && (as.numeric(GETterms()$xaxis) %in% 0:length(goalie.var.choices)))
                              as.numeric(GETterms()$xaxis) else 0)
    output$onload.xaxis <- renderUI(selectInput(inputId="xaxis", label="X Axis Variable",
                                                selected = c("Time",goalie.var.choices)[1+onload.xaxis()],
                                                choices = c("Time",goalie.var.choices)))
    
    onload.yaxis <- reactive (if (!is.null(GETterms()$yaxis) && (as.numeric(GETterms()$yaxis) %in% 1:length(goalie.var.choices)))
                              as.numeric(GETterms()$yaxis) else which(goalie.var.choices == "Adjusted Save Percentage"))
    output$onload.yaxis <- renderUI(selectInput(inputId="yaxis", label="Y Axis Variable",
                                                selected = goalie.var.choices[onload.yaxis()],
                                                choices=goalie.var.choices))
    
    onload.caxis <- reactive (if (!is.null(GETterms()$caxis) && (as.numeric(GETterms()$caxis) %in% 1:length(goalie.var.choices)))
                              as.numeric(GETterms()$caxis) else which(goalie.var.choices == "High-Danger Save Percentage"))
    output$onload.caxis <- renderUI(selectInput(inputId="caxis", label="Color Variable",
                                                selected = goalie.var.choices[onload.caxis()],
                                                choices=goalie.var.choices))
    
    onload.saxis <- reactive (if (!is.null(GETterms()$saxis) && (as.numeric(GETterms()$saxis) %in% 1:length(goalie.var.choices)))
                              as.numeric(GETterms()$saxis) else which(goalie.var.choices == "Games"))
    output$onload.saxis <- renderUI(selectInput(inputId="saxis", label="Size Variable",
                                                selected = goalie.var.choices[onload.saxis()],
                                                choices=goalie.var.choices))
    
    onload.xaxisg <- reactive (if (!is.null(GETterms()$xaxisg) && (as.numeric(GETterms()$xaxisg) %in% 1:length(goalie.var.choices)))
                               as.numeric(GETterms()$xaxisg) else which(goalie.var.choices == "Adjusted Save Percentage"))
    output$onload.xaxisg <- renderUI(selectInput(inputId="xaxis.g", label="Primary Variable",
                                                 selected = goalie.var.choices[onload.xaxisg()],
                                                 choices=goalie.var.choices))
                              
    onload.yaxisg <- reactive (if (!is.null(GETterms()$yaxisg) && (as.numeric(GETterms()$yaxisg) %in% 0:length(goalie.var.choices)))
                               as.numeric(GETterms()$yaxisg) else 0)
    output$onload.yaxisg <- renderUI(selectInput(inputId="yaxis.g", label="Secondary Variable",
                                                 selected = c("(none)",goalie.var.choices)[1+onload.yaxisg()],
                                                 choices=c("(none)",goalie.var.choices)))

                              
    
    ## Tabset panel.
    ## tab.choices <- c("Table By Season", "Season Plot",  "Career Plot", "Table By Game", "Game Plot")
    tab.choices <- c("Graphical By Season", "Tabular By Season", "Graphical By Game", "Tabular By Game")
    onload.tabpanelchoice <- reactive (if (!is.null(GETterms()$panel) && (GETterms()$panel %in% 1:length(tab.choices)))
                                       tab.choices[as.numeric(GETterms()$panel)] else tab.choices[1])
    output$onload.tabset <-
        renderUI(tabsetPanel(id="tabset",
                             tabPanel("Graphical By Season",
                                      fluidRow(
                                          column(3, htmlOutput("onload.xaxis")),
                                          column(3, htmlOutput("onload.yaxis")),
                                          column(3, htmlOutput("onload.caxis")),
                                          column(3, htmlOutput("onload.saxis"))
                                          ),
                                      plotOutput(outputId = "scatter", height="600px", width="900px")),
                             tabPanel("Tabular By Season",
                                      dataTableOutput('mytable')),
                             tabPanel("Graphical By Game",
                                      fluidRow(
                                          column(3, htmlOutput("onload.xaxisg")),
                                          column(3, htmlOutput("onload.yaxisg")),
                                          column(3, htmlOutput("nameSelect2")),   #htmlOutput("onload.name2")
                                          column(3, 
                                                 sliderInput(inputId="windowsize",
                                                             "Number of Games in Moving Average",
                                                             min = 1, max = 40, value = 20)
                                                 ##,checkboxInput("exponential")
                                                 )),
                                      plotOutput(outputId = "careerbygame", height="900px", width="900px")
                                      ),
                             tabPanel("Tabular By Game",
                                      dataTableOutput('mytable.game')),
                             selected = onload.tabpanelchoice()
                             ))
  
  
    outdest <- reactive(paste0 ("http://war-on-ice.com/goalieseason.html?",
                                "mansit=",which(input$manchoice == split.man.choices),
                                "&scoresit=",which(input$scorechoice == split.score.choices),
                                "&homeawaysit=",which(input$homechoice == homeaway.choices),
                                "&name1=",gsub(" ","0",input$player1),
                                "&name2=",gsub(" ","0",input$player2),
                                        #"&shotattsit=",which(input$whichPosLoc == shotatt.choices),
                                "&srs=",as.numeric(input$showRawStats),
                                "&playoffs=",input$playoffs,
                                "&datestart=",input$daterange[1],
                                "&dateend=",input$daterange[2],
                                "&xaxis=",which(input$xaxis == c("Time", goalie.var.choices) - 1),
                                "&yaxis=",which(input$yaxis == goalie.var.choices),
                                "&caxis=",which(input$caxis == goalie.var.choices),
                                "&saxis=",which(input$saxis == goalie.var.choices),

                                "&xaxisg=",which(input$xaxisg == goalie.var.choices),
                                "&yaxisg=",which(input$yaxisg == c("(none)",goalie.var.choices) - 1),
                                "&panel=",which(input$tabset == tab.choices)))
    
    output$shareDestination <- renderText({ 
        input$sharePage
        outdest()
    })


    ## #################################################################
    ##
    ## Display features
    
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
    woiid.1 <- reactive(roster.unique$woi.id[match(input$player1, roster.unique$firstlast)])

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

    ## Recalculation of everything.
    
    total.shots <- reactive({
        load("../common-data/conditions.RData")
        
        filter (conditions,
                scorediffcat %in% split.score.list[[match(input$scorechoice, split.score.choices)]],
                gamestate %in% split.man.list[[match(input$manchoice, split.man.choices)]],
                home %in% homeaway.list[[match(input$homechoice, homeaway.choices)]]) %>%
            summarize(S1=sum(shots.1+goals.1), S2=sum(shots.2+goals.2), S3=sum(shots.3+goals.3+shots.4+goals.4)) %>% unlist

    })

    
    
    queried.data <- reactive (
        oneplayer.query (input$player1,
                         playoffs = input$playoffs %in% c("All", "Playoffs"),
                         regseason = input$playoffs %in% c("All", "Regular"),
                         daterange=input$daterange,
                         my.gamestate = split.man.list[[match(input$manchoice, split.man.choices)]],
                         my.scorediffcat = split.score.list[[match(input$scorechoice, split.score.choices)]],
                             my.home = homeaway.list[[match(input$homechoice, homeaway.choices)]]
                             )
        )
    queried.data.comp <- reactive (
        if (length(input$player2) > 0)
        lapply (input$player2, function(name)
                oneplayer.query (name,
                                 playoffs = input$playoffs %in% c("All", "Playoffs"),
                                 regseason = input$playoffs %in% c("All", "Regular"),
                                 daterange=input$daterange,
                                 my.gamestate = split.man.list[[match(input$manchoice, split.man.choices)]],
                                 my.scorediffcat = split.score.list[[match(input$scorechoice, split.score.choices)]],
                                 my.home = homeaway.list[[match(input$homechoice, homeaway.choices)]]
                                     )
                )
        )

    
    ## For making seasonal everything.
    season.data <- reactive ({
        qd <- queried.data()
        ##print(substr(qd$gcode,1,1))
        addon <- rep("", nrow(qd)); addon[substr(qd$gcode,1,1)=="3"] <- "-Playoffs"
        qd$season <- paste0(qd$season, addon)
        out <- compress.goalietable.season (qd)
        out
    })
  
    gamedata <- reactive ({
        qd <- queried.data()
        big.out <- compress.goalietable.date (qd)
        big.out$Date <- gamestest$date[match(paste0(big.out$season, big.out$gcode), paste0(gamestest$season, gamestest$gcode))]
        big.out
    })
  
    gamedata.comp <- reactive ({
        qdc <- queried.data.comp()
        obs <- lapply(qdc, function(qd) {
            big.out <- compress.goalietable.date (qd)
            big.out$Date <- gamestest$date[match(paste0(big.out$season, big.out$gcode), paste0(gamestest$season, gamestest$gcode))]
            big.out <- refine.data(big.out, total.shots())
            big.out <- big.out[order(big.out$Date),]
            big.out
        })
        return(obs)
    })


    
    seasontable.data <- reactive ({
    
        out <- as.data.frame(season.data())
        out <- refine.data(out, total.shots())
        
        print(paste(out$Name, out$season))
        
        rowmatch <- match (paste0(out$ID, out$season),
                           paste0(contracts$woiid, contracts$Year-1, contracts$Year))
        out$Salary <- sprintf("%.3f", contracts$TotalComp[rowmatch]/1E6)
        out$AAV <- sprintf("%.3f", contracts$AAV[rowmatch]/1E6)
 
        return(out[rev(order(out$season)),])
        
    })

 
    output$mytable = renderDataTable({
        out <- seasontable.data()
        out <- out[,c("Name","season","Salary","AAV","Team","Gm","Sv%","AdSv%","SA60",
                      "G","S","Sh","Sv%L","Sv%M","Sv%H",
                      "G.L","S.L","G.M","S.M","G.H","S.H")]
        out$Team <- teamtime.url(out$Team)
        colnames(out) <- augment.html.div (colnames(out))

        out
        
    }, options = list(searching = FALSE,
           scrollX = "100%", scrollY = "100%",
           lengthChange = FALSE, pageLength=20)
        )
    
    output$downloadSData <- downloadHandler (
        filename = paste0("war-on-ice-", Sys.time(),".csv"),
        content = function (file) write.csv (seasontable.data(), file)
        )
    


  
    gametable.data <- reactive ({
        out <- as.data.frame(gamedata())
        out <- refine.data(out, total.shots())
        out <- out[rev(order(out$Date)),]
        out[,unique(names(out))]
    })
    gametable.data.for.table <- reactive ({

        out <- gametable.data()
        out$Date <-woigame.url (out$Date, paste0(out$season, out$gcode)) 
        out$Team <- teamtime.url(out$Team)
        out$Opp <- teamtime.url(out$Opp)
        out
    })

      
    output$mytable.game = renderDataTable({
        out <- gametable.data.for.table()
        out <- out[,c("Name","Team","Opp","Date","Sv%","AdSv%","SA60",
                      "G","S","Sh","Sv%L","Sv%M","Sv%H",
                      "G.L","S.L","G.M","S.M","G.H","S.H")]

        colnames(out) <- augment.html.div (colnames(out))
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
                                              out2$Opp <- un.url(out2$Opp)
                                              out2[order(out2$Date),]}, file))
  
  
    output$scatter = renderPlot ({
        out <- seasontable.data()
        out <- out[order(out$season),]
        
        myplan <- layout (matrix(c(2,1), nrow=1, ncol=2),
                          widths=c(1, 6.5), heights=c(6))

        print(c(onload.xaxis(), onload.yaxis(), onload.caxis(), onload.saxis()))
        print(c(input$xaxis, input$yaxis, input$caxis, input$saxis))
        
        pieces <- goalie.variables[match(c(input$xaxis, input$yaxis, input$caxis, input$saxis),
                                         goalie.variables[,2]), 1]
        print(pieces)
        
    ## Main plot.
        legend.length <- 25
        colprimer = out[,pieces[2]]
        this.col <- crp(c("#FF8888","white","#8888FF"),(colprimer - min(colprimer))/(max(colprimer) - min(colprimer)))
        d.range <- seq(min(colprimer), max(colprimer), length=legend.length)
    
        this.cex <- 15*(out[,pieces[4]]/max(out[,pieces[4]]))
        xvar <- if (input$xaxis == "Time") 1:length(out[,"season"]) else out[,pieces[1]]
        
        plot (xvar, out[,pieces[2]], xlab=input$xaxis, ylab=input$yaxis, xaxt='n',
              cex=this.cex, col=this.col, pch=16, main=paste(input$player1, input$yaxis, "vs.", input$xaxis))
        points (xvar, out[,pieces[2]], cex=this.cex)
        if (input$xaxis == "Time") {
            axis(1, at = 1:length(out[,"season"]), labels = fixSeasons(out[,"season"]))
            lines (1:length(out[,"season"]), out[, pieces[2]])
        } else axis(1)
        
        text (xvar, 
              out[,pieces[2]], out[,"season"])
        legend ("bottomright", "war-on-ice.com", bty="n", cex=0.8)
        
        ## Legend.
        par(mar=c(3,3,3,0))
        xlim1 <- 16*c(-0.5, 0.5)/7/1.7
        plot (rep(0, legend.length), d.range,
              xlim=xlim1, axes=FALSE, cex=4, main=input$caxis, cex.main=0.8,
              pch=16, col=crp(c("#FF8888","white","#8888FF"), seq(0, 1, length=legend.length)))
        points (rep(0, legend.length), d.range, cex=4)
        axis(2)
        
    })
  
  
  
    output$careerbygame = renderPlot ({
        
        par(mfrow = c(2,1))
        acd <- function(xx) as.Date(as.character(xx))
        out <- gametable.data() #subset(season.data(), team == input$teamsearch)
        out <- out[order(out$Date),]
        out2 <- lapply(gamedata.comp(), function(thispl) thispl[order(thispl$Date),])
        data.pieces <- c(list(out), out2)
        
        matchpiece <- goalie.variables[match(input$xaxis.g, goalie.variables[,2]), 1]
        matchpiece.2 <- if (input$yaxis.g == "(none)") NULL else goalie.variables[match(input$yaxis.g, goalie.variables[,2]), 1]
        
        density.pieces <- lapply(data.pieces, function(dd) if (input$yaxis.g == "(none)") {
            if (matchpiece %in% c("Sv%", "AdSv%", "Sv%L", "Sv%M", "Sv%H")) {
                density(dd[[matchpiece]], na.rm = T, from = 0, to = 100)
            } else if (matchpiece %in% c("S.L", "S.M", "S.H", "G.L", "G.M", "G.H")) {
                density(dd[[matchpiece]], na.rm = T, from = 0)
            } else {
                density(dd[[matchpiece]], na.rm = T)
            }
        } else {
            bits <- cbind(dd[[matchpiece]], dd[[matchpiece.2]])
            bkde2D (bits[complete.cases(bits),], 50)
        })


        
        ###########################################################################
        ## Rolling mean pieces.

        rolling.mean.pieces <- lapply(data.pieces, function(dd)
                                      rolling.mean(dd[[matchpiece]],
                                                   count = input$windowsize))# else out[[input$yaxis]]
        main <- if (length(input$player2) > 0) {
            paste (input$player1, "vs", paste(input$player2,collapse=","), "over time")
        } else { paste (input$player1, "over time")}

        
        plot (range(acd(out$Date), na.rm=TRUE),
              range(unlist (rolling.mean.pieces), na.rm=TRUE), 
              xlim = range(acd(out$Date), na.rm=TRUE),
              ylim = range(unlist (rolling.mean.pieces), na.rm=TRUE), 
              xaxt="n",
              ty="n", xlab="Date", ylab=matchpiece, main=main)
        
        date.diff <- max(acd(out$Date)) - min(acd(out$Date))
        axis.Date (side=1, at = seq (min(acd(out$Date)), max(acd(out$Date)),
                               if (date.diff > 730) "years" else "months"))


        abline (h=seq(-300,300,by=10), col=8, lwd=2, lty=2)

        ## The actual lines.
        this.plot <- lapply(1:length(rolling.mean.pieces), function(ii) {
            bad <- which(is.na(rolling.mean.pieces[[ii]]))
            lines (acd(data.pieces[[ii]]$Date[-bad]), rolling.mean.pieces[[ii]][-bad], col=ii, lwd=3)
        })
               
        ## Season gaps.
        for (kk in 1:nrow(season.breaks)) rect (season.breaks[kk,1], -1000, season.breaks[kk,2], 1000, col="#EEEEEE", border="#EEEEEE")

        
        ## Legends.
        legend ("bottomright", "war-on-ice.com", bty="n", cex=1)
        legend ("topleft", c(input$player1, input$player2), col=1:(1+length(input$player2)), lwd=12)
        


        ####################################################################
        ## Density plot 1.
         #       ignore.me <- function () {
        if (input$yaxis.g == "(none)") {
            ## One-dimensional density plot.
            
            range.x <- range(sapply(density.pieces, function(oo) oo$x), na.rm=TRUE)
            range.y <- range(sapply(density.pieces, function(oo) oo$y), na.rm=TRUE)
            
            main2 <- if (length(input$player2) > 0) paste ("Distribution of Game-by-Game", matchpiece, "\n", input$player1, "vs.", paste(input$player2, collapse=",")) else paste ("Distribution of", input$xaxis.g, "for", input$player1)
            
            plot (range.x, range.y, ty="n", xlab=paste("Game-by-Game", matchpiece),
                  ylab="Density", main=main2)

            this.plot <- lapply(1:length(rolling.mean.pieces), function(ii) {
                bad <- which(is.na(rolling.mean.pieces[[ii]]))
                lines (acd(data.pieces[[ii]]$Date[-bad]), rolling.mean.pieces[[ii]][-bad], col=ii, lwd=3)
                abline (v=mean(data.pieces[[ii]][,matchpiece], na.rm = TRUE), col=ii, lty = 3, lwd = 3)
            })
                        
            for (ii in 1:length(density.pieces)) lines (density.pieces[[ii]], col=ii, lwd=6)   
            
            ## Legends.
            legend ("bottomright", "war-on-ice.com", bty="n", cex=1)
            
        } else {

            d1 <- cbind(out[,matchpiece], out[,matchpiece.2])
            d1 <- d1[complete.cases(d1),]
            
            main2 <- if (length(input$player2) > 0) paste ("Distribution of Game-by-Game", matchpiece.2, "vs", matchpiece, "\n", input$player1, "vs.", paste(input$player2, collapse=",")) else paste ("Distribution of", matchpiece.2, "vs", matchpiece, "for", input$player1)
      
            range.x <- range(sapply(data.pieces, function(oo) oo[[matchpiece]]), na.rm=TRUE)
            range.y <- range(sapply(data.pieces, function(oo) oo[[matchpiece.2]]), na.rm=TRUE)

            plot (data.pieces[[1]][,matchpiece], data.pieces[[1]][,matchpiece.2], main=main,
                  xlab=matchpiece, ylab=matchpiece.2, xlim=range.x, ylim=range.y, pch=19)

            if (length(input$player2) > 0) d1 <- lapply(1:(length(density.pieces)-1), function(ii) {
                print(head(data.pieces[[ii+1]]))
                print(data.pieces[[ii+1]][,matchpiece])
                print(data.pieces[[ii+1]][,matchpiece.2])
                
                points (data.pieces[[ii+1]][[matchpiece]], 
                        data.pieces[[ii+1]][[matchpiece.2]],
                        col=ii+1, pch=19)
            })
            for (ii in 1:(length(density.pieces))) {
                contour (density.pieces[[ii]]$x1, density.pieces[[ii]]$x2, density.pieces[[ii]]$fhat, col=ii, nlevels=5, add=TRUE)
            }

            ## Legends.
            legend ("bottomright", "war-on-ice.com", bty="n", cex=1)

        }

    
        
    })
  
  
  # output$Errorchecker <- renderPrint ({
  #     input$daterange
  # })
  
})

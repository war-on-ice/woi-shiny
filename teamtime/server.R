## team-time server
## acthomas, 9-3-14

#library(shiny)
#load("season1314.RData")
source("global.R")

##sqlite.link <- "~/Documents/nhlr/war-on-ice.com/common-data/waronice.sqlite"
sqlite.link <- "../common-data/waronice.sqlite"

team.query <- function (team.name,
                        playoffs=TRUE,
                        regseason=TRUE,
                        daterange=c("2002-10-01",as.character(Sys.Date())),
                        period.set = 0,
                        man.factor = split.man.list[[5]],
                        score.factor = split.score.list[[1]],
                        homechoice = "All") {
    ## playoffs=TRUE; daterange=c("2013-10-01","2014-04-01"); my.gamestate = split.man.list[[3]];  my.scorediffcat = split.score.list[[8]];  my.home = homeaway.list[[1]]
##    woi.id <- roster.unique$woi.id[match(player.name, roster.unique$firstlast)]

    my.home <- if (homechoice == "Away") 0 else if (homechoice == "Home") 1 else c(0,1)
    message (team.name," gm ",man.factor," sd ",score.factor," hm ",my.home)
    
    statement <- paste0('SELECT * FROM teamrun WHERE scorediffcat = ',       ##ID = "', woi.id, '" AND 
                        score.factor,
                        ' AND Team = "',
                        team.name,
                        '" AND gamestate = ',
                        man.factor,
                        ' AND period = ',period.set,' AND Date >= "', daterange[1],
                        '" AND Date <= "', daterange[2],'"',
                        collapse="")
    if (length(my.home) == 1) statement <- paste (statement, " AND home = ", my.home, collapse="")

    con <- dbConnect(SQLite(), dbname=sqlite.link)
    primer <- dbSendQuery(con, statement)
    this.subset <- fetch(primer, n = -1)
    dbDisconnect(con)

    print(head(this.subset,2))
    
    this.subset <- mutate (this.subset,## Name = roster.unique$firstlast[match(ID, roster.unique$woi.id)],
                label = as.character(paste0(season,"-",substr(gcode,1,1)))) ##%>% rename (thisdate = Date)
    
    if (!playoffs) this.subset <- subset(this.subset, substr(gcode,1,1)==2)
    if (!regseason) this.subset <- subset(this.subset, substr(gcode,1,1)==3)
    
    return (this.subset)
    
}


load.team <- function (teamname, ...) {
    allteams <- unlist(regmatches (teamname, gregexpr("[A-Z\\.]{3}", teamname)))
    loaded.data <- rbind_all(lapply (allteams, team.query, ...))
    return(loaded.data)
}


shinyServer(function(input, output, session){
  #input=list(daterange=c("2013-10-01","2014-10-01"), minTOI=60, whichPosLoc="All")
  
    GETterms <- reactive({g1 <- split.GET.to.frame(session$clientData$url_search); g1})
  
  ## Retrieve GET options and replace them into the code if they exist.
  
    onload.a <- reactive (if (!is.null(GETterms()$mansit) && (GETterms()$mansit %in% 1:length(split.man.choices)))
                          split.man.choices[as.numeric(GETterms()$mansit)] else "Even Strength 5v5")
    output$onload.1 <- renderUI(selectInput(inputId="manchoice", label="Team Strengths", selected = onload.a(), choices=split.man.choices))
    
    onload.b <- reactive (if (!is.null(GETterms()$scoresit) && (GETterms()$scoresit %in% 1:length(split.score.choices)))
                          split.score.choices[as.numeric(GETterms()$scoresit)] else "All")
    output$onload.2 <- renderUI(selectInput(inputId="scorechoice", label="Score Situation", selected = onload.b(), choices=split.score.choices))
    
    onload.c <- reactive (if (!is.null(GETterms()$homeawaysit) && (GETterms()$homeawaysit %in% 1:length(homeaway.choices)))
                          homeaway.choices[as.numeric(GETterms()$homeawaysit)] else "All")
    output$onload.3 <- renderUI(selectInput(inputId="homechoice", label="Home/Away Situation", selected = onload.c(), choices=homeaway.choices))
    
    onload.d <- reactive (if (!is.null(GETterms()$team) && any(grepl(GETterms()$team, team.options)))
                          grep(GETterms()$team, team.options, value=TRUE) else "BOS")##sample(team.options, 1))
    output$onload.4 <- renderUI(selectInput (inputId="teamsearch", label="Select Team",
                                             selected = onload.d(), choices = team.options))
    
    onload.tablegroup <- reactive (if (!is.null(GETterms()$tablegroup) &&
                                       (GETterms()$tablegroup %in% 1:length(screening.choices)))
                                   screening.choices[as.numeric(GETterms()$tablegroup)] else "Prime")
    output$onload.5 <-
        renderUI(selectInput(inputId="whichPosLoc", label="Columns To Display",
                             selected = onload.tablegroup(), choices=screening.choices))
    


    
    onload.f <- reactive (if (!is.null(GETterms()$secondteam) && any(grepl(GETterms()$team, team.options))) #reactive("NYR")#
                          grep(GETterms()$secondteam, team.options, value=TRUE) else NULL)
    output$onload.secondteam <- renderUI(selectInput(inputId="secondteam", label="Comparison Team",
                                                     selected = onload.f(),
                                                     choices=c(team.options), multiple=TRUE) )
    
    onload.g <- reactive (if (!is.null(GETterms()$windowsize) && (GETterms()$windowsize %in% 1:40))
                          as.numeric(GETterms()$windowsize) else 25)
    output$onload.windowsize <- renderUI(sliderInput(inputId="windowsize",
                                                     "Number of Games in Moving Average",
                                                     min = 1, max = 50, value = onload.g()))

    
    onload.playoffs <-
        reactive (if (!is.null(GETterms()$playoffs) && (GETterms()$playoffs %in% session.choices))
                  GETterms()$playoffs else session.choices[1])
    output$onload.playoffs <- renderUI(selectInput(inputId="playoffs",
                                                   label="Regular/Playoffs",
                                                   choices = session.choices,
                                                   selected = onload.playoffs()))


    team.3.variables <- reactive(if (input$legacynames) team.variables[,1:3] else team.variables[,c(4,5,3)])
    team.short.match <- reactive(if (input$legacynames) team.variables[,c(1,1)] else team.variables[,c(1,4)])
    team.plot.subset <- reactive({
        out <- team.3.variables()[-(1:5),]; print(head(out)); out
    })
    team.var.choices <- reactive(team.plot.subset()[,2])


    onload.e1 <- reactive (if (!is.null(GETterms()$xaxis) && (GETterms()$xaxis %in% 1:length(team.var.choices())))
                           as.numeric(GETterms()$xaxis) else which(team.variables[,2] == "Fenwick For Percentage of Total"))
    output$onload.xaxis <- renderUI(selectInput(inputId="xaxis.g", label="Primary Variable",
                                                selected = team.var.choices()[onload.e1()], choices=team.var.choices()) )
    
    onload.e <- reactive (if (!is.null(GETterms()$yaxis) &&  (GETterms()$yaxis %in% 0:length(team.var.choices())))
                          as.numeric(GETterms()$yaxis) else 0)
    output$onload.yaxis <- renderUI(selectInput(inputId="yaxis.g", label="Secondary Variable",
                                                selected = c("(none)", team.var.choices())[onload.e()+1],
                                                choices=c("(none)", team.var.choices())))


    
    
    onload.yx <- reactive (if (!is.null(GETterms()$xaxiss) && (GETterms()$xaxiss %in% 1:length(team.var.choices())))
                           as.numeric(GETterms()$xaxiss) else 0)
    output$onload.xaxis.s <- renderUI(selectInput(inputId="xaxiss", label="X Axis Variable",
                                                  selected = c("Time", team.var.choices())[onload.yx()+1],
                                                  choices=c("Time", team.var.choices())))
    onload.ya <- reactive (if (!is.null(GETterms()$yaxiss) && (GETterms()$yaxiss %in% 1:length(team.var.choices())))
                           as.numeric(GETterms()$yaxiss) else which(team.variables[,2] == "Fenwick For Percentage of Total"))
    output$onload.yaxis.s <- renderUI(selectInput(inputId="yaxiss", label="Y Axis Variable",
                                                  selected = team.var.choices()[onload.ya()], choices=team.var.choices()) )
    
    onload.yb <- reactive (if (!is.null(GETterms()$colors) && (GETterms()$colors %in% 1:length(team.var.choices())))
                           as.numeric(GETterms()$colors) else which(team.variables[,2] == "PDO (On-Ice SvPct plus On-Ice ShPct)"))
    output$onload.color.s <- renderUI(selectInput(inputId="colors", label="Color Variable",
                                                  selected = team.var.choices()[onload.yb()], choices=team.var.choices()) )
    
    onload.yc <- reactive (if (!is.null(GETterms()$sizes) && (GETterms()$sizes %in% 1:length(team.var.choices())))
                           as.numeric(GETterms()$yaxiss) else which(team.variables[,2] == "On-Ice Unblocked Shot Attempts On Goal"))
    output$onload.size.s <- renderUI(selectInput(inputId="sizes", label="Size Variable",
                                                 selected = team.var.choices()[onload.yc()], choices=team.var.choices()) )
    
    tab.choices <- c("Tabular By Game", "Graphical By Game",
                     "Tabular By Season", "Graphical By Season")
    onload.tabpanelchoice <- reactive (if (!is.null(GETterms()$tab) &&
                                           (GETterms()$tab %in% 1:length(tab.choices)))
                                       tab.choices[as.numeric(GETterms()$tab)] else tab.choices[1])
    
    output$onload.tabset <- renderUI(
        tabsetPanel(id="tabset",
                    
                    tabPanel("Tabular By Game",
                             dataTableOutput('gametable')
                             ),
                    tabPanel("Graphical By Game",
                             fluidRow(
                                 column(3, htmlOutput ("onload.xaxis")),
                                 column(3, htmlOutput ("onload.yaxis")),
                                 column(3, htmlOutput ("onload.secondteam")),
                                 column(3, htmlOutput ("onload.windowsize"))),
                             plotOutput(outputId = "timeline", height="900px", width="920px")
                             ),
                    
                    tabPanel("Tabular By Season",
                             dataTableOutput('teamtable')),
                    tabPanel("Graphical By Season",
                             fluidRow(
                                 column(3, htmlOutput ("onload.xaxis.s")),
                                 column(3, htmlOutput ("onload.yaxis.s")),
                                 column(3, htmlOutput ("onload.color.s")),
                                 column(3, htmlOutput ("onload.size.s"))
                                 ),
                             plotOutput(outputId = "scatter.s", height="720px", width="920px")),
                    selected = onload.tabpanelchoice()
                    ) #tabset
        )
    
    outdest <- reactive(paste0 ("http://war-on-ice.com/teambygame.html?",
                                "mansit=",which(input$manchoice == split.man.choices),
                                "&scoresit=",which(input$scorechoice == split.score.choices),
                                "&homeawaysit=",which(input$homechoice == homeaway.choices),
                                "&team=",input$teamsearch,
                                "&secondteam=",input$secondteam,
                                "&xaxis=",which(input$xaxis.g == team.var.choices()),
                                "&yaxis=",which(input$yaxis.g == team.var.choices()),
                                "&windowsize=",input$windowsize,
                                "&yaxiss=",which(input$yaxiss == team.var.choices()),
                                "&colors=",which(input$colors == team.var.choices()),
                                "&sizes=",which(input$sizes == team.var.choices()),
                                "&panel=",which(input$tabset == tab.choices),
                                "&tablegroup=",which(input$whichPosLoc == screening.choices)
                                ))
    
                                        #output$twittershare <- renderUI(HTML(
                                        #    paste("<a href=\"https://twitter.com/share\" class=\"twitter-share-button\" data-url=\"",outdest(),"\" data-text=\"Share a war-on-ice plot\" data-via=\"war_on_ice\">Tweet</a>
  #    <script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+'://platform.twitter.com/widgets.js';fjs.parentNode.insertBefore(js,fjs);}}(document, 'script', 'twitter-wjs');</script>")))
  
  #    <a href="https://twitter.com/share" class="twitter-share-button" data-via="war_on_ice">Tweet This Page</a>
  #        <script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+'://platform.twitter.com/widgets.js';fjs.parentNode.insertBefore(js,fjs);}}(document, 'script', 'twitter-wjs');</script>
  
    output$shareDestination <- renderText({ 
        outdest()
    })
    
    fullteamdata <- reactive(lapply(c(input$teamsearch, input$secondteam), function(tm)
                                    load.team(tm,
                                              playoffs = input$playoffs %in% c("All", "Playoffs"),
                                              regseason = input$playoffs %in% c("All", "Regular"),
                                              score.factor=split.score.list[[match(input$scorechoice, split.score.choices)]],
                                              man.factor=split.man.list[[match(input$manchoice, split.man.choices)]],
                                              daterange=input$daterange,
                                              period.set=0,
                                              homechoice=input$homechoice
                                              ##, playoffs=input$playoffs
                                              )))

    
    
    
    teamdata <- reactive(lapply(fullteamdata(), function(ff) {
        ##print(head(ff,2))
        ff %>% ##filter(
            ##Date >= as.Date(input$daterange[1]),
            ##Date <= as.Date(input$daterange[2])
##            , score.diff.cat %in%, gamestate %in% , home %in% homeaway.list[[match(input$homechoice, homeaway.choices)]]
##            ) %>%
                                swapnames 
    }))

    supergamedata <- reactive (lapply(teamdata(), function(tm) {
        out <- tm %>%
            group_by (Team, season, gcode, Date, Opponent) %>%
                compress.team %>% add.rates.teams #%>% arrange(desc(Date))
        ##message ("Calling supergamedata")
        ##print(head(out,2))
        out$Date <- as.Date(out$Date)
        out <- out[order(out$Date),]
        ##message("HiO")
        out
    }))

    gametable.prime <- reactive (supergamedata()[[1]])
    
    output$gametable = renderDataTable({
        outtable <- pick.columns(gametable.prime(),
                                 data.frame(Date=gametable.prime()$Date,
                                            Opponent=gametable.prime()$Opponent),
                                 group=input$whichPosLoc)
        #message("Hi")
        Date1 <- outtable$Date
        outtable$Date <- woigame.url(outtable$Date, paste0(gametable.prime()$season, gametable.prime()$gcode))   #gsub(" -Playoffs", "", 
        outtable <- outtable[rev(order(Date1)),]


        outtable$Opponent <- teamtime.url(outtable$Opponent)


        
        colnames(outtable) <- swapout.oldnew (colnames(outtable), team.short.match()) %>% augment.html.div (team.3.variables())

        ##augment.html.div (colnames(outtable), team.plot.variables)
        ##message("Hi2")
        ##print(head(outtable,2))
        outtable ##%>% arrange(Date)
    #        out <- fulldata.1(); if (!input$showRawStats) out <- out[,1:11]; out
    } ,options=list(searching=FALSE, lengthChange = FALSE, pageLength=20,
           scrollX = "100%", scrollY = "100%"))
  
    output$downloadGData <- downloadHandler (
        filename = paste0("war-on-ice-", Sys.time(),".csv"),
        content = function (file) write.csv ({out2=supergamedata()[[1]]; out2}, file) )
  
  
  
    ###############################################################################
    ## Season data
  
    superseasondata <- reactive ({
        indata <- teamdata()[[1]]; indata$seasonblock <- paste(indata$season, substr(indata$gcode,1,1), sep="-")
        indata$seasonblock <- gsub ("-2", "", indata$seasonblock)
        indata$seasonblock <- gsub ("-3", "-Playoffs", indata$seasonblock)
        
        indata %>% group_by (Team, seasonblock) %>% compress.team %>% add.rates.teams #%>% arrange(desc(season))
    })
    
    seasondata <- reactive ({
        out <- pick.columns(superseasondata(), data.frame(Season=superseasondata()$seasonblock,
                                                          Gm=superseasondata()$Gm), group=input$whichPosLoc)
        out[rev(order(out$Season)),]
    })
    teamtable.prime <- reactive (seasondata())
    
    output$teamtable = renderDataTable({
        out <- teamtable.prime()
        colnames(out) <- swapout.oldnew (colnames(out), team.short.match()) %>% augment.html.div (team.3.variables())
        ##augment.html.div (colnames(out), team.plot.variables)
        out
    } ,options=list(searching=FALSE, lengthChange = FALSE, pageLength=20,
           scrollX = "100%", scrollY = "100%"))
    
    output$downloadSData <- downloadHandler (
        filename = paste0("war-on-ice-", Sys.time(),".csv"),
        content = function (file) write.csv (teamtable.prime(), file)
        )
    
    
    
    output$timeline = renderPlot ({
        
        par(mfrow = c(2,1))
        teams <- substr(c(input$teamsearch, input$secondteam), 1, 3)

        pieces.1 <- team.3.variables()[match(c(input$xaxis.g, input$yaxis.g), team.3.variables()[,2]), 1]
        pieces.2 <- team.short.match()[match(pieces.1, team.short.match()[,2]), 1]
                
        matchpiece <- pieces.2[1] ##player.plot.variables[match(input$xaxis.g, player.plot.variables[,2]), 1]
        matchpiece.2 <- if (input$yaxis.g == "(none)") NULL else pieces.2[2] ## player.plot.variables[match(input$yaxis.g, player.plot.variables[,2]), 1]

        
        ##matchpiece <- team.plot.variables[match(input$xaxis.g, team.plot.variables[,2]), 1]
        ##matchpiece.2 <- if (input$yaxis.g == "(none)") NULL else team.plot.variables[match(input$yaxis.g, team.plot.variables[,2]), 1]

        
        ###################################################################################
        ## Rolling mean pieces.
        data.pieces <- lapply(supergamedata(), function(tm) tm[order(tm$Date),])
        
        outputs <- lapply(data.pieces, function(tm) rolling.mean(tm[[matchpiece]], count = input$windowsize))
        #print(outputs)
        #print(supergamedata()[[1]]$Date)
        
        main <- if (length(input$secondteam) > 0) {
          paste ("Moving Average of", input$xaxis.g, "for", input$teamsearch, "vs.", paste(input$secondteam,collapse=", "), "over time")
        } else {
          paste ("Moving average of", input$xaxis.g, "for", input$teamsearch, "over time")
        }

        plot (range(as.Date(supergamedata()[[1]]$Date), na.rm=TRUE),
              range(unlist(outputs), na.rm=TRUE),
              ty="n", xlab="Date", ylab=input$xaxis.g,
              xaxt="n",
              main = main)
        date.diff <- max(as.Date(supergamedata()[[1]]$Date)) - min(as.Date(supergamedata()[[1]]$Date))
        axis.Date (side=1, at = seq (min(as.Date(supergamedata()[[1]]$Date)), max(as.Date(supergamedata()[[1]]$Date)), if (date.diff > 730) "years" else "months"))
        
        abline (h=0, col=8)
        if (matchpiece %in% c("ZSO%", "GF%", "FF%", "CF%", "SF%")) abline (h=50, col=8)
        if (matchpiece %in% c("G+/-", "F+/-", "C+/-", "S+/-")) abline (h=0, col=8)
        if (matchpiece %in% c("PDO")) abline (h=100, col=8)
        for (tm in 1:length(outputs)) {
            lines (supergamedata()[[tm]]$Date, outputs[[tm]], col=team.colors$color[match(teams[tm], team.colors$team)], lwd=8)
            lines (supergamedata()[[tm]]$Date, outputs[[tm]], col=team.colors$color2[match(teams[tm], team.colors$team)], lwd=3)
        }
        for (kk in 1:nrow(season.breaks)) rect (season.breaks[kk,1], -1000, season.breaks[kk,2], 1000, col="#EEEEEE", border="#EEEEEE")
        legend ("bottomright", "war-on-ice.com", bty="n", cex=1) ## Legend.

        ##########################################################################################
        ## Density plot.

        densities <- lapply(supergamedata(), function(tm) if (input$yaxis.g == "(none)") { 
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
            main2 <- if (length(input$secondteam) > 0) {
                paste ("Distribution of Game-by-Game", input$xaxis.g, "\n", input$teamsearch, "vs.", paste(input$secondteam,collapse=", "))
            } else {
                paste ("Distribution of", input$xaxis.g, "for", input$teamsearch)
            }
            
            plot (range(unlist(lapply(densities, function(dd) dd$x)), na.rm=TRUE),
                  range(unlist(lapply(densities, function(dd) dd$y)), na.rm=TRUE),
                  ty="n", xlab=paste("Game-by-Game", input$xaxis.g), ylab="Density", main=main2)
            
            if (matchpiece %in% c("ZSO%", "GF%", "FF%", "CF%", "SF%")) abline (h=50, col=8)
            if (matchpiece %in% c("G+/-", "F+/-", "C+/-", "S+/-")) abline (h=0, col=8)
            if (matchpiece %in% c("PDO")) abline (h=100, col=8)
            
            for (tm in 1:length(outputs)) {
                abline (v=mean(supergamedata()[[tm]][[matchpiece]], na.rm = TRUE), col=1, lty = 3, lwd = 3)
            }
            
            for (tm in 1:length(outputs)) {
                lines (densities[[tm]], col=team.colors$color[match(teams[tm], team.colors$team)], lwd=8)
                lines (densities[[tm]], col=team.colors$color2[match(teams[tm], team.colors$team)], lwd=3)
            }

        ## Legends.
            legend ("bottomright", "war-on-ice.com", bty="n", cex=1)
            legend ("topright", c(teams, paste(teams, "average")), 
                    col=team.colors$color2[match(teams, team.colors$team)],
                    text.col=team.colors$color[match(teams, team.colors$team)],
                    lwd=c(rep(12, length(teams)), rep(3, length(teams))),
                    lty = c(rep(1, length(teams)), rep(3, length(teams))))
            
        } else {

            ## First player.
            #d1 <- cbind(data.pieces[[1]][,matchpiece], data.pieces[[1]][,matchpiece.2])
            #d1 <- d1[complete.cases(d1),]
            
            main2 <- if (length(input$secondteam) > 0) paste ("Distribution of Game-by-Game", matchpiece.2, "vs", matchpiece, "\n", input$teamsearch, "vs.", paste(input$secondteam, collapse=",")) else paste ("Distribution of", matchpiece.2, "vs", matchpiece, "for", input$teamsearch)
      
            range.x <- range(sapply(data.pieces, function(oo) oo[[matchpiece]]), na.rm=TRUE)
            range.y <- range(sapply(data.pieces, function(oo) oo[[matchpiece.2]]), na.rm=TRUE)

            ##print(range.x); print(range.y); print(matchpiece); print(matchpiece.2)
            ##print(main2)
            ##print(data.pieces[[1]][,matchpiece])
            ##print(data.pieces[[1]][,matchpiece.2])
            
            plot (data.pieces[[1]][[matchpiece]],
                  data.pieces[[1]][[matchpiece.2]], main=main2,
                  xlab=matchpiece, ylab=matchpiece.2,
                  xlim=range.x, ylim=range.y, pch=19,
                  
                  col=team.colors$color[match(teams[1], team.colors$team)], cex=1.5)
            
            points (data.pieces[[1]][[matchpiece]],
                    data.pieces[[1]][[matchpiece.2]],
                    pch=19,
                    col=team.colors$color2[match(teams[1], team.colors$team)], cex=0.7)
            
            if (length(input$secondteam) > 0) d1 <- lapply(1:(length(densities)-1), function(ii) {
                points (data.pieces[[ii+1]][[matchpiece]], 
                        data.pieces[[ii+1]][[matchpiece.2]],
                        pch=19,
                        col=team.colors$color[match(teams[ii+1], team.colors$team)], cex=1.5)
                points (data.pieces[[ii+1]][[matchpiece]], 
                        data.pieces[[ii+1]][[matchpiece.2]],
                        pch=19,
                        col=team.colors$color2[match(teams[ii+1], team.colors$team)], cex=0.7)
            })
            
            for (ii in 1:(length(densities))) {
                contour (densities[[ii]]$x1,
                         densities[[ii]]$x2,
                         densities[[ii]]$fhat,
                         col=team.colors$color[match(teams[ii], team.colors$team)],
                         nlevels=5, add=TRUE)
            }

            ## Legends.
            legend ("bottomright", "war-on-ice.com", bty="n", cex=1)




        }



        
    })
  


        
    output$scatter.s = renderPlot ({
        myplan <- layout (matrix(c(2,1), nrow=1, ncol=2),
                          widths=c(1, 10), heights=c(6))
        plotter <- superseasondata()
        ##print("plotter")
        ##print(head(plotter))
        ## Main plot.
        legend.length <- 25

        pieces.1 <- team.3.variables()[match(c(input$xaxiss, input$yaxiss, input$colors, input$sizes), team.3.variables()[,2]), 1]
        datacat <- team.3.variables()[match(c(input$xaxiss, input$yaxiss, input$colors, input$sizes), team.3.variables()[,2]), 3]
        pieces <- team.short.match()[match(pieces.1, team.short.match()[,2]), 1]
        print(c(input$xaxiss, input$yaxiss, input$colors, input$sizes))
        print(pieces)
        #pieces <- team.variables[match(c(input$xaxiss, input$yaxiss, input$colors, input$sizes),
        #                                 team.variables[,2]), 1]
        pieces[is.na(pieces)] <- pieces[!is.na(pieces)][1]
        
        ##print(pieces)
        subdata <- plotter[,pieces]

        ###################################### Main plot

        ## Get the color profiles.
        legend.length <- 25
        colprimer = subdata[[3]]
        
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

        print(head(plotter, 1))

        this.cex <- 15*(subdata[[4]]/max(subdata[[4]]))
        xvar <- if (input$xaxiss == "Time") 1:length(plotter[["seasonblock"]]) else subdata[[1]]

        plot (xvar, subdata[[2]],
              xlab=input$xaxiss, ylab=input$yaxiss, xaxt='n',
              cex=this.cex, col=this.col, pch=16,
              main=paste(input$teamsearch, input$yaxiss, "vs.", input$xaxis))
        points (xvar, subdata[[2]], cex=this.cex)
        if (input$xaxiss == "Time") {
            axis(1, at = 1:length(plotter[["seasonblock"]]), labels = plotter[["seasonblock"]])
            lines (xvar, subdata[[2]])  
        } else axis(1)
        text (xvar, subdata[[2]], plotter$seasonblock)

         
        legend ("bottomright", "war-on-ice.com", bty="n", cex=0.8)
        
        ## Legend.
        par(mar=c(3,3,3,0))
        xlim1 <- 16*c(-0.5, 0.5)/7/1.7
        plot (rep(0, legend.length), d.range,
              xlim=xlim1, axes=FALSE, cex=4, main=input$colors, cex.main=0.8,
              pch=16, col=crp(colrange, seq(0, 1, length=legend.length)))
        points (rep(0, legend.length), d.range, cex=4)
        axis(2)
        
    })
  
})

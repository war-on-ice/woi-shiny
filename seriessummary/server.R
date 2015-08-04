## playertable server
## acthomas, 8-20-14
library(shiny)
library(nhlplot)
library(nhlscrapr)

#load("season1314.RData")

source("global.R")

#source("../warbase/R/teamtables.R")
countdown.time <- 120

shinyServer(function(input, output, session) {
  
  ## input <- list(manchoice="All", scorechoice="All", homechoice="All", minTOI=0, datemode=FALSE, playersearch="", teamsearch="", startdate="20132014 Regular", enddate="20132014 Regular", daterange=c("2013-01-01","2014-09-01")); m1=21; m2=21; teampick=""
  
  #autoInvalidate <- reactiveTimer(120*1000, session)
  #    autoInvalidate <- reactiveTimer(games.time()*1000, session)
  
    GETterms <- reactive({g1 <- split.GET.to.frame(session$clientData$url_search); print(g1); g1})
    observe ({
        if (!is.null(GETterms()$mansit) &&
            (GETterms()$mansit %in% 1:length(split.man.choices)))
            updateSelectInput(session, "manchoice", selected=split.man.choices[as.numeric(GETterms()$mansit)])
        
        if (!is.null(GETterms()$scoresit) &&
            (GETterms()$scoresit %in% 1:length(split.score.choices)))
            updateSelectInput(session, "scorechoice", selected=split.score.choices[as.numeric(GETterms()$scoresit)])

        if (!is.null(GETterms()$team1) && any(grepl(GETterms()$team1, teams)))
            updateSelectInput(session, "team1", selected=grep(GETterms()$team1, teams, value=TRUE)[1])
        if (!is.null(GETterms()$team2) && any(grepl(GETterms()$team2, teams)))
            updateSelectInput(session, "team2", selected=grep(GETterms()$team2, teams, value=TRUE)[1])

        if (!is.null(GETterms()$season) && (GETterms()$season %in% seasons))
            updateSelectInput(session, "season", selected=GETterms()$season)
        if (!is.null(GETterms()$psession) &&
            (GETterms()$psession %in% c("Regular/Playoffs","Regular","Playoffs")))
            updateSelectInput(session, "psession", selected=GETterms()$psession)
    })

    
    
    outdest <- reactive(paste0 ("http://war-on-ice.com/series.html?",
                                "team1=",input$team1,
                                "&team2=",input$team2,
                                "&season=",input$season,
                                "&psession=",input$psession,
                                
                                "&mansit=",which(input$manchoice == split.man.choices),
                                "&scoresit=",which(input$scorechoice == split.score.choices)
                                ))
    output$shareDestination <- renderText(outdest())
  

    player.3.variables <- reactive(if (input$legacynames) player.variables[,1:3] else player.variables[,c(4,5,3)])
    player.short.match <- reactive(if (input$legacynames) player.variables[,c(1,1)] else player.variables[,c(1,4)])
    
    team.3.variables <- reactive(if (input$legacynames) team.variables[,1:3] else team.variables[,c(4,5,3)])
    team.short.match <- reactive(if (input$legacynames) team.variables[,c(1,1)] else team.variables[,c(1,4)])

    goalie.3.variables <- reactive(if (input$legacynames) goalie.variables[,1:3] else goalie.variables[,c(4,5,3)])
    goalie.short.match <- reactive(if (input$legacynames) goalie.variables[,c(1,1)] else goalie.variables[,c(1,4)])


  ## Get the data.

    get.game.infos <- reactive ({
        psession <- if (input$psession == "Regular and Playoffs") c("Regular", "Playoffs") else input$psession
        retrieve.gameset (input$season, input$team1, input$team2, psession)
    })
    output$GamesInfo <- renderUI(list(
        h4 (paste("Total Games In Series:", nrow(get.game.infos())))
        ))
    
    roster.and.games <- reactive (list(roster.unique=roster.unique, gamestest=gamestest))
    
    roster.table <- reactive (roster.and.games()$roster.unique)
    games.1 <- reactive (roster.and.games()$gamestest)  
   
    loaded.data <- reactive ({
        psession <- if (input$psession == "Regular and Playoffs") c("Regular", "Playoffs") else input$psession
        message ("Settings: ", input$season, ";", input$team1, ";", input$team2, ";", psession)
        retrieve.series (input$season, input$team1, input$team2, psession)
    })
  
  
    period.options <- reactive (max(loaded.data()$playbyplay$period))
    output$dynamic.period <-
        renderUI (selectInput(inputId="period", label="Period",
                              selected = "All", choices=c("All", paste(1:period.options()))))
    period.set <- reactive(if (input$period == "All") 0 else as.numeric(input$period))
    
    queried.data <- reactive ({
        gamedata <- loaded.data()    
        this.subset <- 
            subset(gamedata$playerrun,
                   score.diff.cat %in% split.score.list[[match(input$scorechoice, split.score.choices)]] &
                   gamestate %in% split.man.list[[match(input$manchoice, split.man.choices)]] &
                   period %in% period.set() )
        playbyplay <- gamedata$playbyplay
        return(list(playbyplay=playbyplay, alldata=this.subset))
    })
  
    ## #####################################################################
    ##
    ## Goalie Data
    
    goaliedata <- reactive ({
        starter <- loaded.data()$goalierun
        ##print(head(starter))
        this.subset <- starter %>%
            filter(score.diff.cat %in% split.score.list[[match(input$scorechoice, split.score.choices)]],
                   gamestate %in% split.man.list[[match(input$manchoice, split.man.choices)]],
                   period %in% period.set()) %>%
                       mutate (Name = roster.unique$firstlast[match(ID, roster.unique$woi.id)])
        ##print(this.subset)
        out <- this.subset %>% fix.column.names %>% compress.goalietable %>% ungroup
        ##print(out)
        out
    })
  
    goalietable.prime <- reactive ({
        out <- goaliedata()
        rownames(out) <- out$ID
        out %>% select (Name, Team, Gm, GU, SU, G.L, S.L, G.M, S.M, G.H, S.H, TOI)
    })
  
    output$goalietable = renderDataTable({
        output <- goalietable.prime()
        output$Team <- teamtime.url(output$Team)
        output$Name <- playerseason.url(output$Name, rownames(output))
        colnames(output) <- augment.html.div(colnames(output), goalie.3.variables())
        output %>% as.data.frame
    } ,options=list(searching=FALSE, paging = FALSE, info=FALSE,
           scrollX = "100%", scrollY = "100%",
           lengthChange = FALSE, pageLength=20)
        )
  

    ## #####################################################################
    ##
    ## Team Data

    teamdata <- reactive ({
        teamrun <- loaded.data()$teamrun
          
        this.subset <- 
            subset(teamrun,
                   score.diff.cat %in% split.score.list[[match(input$scorechoice, split.score.choices)]] &
                   gamestate %in% split.man.list[[match(input$manchoice, split.man.choices)]] &
                   period %in% period.set() )
    
        out <- this.subset %>% fix.column.names %>%
            group_by (Team) %>% compress.team %>% add.rates.teams
    
        out <- out[rev(order(out[["CF%"]])),]
        out
    })
  
  
    teamtable.prime <- reactive ({
    
    ##group <- input$whichPosLoc
        out <- teamdata()
        out2 <- data.frame(Team=out$Team)
        out2 <- cbind(out2, out[,c("GF", "SF", "MSF", "BSF", "CF", "SCF", "HSCF",
                                   "ZSO", "HIT", "PN", "FO_W", "TOI")])
    
    #message("teamfinal"); print(out2)
    ##if (group %in% c("Prime", "All"))   
    ##out2 <- cbind(out2, out[,c("GF","GA","G+/-","CF%","CP60","OFOn%","OSh%","OSv%","FO%","PDO","ZSO%")])
    ##        if (group %in% c("Corsi", "Fenwick", "All")) out2 <- cbind(out2, out[,c("CF","FF","MSF","BSF")])
    ##        if (group %in% c("Shot-Based", "Goal-Based", "All")) out2 <- cbind(out2, out[,c("SF%","S+/-", "SF60","SA60", "SF","SA","GF60", "GA60","GF%", "OSh%", "OFenSh%", "OSv%","OCOn%", "OFOn%")])
    ##        if (group %in% c("Faceoffs", "Base Counts", "All")) out2 <- cbind(out2, out[,c("FO%", "FO_W", "FO_L", "ZSO","ZSN","ZSD", "HIT", "HIT-","PN", "PN-", "TOI")])
    
        numeric.columns <- sapply(out2, class) 
        out2[,numeric.columns=="numeric"] <- round(out2[,numeric.columns=="numeric"], 1)
    
        out2##[,unique(names(out2))]
    
    })
  
    output$teamtable = renderDataTable({
        output <- teamtable.prime()
        output$Team <- teamtime.url(output$Team)
        colnames(output) <- swapout.oldnew (colnames(output), team.short.match()) %>%
            augment.html.div (team.3.variables())
        output
    }       
        ,options=list(searching=FALSE, paging = FALSE, info=FALSE,
             scrollX = "100%", scrollY = "100%",
             lengthChange = FALSE, pageLength=20)
        )
  
  
  
  ## ########################################################################
  ## Player tables for this game.
  
    compiled.table.data <- reactive ({
    
    #out <- compress.game (queried.data()$alldata)
        out <- queried.data()$alldata %>% swapnames %>%
      #make.count.adjustments.player (do.rink=input$rinkbias,
      #                               do.home=input$fixhomead,
      #                               do.scorestate=input$scoread) %>%
            group_by(ID) %>% compress.player.game %>% add.rates.players
    
        out$Name <- roster.table()$firstlast[match(out$ID, roster.table()$woi.id)] #player.id)]
        out$Pos <- roster.table()$pos[match(out$ID, roster.table()$woi.id)]
    
        ##print(head(out,3))
        out <- out[rev(order(out$home, out[["C+/-"]])),]
        ##out$TOI <- round(out$TOI, 1)
    
        return(out)
    })
  
    mytable.prime <- reactive ({
    
        out <- compiled.table.data()
        ##print(head(out))
        out2 <- data.frame(Name=gsub(" ",".",out$Name),
                           Pos=out$Pos,
                           Team=out$Team,
                           Gm=out$Gm,
                           G=out$G, A1=out$A1, A2=out$A2,
                           P=out$G + out$A1 + out$A2)
        rownames(out2) <- out$ID
        group <- input$whichPosLoc
        
        if (group %in% c("Prime", "All"))
            out2 <- cbind(out2, out[,c("iHSC", "iSC","iCF", "C+/-", "F+/-", "G+/-", "CF", "FF",
                                       "ZSO", "ZSD"
                                       ,"AB","FO_W","FO_L", "HIT","HIT-",
                                       "PN","PN-","TOI")])
        if (group %in% c("Rates", "All"))
            out2 <- cbind(out2, out[,c("CF60","CA60","CP60", "FF60","FA60","FP60", "SF60","SA60"
                                       ##,"TOIC%","TOIT%","FenC%","FenT%","CorC%","CorT%"
                                       )])
        if (group %in% c("Counts", "All"))
            out2 <- cbind(out2, out[,c("BK","AB","MS","SH","SF","SA","GF","GA",
                                       "FF","FA","CF","CA","SCF","SCA")])
    
        numeric.columns <- sapply(out2, class) 
        out2[,numeric.columns=="numeric"] <- round(out2[,numeric.columns=="numeric"], 1)
    
        out2
    })
  
    output$awaytable = renderDataTable({
        out <- subset(mytable.prime(), Team == input$team1)
        out$Name <- playerseason.url(out$Name, rownames(out))
        out <- out[,which(colnames(out) != "home")]

        colnames(out) <- swapout.oldnew (colnames(out), player.short.match()) %>% augment.html.div (player.3.variables())
        ##colnames(out) <- augment.html.div(colnames(out), player.variables)
        
        out
    }, options = list(searching = FALSE, paging = FALSE, info=FALSE,
           scrollX = "100%", scrollY = "100%",
           lengthChange = FALSE, pageLength=20)
        )
    output$hometable = renderDataTable({
        out <- subset(mytable.prime(), Team == input$team2)
        out$Name <- playerseason.url(out$Name, rownames(out))
        out <- out[,which(colnames(out) != "home")]
        ##colnames(out) <- augment.html.div(colnames(out), player.variables)
        colnames(out) <- swapout.oldnew (colnames(out), player.short.match()) %>% augment.html.div (player.3.variables())

        out
    }, options = list(searching = FALSE, paging = FALSE, info=FALSE,
           scrollX = "100%", scrollY = "100%",
           lengthChange = FALSE, pageLength=20)
        )
  
  
  
    output$shiftchart = renderPlot ({
        make.shift.chart(loaded.data()$playbyplay, roster.table())
    })
  
  ## precalculated!
    output$headtohead = renderPlot ({
        time.together.plot(loaded.data()$playbyplay,
                           loaded.data()$coplayer,
                           rosters=roster.table(),
                           usecolors=input$showcolors)
    })
    output$toih2h = renderPlot ({
        time.together.plot(loaded.data()$playbyplay,
                           loaded.data()$coplayer,
                           rosters=roster.table(),
                           just.numbers=TRUE,
                           just.ES = input$manchoice=="Even Strength 5v5")
    })
  
  
  
    output$newSHAC <- renderUI(HTML(shot.count.d3(queried.data()$alldata %>% group_by(ID) %>% compress.player.game %>% ungroup %>% rename(TOI=TOI.on),
                                                  input$team1, input$team2)))

    output$toiMatchups <- renderUI(HTML(time.together.d3(loaded.data()$playbyplay,
                                                         ##loaded.data()$coplayer,
                                                         rosters=roster.table(),
                                                         just.numbers=TRUE,
                                                         just.ES = input$manchoice=="Even Strength 5v5")))
  

    output$winprobLink <- renderUI(tags$iframe(src=paste0("http://biscuit.war-on-ice.com/rinkstats/?seasongcode=", input$seasongcode), seamless=TRUE, height=720, width=960))
##        shot.count.d3(queried.data()$alldata)))
    
    reduced.pbp <- reactive ({
        
        this.table <- queried.data()$playbyplay
        if (period.set() > 0) this.table <- filter (this.table, period %in% period.set())
        
        if (substr(this.table$gcode[1], 1, 1) == "2") this.table <- subset(this.table, period <= 4)
        
        this.table$score.diff.cat <- with(this.table,  ## Just upped to 3 goal splits.
                                          0*(home.score - away.score <= -3) +
                                          1*(home.score - away.score == -2) +
                                          2*(home.score - away.score == -1) +
                                          3*(home.score - away.score == 0) +
                                          4*(home.score - away.score == 1) +
                                          5*(home.score - away.score == 2) +
                                          6*(home.score - away.score >= 3))
        this.table$score.diff.cat[this.table$ev.team == this.table$awayteam] <-
            6 - this.table$score.diff.cat[this.table$ev.team == this.table$awayteam]
        
        
        this.table$gamestate <-
            with(this.table,
                 1*(home.skaters == 6 & away.skaters == 6 & home.G > 1 & away.G > 1) +
                 2*(home.skaters > away.skaters & away.skaters > 2 & home.G > 1 & away.G > 1 & ev.team==hometeam) +
                 3*(home.skaters > away.skaters & away.skaters > 2 & home.G > 1 & away.G > 1 & ev.team!=hometeam) +
                 
                 3*(home.skaters < away.skaters & home.skaters > 2 & home.G > 1 & away.G > 1 & ev.team==hometeam) +
                 2*(home.skaters < away.skaters & home.skaters > 2 & home.G > 1 & away.G > 1 & ev.team!=hometeam) +
                 
                 4*(home.skaters == 5 & away.skaters == 5 & home.G > 1 & away.G > 1) +
                 
                 5*(home.skaters > 2 & away.skaters > 2 & home.G > 1 & away.G == 1 & ev.team==hometeam) +
                 6*(home.skaters > 2 & away.skaters > 2 & home.G > 1 & away.G == 1 & ev.team!=hometeam) +
                 6*(home.skaters > 2 & away.skaters > 2 & home.G == 1 & away.G > 1 & ev.team==hometeam) +
                 5*(home.skaters > 2 & away.skaters > 2 & home.G == 1 & away.G > 1 & ev.team!=hometeam) 
                 )
        picked <- split.score.list[[match(input$scorechoice, split.score.choices)]]
        
        message ("Score state picked: ", picked)
        
        if (picked %in% 0:6)  ## all regulars.
            this.table <- filter(this.table, score.diff.cat %in% picked)
        ## 7: all. 8: all, score-adjusted
        if (picked %in% 9)  ## within 1
            this.table <- filter(this.table, score.diff.cat %in% 2:4)
        if (picked %in% 10)  ## close
            this.table <- filter(this.table, (score.diff.cat %in% 2:4 & period %in% 1:2) | score.diff.cat == 3)
        if (picked %in% 11)  ## leading
            this.table <- filter(this.table, score.diff.cat %in% 4:6)
        if (picked %in% 12)  ## leading 2+
            this.table <- filter(this.table, score.diff.cat %in% 5:6)
        if (picked %in% 13)  ## trailing 2+
            this.table <- filter(this.table, score.diff.cat %in% 0:1)
        if (picked %in% 14)  ## trailing
            this.table <- filter(this.table, score.diff.cat %in% 0:2)
        
        
        
        picked.2 <- split.man.list[[match(input$manchoice, split.man.choices)]]
        message ("Man state picked: ", picked.2)
        
        if (input$manchoice != "All") this.table <- filter(this.table, gamestate %in% picked.2)
        
        return(this.table)

    })
      
    ## output$shotplot = renderPlot ({})


    
    output$newShotPlot <- renderUI(HTML(shot.plot.d3(reduced.pbp())))
  
    autoInvalidate2 <- reactiveTimer(1*1000, session)
    output$downloadData <- downloadHandler (
        filename = paste0("war-on-ice-", {autoInvalidate2(); Sys.time()},".csv"),
        content = function (file) write.csv (mytable.prime(), file)
        )
  
  
  # output$Errorchecker <- renderPrint ({
  #     input$daterange
  # })
  
})

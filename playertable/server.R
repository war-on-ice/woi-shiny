## playertable server
## acthomas, 8-20-14

library(shiny)
#load("season1314.RData")

source("global.R")

##sqlite.link <- "~/Documents/nhlr/war-on-ice.com/common-data/waronice.sqlite"
sqlite.link <- "../common-data/waronice.sqlite"

allplayer.query <- function (##player.name,
                             playoffs=TRUE,
                             regseason=TRUE,
                             daterange=c("2002-10-01",as.character(Sys.Date())),
                             my.gamestate = split.man.list[[5]],
                             my.scorediffcat = split.score.list[[1]],
                             my.home = homeaway.list[[1]]) {
    ## player.name = "Sidney Crosby"; playoffs=TRUE; daterange=c("2013-10-01","2014-04-01"); my.gamestate = split.man.list[[3]];  my.scorediffcat = split.score.list[[8]];  my.home = homeaway.list[[1]]
##    woi.id <- roster.unique$woi.id[match(player.name, roster.unique$firstlast)]
    
    statement <- paste0('SELECT * FROM playerrun WHERE scorediffcat = ',       ##ID = "', woi.id, '" AND 
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

    if (!playoffs) this.subset <- subset(this.subset, substr(gcode,1,1)==2)
    if (!regseason) this.subset <- subset(this.subset, substr(gcode,1,1)==3)
    
    return (this.subset)
    
}




shinyServer(function(input, output, session){
  
  ## input <- list(manchoice="All", scorechoice="All", homechoice="All", minTOI=0, datemode=FALSE, playersearch="", teamsearch="", startdate="20132014 Regular", enddate="20132014 Regular", daterange=c("2013-01-01","2014-09-01")); m1=21; m2=21; teampick=""
  
    GETterms <- reactive({g1 <- split.GET.to.frame(session$clientData$url_search); print(g1); g1})
  
    onload.usedaterange <- reactive (if (!is.null(GETterms()$usedaterange) &&
                                         (GETterms()$usedaterange %in% 0:1))
                                     GETterms()$usedaterange > 0 else FALSE)
    output$onload.datemode <-
        renderUI(checkboxInput(inputId="datemode", label="Use Date Range", value = onload.usedaterange()))
    
    onload.splitseasons <- reactive (if (!is.null(GETterms()$splitseasons) &&
                                         (GETterms()$splitseasons %in% 0:1))
                                     GETterms()$splitseasons > 0 else TRUE)
    output$onload.splitseasons <-
        renderUI(checkboxInput(inputId="splitseasons", label="Divide Data By Season", value = onload.splitseasons()))
    

    onload.playoffs <-
        reactive (if (!is.null(GETterms()$playoffs) && (GETterms()$playoffs %in% session.choices))
                            GETterms()$playoffs else session.choices[1])
    output$onload.playoffs <- renderUI(selectInput(inputId="playoffs",
                                                   label="Regular/Playoffs",
                                                   choices = session.choices,
                                                   selected = onload.playoffs()))


    onload.mintoi <- reactive (if (!is.null(GETterms()$mintoi))
                               as.numeric(GETterms()$mintoi) else 0) #
    output$onload.mintoi <- renderUI(numericInput("minTOI", "Minimum Time On Ice (Minutes)", 
                                                    min = 0, max = 6000, value = onload.mintoi()))
    

    
    onload.start0 <- reactive (if (!is.null(GETterms()$start0) &&
                                   (GETterms()$start0 %in% seasons))
                               GETterms()$start0 else "20142015")
    onload.end0 <- reactive (if (!is.null(GETterms()$end0) &&
                                 (GETterms()$end0 %in% seasons))
                             as.numeric(GETterms()$end0) else "20142015")
    output$onload.start0 <-
        renderUI(selectInput(inputId="start0", label="Starting Season",
                             selected = onload.start0(), choices=seasons))
    output$onload.end0 <-
        renderUI(selectInput(inputId="end0", label="Ending Season",
                             selected = onload.end0(), choices=seasons))
    
    onload.start1 <- reactive (if (!is.null(GETterms()$start1) &&
                                   grepl("[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}",GETterms()$start1))
                               GETterms()$start1 else "2014-10-01")
    onload.end1 <- reactive (if (!is.null(GETterms()$end1) &&
                                 grepl("[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}",GETterms()$end1))
                             GETterms()$end1 else Sys.Date())

    output$onload.daterange <-
        renderUI(dateRangeInput('daterange',
                                label = 'Date range', 
                                min = "2002-10-01", max = Sys.Date()+10,
                                start = onload.start1(), end= onload.end1(),
                                separator = " - ",
                                format = "yyyy-mm-dd", startview = 'year',
                                language = 'en', weekstart = 1))
  
    onload.manchoice <- reactive (if (!is.null(GETterms()$mansit) &&
                                      (GETterms()$mansit %in% 1:length(split.man.choices)))
                                  split.man.choices[as.numeric(GETterms()$mansit)] else "Even Strength 5v5") #
    output$onload.manchoice <-
        renderUI(selectInput(inputId="manchoice", label="Team Strengths",
                             selected = onload.manchoice(), choices=split.man.choices))
    
    onload.homeaway <- reactive (if (!is.null(GETterms()$homeawaysit) &&
                                     (GETterms()$homeawaysit %in% 1:length(homeaway.choices)))
                                 homeaway.choices[as.numeric(GETterms()$homeawaysit)] else "All")


            
    output$onload.homechoice <-
        renderUI(selectInput(inputId="homechoice", label="Home/Away Situation",
                             selected = onload.homeaway(), choices=homeaway.choices))
    
    
    onload.scorechoice <- reactive (if (!is.null(GETterms()$scoresit) &&
                                        (GETterms()$scoresit %in% 1:length(split.score.choices)))
                                    split.score.choices[as.numeric(GETterms()$scoresit)] else "All")
    output$onload.scorechoice <-
        renderUI(selectInput(inputId="scorechoice", label="Score Situation",
                             selected = onload.scorechoice(), choices=split.score.choices))

  
    onload.team <- reactive (if (!is.null(GETterms()$team) && any(unlist(strsplit(GETterms()$team, "\\+")) %in% team.options))
                             unlist(strsplit(GETterms()$team, "\\+"))[which(unlist(strsplit(GETterms()$team,"\\+")) %in% team.options)] else NULL)  #All
    output$onload.teamsearch <-
        renderUI(selectInput (inputId="teamsearch", label="Filter Teams",
                              selected = onload.team(), choices = team.options, multiple=TRUE))
    
  
  #    onload.whichPos <- reactive (if (!is.null(GETterms()$shotattsit) &&
  #                                     (GETterms()$shotattsit %in% 1:length(shotatt.choices)))
  #                                 shotatt.choices[as.numeric(GETterms()$shotattsit)] else "Fenwick")
  #    output$onload.whichPosLoc <-
  #        renderUI(selectInput(inputId="whichPosLoc", label="Shot Attempt Stats",
  #                             selected = onload.whichPos(), choices=shotatt.choices))
    onload.d <- reactive (if (!is.null(GETterms()$tablegroup) &&
                              (GETterms()$tablegroup %in% 1:length(screening.choices.pl)))
                          screening.choices.pl[as.numeric(GETterms()$tablegroup)] else "Prime")
    output$onload.whichPosLoc <-
        renderUI(selectInput(inputId="whichPosLoc", label="Columns To Display",
                             selected = onload.d(), choices=screening.choices.pl))
  
    onload.pos <- reactive (if (!is.null(GETterms()$pos) && (as.numeric(GETterms()$pos) %in% 1:length(position.choices)))
                            position.choices[as.numeric(GETterms()$pos)] else "All")
    output$onload.whichPosition <-
        renderUI(selectInput(inputId="whichPosition", label="Position",
                             selected = onload.pos(), choices = position.choices))
  
  
    onload.playersearch <- reactive (if (!is.null(GETterms()$names)) gsub("0"," ",GETterms()$names) else "")
    output$onload.playersearch <-
        renderUI(textInput (inputId="playersearch", label="Filter Players", value=onload.playersearch()))
  
  #    output$onload.showRawStats <-
  #        renderUI(checkboxInput(inputId="showRawStats", label="Show Extended Table", value = FALSE))


    
    player.3.variables <- reactive(if (input$legacynames) player.variables[,1:3] else player.variables[,c(4,5,3)])
    player.short.match <- reactive(if (input$legacynames) player.variables[,c(1,1)] else player.variables[,c(1,4)])
    player.plot.subset <- reactive({
        out <- player.3.variables()[-(1:5),]; print(head(out)); out
    })
    player.var.choices <- reactive(player.plot.subset()[,2])

    
    onload.xaxis <- reactive (if (!is.null(GETterms()$xaxis) && (as.numeric(GETterms()$xaxis) %in%
                                                                 1:length(player.var.choices())))
                              as.numeric(GETterms()$xaxis) else which (player.var.choices() == "Fraction of Off vs Def Zone Starts, Relative"))
    output$onload.xaxis <- renderUI(selectInput(inputId="xaxis", label="X Axis Variable",
                                                selected = player.var.choices()[onload.xaxis()], choices=player.var.choices()))
    
    onload.yaxis <- reactive (if (!is.null(GETterms()$yaxis) && (as.numeric(GETterms()$yaxis) %in%
                                                                 1:length(player.var.choices())))
                              as.numeric(GETterms()$yaxis) else which (player.var.choices() == "TOI/60 Of Competition"))
    output$onload.yaxis <- renderUI(selectInput(inputId="yaxis", label="Y Axis Variable",
                                                selected = player.var.choices()[onload.yaxis()], choices=player.var.choices()))
    
    onload.caxis <- reactive (if (!is.null(GETterms()$caxis) && (as.numeric(GETterms()$caxis) %in%
                                                                 1:length(player.var.choices())))
                              as.numeric(GETterms()$caxis) else which (player.var.choices() == "Relative Corsi For Percentage of Total"))
    output$onload.caxis <- renderUI(selectInput(inputId="caxis", label="Color Variable",
                                                selected = player.var.choices()[onload.caxis()], choices=player.var.choices()))
    
    onload.saxis <- reactive (if (!is.null(GETterms()$saxis) && (as.numeric(GETterms()$saxis) %in%
                                                                 1:length(player.var.choices())))
                              as.numeric(GETterms()$saxis) else which (player.var.choices() == "Time On Ice Per Game"))
    output$onload.saxis <- renderUI(selectInput(inputId="saxis", label="Size Variable",
                                                selected = player.var.choices()[onload.saxis()], choices=player.var.choices()))


    
    tab.choices <- c("Tabular View", "Graphical View")
    onload.tabpanelchoice <- reactive (if (!is.null(GETterms()$tab) && (GETterms()$tab %in% 1:length(tab.choices)))
                                       tab.choices[as.numeric(GETterms()$tab)] else tab.choices[1])
    output$onload.tab <- renderUI(
        tabsetPanel(id="tabset",
                    tabPanel("Graphical View",
                             div(
                                 column(3, htmlOutput ("onload.xaxis")),
                                 column(3, htmlOutput ("onload.yaxis")),
                                 column(3, htmlOutput ("onload.caxis")),
                                 column(3, htmlOutput ("onload.saxis")))
                             ##,h3 ("Plots are down for maintenance.")
                             ,plotOutput(outputId = "scatter", height="900px", width="900px")
                             ),
                    tabPanel("Tabular View",
                             dataTableOutput('mytable')
                             ),
                    selected = onload.tabpanelchoice()
                    ))


    
    outdest <- reactive(paste0 ("http://war-on-ice.com/playertable.html?",
                                "mansit=",which(input$manchoice == split.man.choices),
                                "&scoresit=",which(input$scorechoice == split.score.choices),
                                "&homeawaysit=",which(input$homechoice == homeaway.choices),
                                "&shotattsit=",which(input$whichPosLoc == shotatt.choices),
                                "&playoffs=",input$playoffs,
                                "&names=",gsub(" ","0",input$playersearch),
                                "&team=",paste(input$teamsearch, collapse="+"),
                                "&pos=",which(input$whichPosition == position.choices),
                                "&start1=",input$daterange[1],
                                "&xaxis=",which(input$xaxis == player.var.choices()),
                                "&yaxis=",which(input$yaxis == player.var.choices()),
                                "&caxis=",which(input$caxis == player.var.choices()),
                                "&saxis=",which(input$saxis == player.var.choices()),
                                "&mintoi=",input$minTOI,
                                "&tab=",which(input$tabset == tab.choices),
                                "&usedaterange=",1*(input$datemode),
                                "&start0=",input$start0,
                                "&end0=",input$end0,
                                "&end1=",input$daterange[2],
                                "&splitseasons=",1*(input$splitseasons)
                                ))
    
    output$shareDestination <- renderText(outdest())



    
    score.factor <- reactive(split.score.list[[match(input$scorechoice, split.score.choices)]])
    man.factor <- reactive(split.man.list[[match(input$manchoice, split.man.choices)]])

    
    grand.date.range <- reactive(if (input$datemode) {
        input$daterange
    } else {
        seasonrange <- sort(c(input$start0,input$end0))
        c(paste0(substr(seasonrange[1],1,4),"-09-15"),
          paste0(substr(seasonrange[2],5,8),"-06-30"))
    })

   
    queried.datemode <- reactive({

        st.by.date <- allplayer.query (playoffs = input$playoffs %in% c("All", "Playoffs"),
                                       regseason = input$playoffs %in% c("All", "Regular"),
                                       daterange=grand.date.range(),
                                       my.gamestate=man.factor(),
                                       my.scorediffcat=score.factor(),
                                       my.home=homeaway.list[[match(input$homechoice, homeaway.choices)]])

        ## Infinity patch. 
        ##st.by.date$cCF60[is.infinite (st.by.date$cCF60)] <- NA        
        ##this.subset <- subset(st.by.date, thisdate >= grand.date.range()[1] &
        ##                      thisdate <= grand.date.range()[2] & playoff.condition) %>%
        ##                         mutate(Name = roster.unique$firstlast[match(ID, roster.unique$woi.id)])   ## recent, 2015-01-29
        st.by.date
        
    })
        
    queried.data <- reactive({queried.datemode() %>% fix.column.names })
    
    fulldata <- reactive ({
        
        teampick <- input$teamsearch;
        ##message ("Teams picked: ",teampick)
        if (is.null(teampick)) teampick <- ""
        if (any(teampick == "All")) teampick <- "" else {
            teampick <- gsub("\\.","\\\\.", teampick)
            teampick <- paste(teampick, collapse="|")
        } 
    
        multiplayer <- unlist(strsplit (toupper(input$playersearch), " *, *"))
        if (length(multiplayer)>0) multiplayer <- multiplayer[nchar(multiplayer)>0]
        if (length(multiplayer)>0) {
            searchmatch <- sapply(multiplayer, function(mm) grepl(mm, toupper(queried.data()$Name)))
            sm1 <- apply(searchmatch, 1, sum) > 0
        } else sm1 <- rep(TRUE, length(queried.data()$Name))
    
        this.subset <- queried.data()[sm1 & grepl(teampick, queried.data()$Team),]
        pos.pick <- pos.list[[match(input$whichPosition, position.choices)]]

        #this.subset <- swapnames(this.subset)
        ##print(head(this.subset,2))
        
        out <- NULL
        if (input$splitseasons) {
            
            this.subset$season[substr(this.subset$gcode,1,1) == 3] <- paste(this.subset$season[substr(this.subset$gcode,1,1) == 3], "-Playoffs")
            
            out <- if (!input$sepgame) {
                this.subset %>% group_by (ID, season) %>% compress.player.game
            } else {
                this.subset %>% group_by (ID, season, gcode) %>% compress.player.game
            }
            out$Age <- ageAsOfSep15 (out$season, as.character(roster.unique$DOB[match(out$ID, roster.unique$woi.id)]))

            #print(out$Age)
            #print(head(as.data.frame(out)))
            #print (paste0(contracts$WOI.ID, contracts$Year-1, contracts$Year))
            #print (paste0(out$ID, out$season))
            
            rowmatch <- match (paste0(out$ID, out$season),
                               paste0(contracts$woiid, contracts$Year-1, contracts$Year))
            out$Salary <- sprintf("%.3f", contracts$TotalComp[rowmatch]/1E6)
            out$AAV <- sprintf("%.3f", contracts$AAV[rowmatch]/1E6)

            out <- out %>% filter (Age >= input$agerange[1] | is.na(Age),
                                   Age <= input$agerange[2] | is.na(Age))
                                   
        } else {
            out <- this.subset %>% group_by (ID) %>% compress.player.game
        }

        out$Name <- roster.unique$firstlast[match(out$ID, roster.unique$woi.id)]
        out$pos <- roster.unique$pos[match(out$Name, roster.unique$firstlast)]

        ## print(head(out,2))
        
        out <- out %>% filter (TOI.on >= as.numeric(input$minTOI), pos %in% pos.pick) %>% add.rates.players
        out <- out[rev(order(out[["C+/-"]])),]

#        if (!input$datemode) out[,c("FenT%","FenC%","CorT%","CorC%","TOIC%","TOIT%")] <-
#                       round(out[,c("FenT%","FenC%","CorT%","CorC%","TOIC%","TOIT%")] * 1, 2)
        out <- subset(out, pos %in% pos.pick)
        out <- out[,c(which(colnames(out) == "pos"), which(colnames(out) != "pos"))] 

                ## !!!

        if (input$FAscreen == "RFA") {
            out <- filter(out, ID %in% RFA.list)
        } else if (input$FAscreen == "UFA") {
            out <- filter(out, ID %in% UFA.list)
        }
        ##if (input$FAscreen) out <- filter(out, ID %in% pending.FAs)
        
        return(out)
    })
  
 
    mytable.prime <- reactive ({
        
        out <- fulldata()
        out2 <- data.frame(Name=gsub(" ",".",out$Name),
                           pos=out$pos,  #roster.unique$pos[match(out$Name,roster.unique$firstlast)]
                           Team=out$Team,
                           Gm=out$Gm)
        
        rownames (out2) <- paste0(out$ID, out$season, out$gcode)
        if (input$splitseasons) {
            if (input$sepgame) {
                out2 <- out2[,colnames(out2) != "Gm"]
                out2$Date <- gamestest$date[match(paste(substr(out$season,1,8), out$gcode),
                                                  paste(gamestest$season, gamestest$gcode))]
            }
            out2 <- cbind (out2, season=out$season, Age=out$Age, Salary=out$Salary, AAV=out$AAV)
        }
        group <- input$whichPosLoc
        
        if (group %in% c("Prime", "All")) {  
            out2$G <- out$G
            out2$A <- out$A1 + out$A2
            out2$P <- out2$G + out2$A
            
            out2 <- cbind(out2, out[,c("G60", "A60", "P60", "PenD", "CF%", "PDO", "PSh%", "ZSO%Rel", "TOI/Gm")])  #
        }
        ##int(head(out,2))


        if (group %in% c("High-Danger Chances", "All"))
            out2 <- cbind(out2, out[,c("iHSC","HSCF%Rel","HSCF%","HSCF%off", "HSC+/-",
                                             "HSCF","HSCA","HSCF60","HSCA60","HSCP60")])
    
        
        if (group %in% c("Scoring Chances", "All"))
            out2 <- cbind(out2, out[,c("SCF%Rel","SCF%","SCF%off", "SC+/-",
                                       "SCF","SCA","SCF60","SCA60","SCP60",
                                       "iSC")])
        

        if (group %in% c("Corsi", "All"))
            out2 <- cbind(out2, out[,c("CF%Rel","CF%","CF%off", "C+/-",
                                       "CF","CA","CF60","CA60","CP60",
                                       "OCOn%", "BK","AB", "iCF")])
        
        if (group %in% c("Fenwick", "All"))
            out2 <- cbind(out2, out[,c("FF%Rel", "FF%","FF%off", "F+/-",
                                       "FF","FA","FF60","FA60", "FP60",
                                       "OFOn%", "MS", "iFF")])
        
        if (group %in% c("Shot-Based", "All"))
            out2 <- cbind(out2, out[,c("SF%Rel","SF%","SF%off", "S+/-",
                                       "SF60","SA60", "SF","SA", "iSF")])
        
        if (group %in% c("Goal-Based", "All"))
            out2 <- cbind(out2, out[,c("GF%Rel","GF%off", "GF60", "GA60",
                                       "GF","GA","G+/-","GF%", "PFenSh%",
                                       "OSh%", "OFenSh%", "OSv%")])
        
        if (group %in% c("Faceoffs", "All"))
            out2 <- cbind(out2, out[,c("FO%^", "FO_W", "FO_L", "ZSO%", "ZSO","ZSN","ZSD")])
        
        if (group %in% c("Base Counts", "All"))
            out2 <- cbind(out2, out[,c("HIT", "HIT-",
                                       "A1", "A2", "SH", "MS", "BK", "AB",
                                       "GV", "TK",
                                       "PN", "PN-", "PenD60",
                                       "TOI", "TOIoff", "TOI%")])
        
        if (group %in% c("Teammate/Competition", "All"))
            out2 <- cbind(out2, out[,c("TOIT%", "TOIC%", "CorT%", "CorC%"   ##,"FenT%", "FenC%"
                                       ,"tCF60","tCA60","cCF60","cCA60"
                                       )])

        colnames(out2) <- gsub("out\\.","",colnames(out2))
        out2 <- out2[,unique(colnames(out2))]
        numeric.columns <- sapply(out2, class) 
        out2[,numeric.columns=="numeric"] <- round(out2[,numeric.columns=="numeric"], 2)

        
        out2
    
    })
    output$mytable = renderDataTable({
        out <- mytable.prime()
        out$Name <- playerseason.url(out$Name, substr(rownames(out),1,9))
        out$Team <- teamtime.url(out$Team)
        if (input$sepgame & input$splitseasons) out$Date <- woigame.url(out$Date, paste0(fulldata()$season, fulldata()$gcode))

        ## Switch it up.
        colnames(out) <- swapout.oldnew (colnames(out), player.short.match()) %>% augment.html.div (player.3.variables())

        #player.short.match()[match(colnames(out), player.short.match()[,1]), 2]
        #colnames(out) <- augment.html.div(colnames(out), player.3.variables())
        out
        
    }, options = list(searching = FALSE,
           scrollX = "100%", scrollY = "100%",
           lengthChange = FALSE, pageLength=20)
        )
  
    output$scatter = renderPlot ({
        myplan <- layout (matrix(c(2, 1), nrow=1, ncol=2),
                          widths=c(1, 6.5), heights=c(6))
        namematch <- match(c(input$xaxis, input$yaxis, input$caxis, input$saxis), player.3.variables()[,2])
        
        pieces.1 <- player.3.variables()[namematch, 1]
        pieces.2 <- player.short.match()[match(pieces.1, player.short.match()[,2]), 1]
        
        subdata <- fulldata()[,pieces.2];
        colprimer = subdata[[3]]
        legend.length <- 25

        print(head(subdata))
        
        datacat <- player.3.variables()[namematch, 3]
        print(datacat)
        
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


        ## Actual plot.
        this.cex <- 15*(subdata[[4]]/max(subdata[[4]]))
        plot (subdata[[1]], subdata[[2]],
              xlab=input$xaxis, ylab=input$yaxis,
              cex=this.cex, col=this.col, pch=16,
              main = paste(input$xaxis, " vs. ", input$yaxis, "\nColored by ",
                  input$caxis, ", Sized by ", input$saxis, sep = "")
              )
        points (subdata[[1]], subdata[[2]], cex=this.cex)
        text (subdata[[1]], subdata[[2]], fulldata()[["Name"]])
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
    
    output$downloadData <- downloadHandler (
        filename = paste0("war-on-ice-", Sys.time(),".csv"),
        content = function (file) write.csv (mytable.prime(), file)
        )
  
  
  # output$Errorchecker <- renderPrint ({
  #     input$daterange
  # })
  
})

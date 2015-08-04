## goaltender server
## acthomas, 8-20-14

#library(shiny)
#load("season1314.RData")

source("global.R")

#sqlite.link <- "~/Documents/nhlr/war-on-ice.com/common-data/waronice.sqlite"
sqlite.link <- "../common-data/waronice.sqlite"


allplayer.query <- function (##player.name,
                             playoffs=TRUE,
                             regseason=TRUE,
                             daterange=c("2002-10-01",as.character(Sys.Date())),
                             my.gamestate = split.man.list[[5]],
                             my.scorediffcat = split.score.list[[1]],
                             my.home = homeaway.list[[1]]) {
    ## playoffs=TRUE; daterange=c("2013-10-01","2014-04-01"); my.gamestate = split.man.list[[3]];  my.scorediffcat = split.score.list[[8]];  my.home = homeaway.list[[1]]
##    woi.id <- roster.unique$woi.id[match(player.name, roster.unique$firstlast)]

    message ("gm ",my.gamestate," sd ",my.scorediffcat," hm ",my.home)
    
    statement <- paste0('SELECT * FROM goalierun WHERE scorediffcat = ',       ##ID = "', woi.id, '" AND 
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


shinyServer(function(input, output, session) {

    
    GETterms <- reactive({g1 <- split.GET.to.frame(session$clientData$url_search); g1})

    onload.splitseasons <- reactive (if (!is.null(GETterms()$splitseasons) &&
                                         (GETterms()$splitseasons %in% 0:1))
                                     GETterms()$splitseasons > 0 else TRUE)
    output$onload.splitseasons <-
        renderUI(checkboxInput(inputId="splitseasons", label="Divide Data By Season", value = onload.splitseasons()))


    

    
    onload.manchoice <- reactive (if (!is.null(GETterms()$mansit) &&
                                      (GETterms()$mansit %in% 1:length(split.man.choices)))
                                  split.man.choices[as.numeric(GETterms()$mansit)] else "Even Strength 5v5")
    output$onload.manchoice <-
        renderUI(selectInput(inputId="whichManSit", label="Team Strengths",
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

    
    onload.team <-
        reactive (if (!is.null(GETterms()$team) && any(unlist(strsplit(GETterms()$team, "\\+"))
                                                       %in% team.options))
                  unlist(strsplit(GETterms()$team, "\\+"))[which(unlist(strsplit(GETterms()$team,"\\+"))
                                                                 %in% team.options)] else NULL)  #All
    onload.team <- reactive (if (!is.null(GETterms()$team) && (GETterms()$team %in% team.options))
                             GETterms()$team else NULL)
    output$onload.teamsearch <-
        renderUI(selectInput (inputId="teamsearch", label="Filter Teams",
                              selected = onload.team(), choices = team.options, multiple=TRUE))


    onload.mintoi <- reactive (if (!is.null(GETterms()$mintoi))
                               as.numeric(GETterms()$mintoi) else 240) #
    output$onload.mintoi <- renderUI(numericInput("minTOI", "Minimum Time On Ice (Minutes)", 
                                                    min = 0, max = 6000, value = onload.mintoi()))



    onload.playoffs <-
        reactive (if (!is.null(GETterms()$playoffs) && (GETterms()$playoffs %in% session.choices))
                  GETterms()$playoffs else session.choices[1])
    output$onload.playoffs <- renderUI(selectInput(inputId="playoffs",
                                                   label="Regular/Playoffs",
                                                   choices = session.choices,
                                                   selected = onload.playoffs()))

        

    onload.start1 <- reactive (if (!is.null(GETterms()$start1) &&
                               grepl("[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}",GETterms()$start1))
                               GETterms()$start1 else "2014-10-01")
    onload.end1 <- reactive (if (!is.null(GETterms()$end1) &&
                                 grepl("[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}",GETterms()$end1))
                             GETterms()$end1 else Sys.Date())

    output$onload.daterange <-
        renderUI(dateRangeInput('daterange',
                                label = 'Date range', 
                                min = "2005-10-01", max = Sys.Date()+10,
                                start = onload.start1(), end= onload.end1(),
                                separator = " - ",
                                format = "yyyy-mm-dd", startview = 'year',
                                language = 'en', weekstart = 1))

    outdest <- reactive(paste0 ("http://war-on-ice.com/goalietable.html?",
                                "mansit=",which(input$manchoice == split.man.choices),
                                "&homeawaysit=",which(input$homechoice == homeaway.choices),
                                "&scoresit=",which(input$scorechoice == split.score.choices),
                                "&team=",input$teamsearch,
                                "&playoffs=",input$playoffs,
                                "&start1=",input$daterange[1],
                                "&end1=",input$daterange[2]))
    
    output$shareDestination <- renderText(outdest())

    score.factor <- reactive(split.score.list[[match(input$scorechoice, split.score.choices)]])
    man.factor <- reactive(split.man.list[[match(input$whichManSit, split.man.choices)]])
    
    superfulldata <- reactive ({
        
        st.by.date <- allplayer.query (playoffs = input$playoffs %in% c("All", "Playoffs"),
                                       regseason = input$playoffs %in% c("All", "Regular"),
                                       daterange=input$daterange,
                                       my.gamestate=man.factor(),
                                       my.scorediffcat=score.factor(),
                                       my.home=homeaway.list[[match(input$homechoice, homeaway.choices)]])
        ## print (head(st.by.date))
        return(st.by.date)
        
    })
  
    r.fulldata <- reactive ({

        teampick <- input$teamsearch;                         ##print (teampick)
        if (is.null(teampick)) teampick <- ""
        if (any(teampick == "All")) teampick <- "" else {
            teampick <- gsub("\\.","\\\\.", teampick)
            teampick <- paste(teampick, collapse="|")
        }
        
        multiplayer <- unlist(strsplit (toupper(input$playersearch), " *, *"))
        if (length(multiplayer)>0) multiplayer <- multiplayer[nchar(multiplayer)>0]
        if (length(multiplayer)>0) {
            searchmatch <- sapply(multiplayer, function(mm) grepl(mm, toupper(superfulldata()$Name)))
            namematch1 <- apply(searchmatch, 1, sum)>0
        } else namematch1 <- rep(TRUE, length(superfulldata()$Name))
        
        rfdata <- superfulldata()[namematch1 &  
                               grepl(teampick, superfulldata()$Team) #&
                                  , ]
        print(dim(rfdata))
        return(rfdata)
        
    })

    total.shots <- reactive({
        load("../common-data/conditions.RData")
        
        filter (conditions,
                scorediffcat %in% score.factor(),
                gamestate %in% man.factor(),
                home %in% homeaway.list[[match(input$homechoice, homeaway.choices)]]) %>%
            summarize(S1=sum(shots.1+goals.1), S2=sum(shots.2+goals.2), S3=sum(shots.3+goals.3+shots.4+goals.4)) %>% unlist

    })

    fulldata <- reactive ({

        #total.shots <- summarize(superfulldata(),
        #                         ts1=sum(goals.1+shots.1), ts2=sum(goals.2+shots.2),
        #                         ts3=sum(goals.3+shots.3+goals.4+shots.4))#, ts0=sum(goals.0+shots.0))
        print(r.fulldata())
        
        if (input$splitseasons) {
            if (input$sepgame) {
                out <-  r.fulldata() %>% compress.goalietable.game () %>% filter (!is.na(Name)) %>% as.data.frame
                rownames (out) <- paste0(out$ID,"-",out$season,out$gcode)
            } else {
                out <-  r.fulldata() %>% compress.goalietable.season () %>% filter(TOI >= as.numeric(input$minTOI)) %>% as.data.frame
                rownames (out) <- paste0(out$ID,"-",out$season)
                    ## Salary data will be added back soon.
            }
                
            rowmatch <- match (paste0(out$ID, out$season),
                               paste0(contracts$woiid, contracts$Year-1, contracts$Year))
            out$Salary <- sprintf("%.3f", contracts$TotalComp[rowmatch]/1E6)
            out$AAV <- sprintf("%.3f", contracts$AAV[rowmatch]/1E6)

        } else {
            out <-  r.fulldata() %>% compress.goalietable () %>% filter(TOI >= as.numeric(input$minTOI)) %>% as.data.frame
            rownames (out) <- out$ID
        }
        ##if (input$FAscreen) out <- filter(out, ID %in% pending.FAs)
        if (input$FAscreen == "RFA") {
            out <- filter(out, ID %in% RFA.list)
        } else if (input$FAscreen == "UFA") {
            out <- filter(out, ID %in% UFA.list)
        }
        
        print(head(out))
                
        out$G <- rowSums(out[,c("G.L","G.M","G.H")])
        out$S <- rowSums(out[,c("S.L","S.M","S.H")])
        out$Sh <- out$G + out$S
        out$Name <- gsub(" ",".",out$Name)
        out$SA60 <- round((out$G+out$S)/out$TOI*60, 2)
        
        overall.totals <- colSums(out[,c("G.L","S.L", "G.M","S.M", "G.H","S.H")])
        out[["Sv%"]] <- round(out$S/(out$S + out$G) * 100, 2)#rowSums(out[,c(5,7,9)])/rowSums(out[,4:9])
        out[["Sv%L"]] <- round(out$S.L/(out$S.L + out$G.L) * 100, 2)
        out[["Sv%M"]] <- round(out$S.M/(out$S.M + out$G.M) * 100, 2)
        out[["Sv%H"]] <- round(out$S.H/(out$S.H + out$G.H) * 100, 2)

        out[["AdSv%"]] <- as.vector(round((out[["Sv%L"]]*total.shots()[[1]] +
                                              out[["Sv%M"]]*total.shots()[[2]] +
                                              out[["Sv%H"]]*total.shots()[[3]])/sum(total.shots()), 2))

        if (input$splitseasons) out$Age <- ageAsOfSep15 (out$season, as.character(roster.unique$DOB[match(out$ID, roster.unique$woi.id)]))

        out <- out[rev(order(out[["Sv%"]])),]
        out2 <- out[,c("Name","Team","Gm")]

        if (input$splitseasons) out2 <- cbind(out2,
                                              season=out[["season"]],
                                              Age=out[["Age"]],
                                              Salary=out[["Salary"]],
                                              AAV=out[["AAV"]])

        if (input$sepgame & input$splitseasons) {
            Date <- gamestest$date[match(paste0(out$season, out$gcode),
                                         paste0(gamestest$season, gamestest$gcode))]
            ##print(Date)
            out2$Date <- woigame.url(Date, paste0(out$season, out$gcode))
        }
        
        out2 <- cbind(out2, out[,c("Sv%","AdSv%","SA60","TOI",
                                   "Sv%L","Sv%M","Sv%H",
                                   "G","S","Sh",
                                   "G.L","S.L","G.M","S.M","G.H","S.H")])
                      
        return(out2)
    })
  
 
    output$mytable = renderDataTable({
        out <- fulldata()
        #out2 <- out[,c("Name", "Team", "Games", "UnadjSvPct", 
        #               "AdjustedSvPct", "SvPctLow", 
        #               "SvPctMed", "SvPctHigh", "ShotsPer60")]
        #if (input$showRawStats)
        #    out2 <- cbind(out2, out[,c("TOI","Goals.Low","Saves.Low","Goals.Med","Saves.Med",
        #                               "Goals.High","Saves.High","Goals.All","Saves.All","Shots.All")])
        out$Name <- goalieseason.url(out$Name, substr(rownames(out), 1, 9))
        out$Team <- teamtime.url(out$Team)
        colnames(out) <- augment.html.div (colnames(out))
        as.data.frame(out)
    }, options = list(searching = FALSE,
           scrollX = "100%", scrollY = "100%",
           lengthChange = FALSE, pageLength=20)  #,options = list(bFilter = FALSE)
        )
  
    output$scatter = renderPlot ({
    
        myplan <- layout (matrix(c(2,1), nrow=1, ncol=2),
                          widths=c(1, 6.5), heights=c(6))
        col.pct <- c("#FF8888","#FF8888","#FF8888","white","#8888FF","#8888FF","#8888FF")
        col.rel <- c("#FF8888","white","#8888FF")
        legend.length <- 25

        pieces <- goalie.variables[match(c(input$xaxis, input$yaxis, input$caxis, input$saxis),
                                         goalie.variables[,2]), 1]        ##print(pieces)
        subdata <- fulldata()[,pieces]
        
        colprimer <- subdata[[3]]#()[,input$caxis]
        
        if (grepl ("%", input$caxis) & !grepl ("Rel%", input$caxis)) {
            this.col <- crp(col.pct,colprimer/100)
            d.range <- seq(0, 1, length=legend.length)
        } else if (grepl ("Rel%", input$caxis)) {
            this.col <- crp(col.rel, 0.5 + 0.5*(colprimer)/max(abs(colprimer)))
            d.range <- seq(-max(abs(colprimer)), max(abs(colprimer)), length=legend.length)
        } else {
            this.col <- crp(c("#FF8888","white","#8888FF"),(colprimer - min(colprimer))/(max(colprimer) - min(colprimer)))
            d.range <- seq(min(colprimer), max(colprimer), length=legend.length)
        }
        
        this.cex <- 15*subdata[[4]]/max(subdata[[4]]) ##(fulldata()[,input$saxis]/max(fulldata()[,input$saxis]))
        xlim1 <- range(subdata[,1]); xlim2 <- xlim1[2]-xlim1[1]
        ylim1 <- range(subdata[,2]); ylim2 <- ylim1[2]-ylim1[1]
        
        plot (subdata[,1:2],  ##fulldata()[,c(input$xaxis, input$yaxis)],
              xlab=input$xaxis, ylab=input$yaxis,
              xlim=range(subdata[,1]) + c(-xlim2, xlim2)/18,
              ylim=range(subdata[,2]) + c(-ylim2, ylim2)/36,
              cex=this.cex, col=this.col, pch=16, 
              main = paste(input$xaxis, " vs. ", input$yaxis, "\nColored by ",
                  input$caxis, ", Sized by ", input$saxis, sep = ""))
        points (subdata[,1:2],   ##fulldata()[,c(input$xaxis, input$yaxis)],
                cex=this.cex)
        text (subdata[,1],  ##fulldata()[,input$xaxis], fulldata()[,input$yaxis]
              subdata[,2], fulldata()[,"Name"])
        legend ("bottomright", "war-on-ice.com", bty="n", cex=0.8)
        
        
        par(mar=c(3,3,3,0))
        xlim1 <- 16*c(-0.5, 0.5)/7/1.7
        if (grepl ("%", input$caxis) & !grepl ("Rel%", input$caxis)) {
            plot (rep(0, legend.length), d.range,
                  xlim=xlim1, axes=FALSE, cex=4, main=input$caxis, cex.main=0.8,
                  pch=16, col=crp(col.pct, seq(0, 1, length=legend.length)))
        } else if (grepl ("Rel%", input$caxis)) {
            plot (rep(0, legend.length), d.range,
                  xlim=xlim1, axes=FALSE, cex=4, main=input$caxis, cex.main=0.8,
                  pch=16, col=crp(col.rel, seq(0, 1, length=legend.length)))
        } else {
            plot (rep(0, legend.length), d.range,
                  xlim=xlim1, axes=FALSE, cex=4, main=input$caxis, cex.main=0.8,
                  pch=16, col=crp(c("#FF8888","white","#8888FF"), seq(0, 1, length=legend.length)))
        }
        points (rep(0, legend.length), d.range, cex=4)
        axis(2)
        
        
    })
    
    output$downloadData <- downloadHandler (
        filename = paste0("war-on-ice-goalie-", Sys.time(),".csv"),
        content = function (file) write.csv ({
            d1 <- fulldata()
            if (input$sepgame) d1$Date <- un.url(d1$Date)
            d1
        }, file)
        )
    
  
  # output$Errorchecker <- renderPrint ({
  #     input$daterange
  # })
  
})

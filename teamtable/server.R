## teamtable server
## acthomas, 8-21-14
#library(shiny)
#load("season1314.RData")

source("global.R")
load("../common-data/woi-common.RData")
#seasons <- seasons[1:11]

##sqlite.link <- "~/Documents/nhlr/war-on-ice.com/common-data/waronice.sqlite"
sqlite.link <- "../common-data/waronice.sqlite"


team.query <- function (playoffs=TRUE,
                        regseason=TRUE,
                        daterange=c("2002-10-01",as.character(Sys.Date())),
                        period.set = 0,
                        man.factor = split.man.list[[5]],
                        score.factor = split.score.list[[1]],
                        homechoice = "All") {
    ## playoffs=TRUE; daterange=c("2013-10-01","2014-04-01"); my.gamestate = split.man.list[[3]];  my.scorediffcat = split.score.list[[8]];  my.home = homeaway.list[[1]]
##    woi.id <- roster.unique$woi.id[match(player.name, roster.unique$firstlast)]

    my.home <- if (homechoice == "Away") 0 else if (homechoice == "Home") 1 else c(0,1)
    message (" gm ",man.factor," sd ",score.factor," hm ",my.home)
    
    statement <- paste0('SELECT * FROM teamrun WHERE scorediffcat = ',       ##ID = "', woi.id, '" AND 
                        score.factor,
                        ' AND gamestate = ',
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




    
shinyServer(function(input, output, session){
  #input=list(daterange=c("2013-10-01","2014-10-01"), minTOI=60, whichPosLoc="All")


    
    GETterms <- reactive({g1 <- split.GET.to.frame(session$clientData$url_search); g1})

    onload.a <- reactive (if (!is.null(GETterms()$mansit) && (GETterms()$mansit %in% 1:length(split.man.choices)))
                              split.man.choices[as.numeric(GETterms()$mansit)] else "Even Strength 5v5")
    output$onload.1 <- renderUI(selectInput(inputId="manchoice", label="Team Strengths", selected = onload.a(), choices=split.man.choices))

    onload.b <- reactive (if (!is.null(GETterms()$scoresit) && (GETterms()$scoresit %in% 1:length(split.score.choices)))
                              split.score.choices[as.numeric(GETterms()$scoresit)] else "All")
    output$onload.2 <- renderUI(selectInput(inputId="scorechoice", label="Score Situation", selected = onload.b(), choices=split.score.choices))

    onload.c <- reactive (if (!is.null(GETterms()$homeawaysit) && (GETterms()$homeawaysit %in% 1:length(homeaway.choices)))
                          homeaway.choices[as.numeric(GETterms()$homeawaysit)] else "All")
    output$onload.3 <- renderUI(selectInput(inputId="homechoice", label="Home/Away Situation", selected = onload.c(), choices=homeaway.choices))

    onload.d <- reactive (if (!is.null(GETterms()$tablegroup) &&
                                     (GETterms()$tablegroup %in% 1:length(screening.choices)))
                                 screening.choices[as.numeric(GETterms()$tablegroup)] else "Prime")
    output$onload.4 <-
        renderUI(selectInput(inputId="whichPosLoc", label="Columns To Display",
                             selected = onload.d(), choices=screening.choices))




    onload.usedaterange <- reactive (if (!is.null(GETterms()$usedaterange) &&
                                         (GETterms()$usedaterange %in% 0:1))
                                     GETterms()$usedaterange > 0 else FALSE)
    output$onload.datemode <-
        renderUI(checkboxInput(inputId="datemode", label="Use Date Range", value = onload.usedaterange()))
    

    onload.startdate <- reactive (if (!is.null(GETterms()$start0) &&
                                      (GETterms()$start0 %in% seasons))
                                  GETterms()$start0 else "20142015")
    onload.enddate <- reactive (if (!is.null(GETterms()$end0) &&
                                    (GETterms()$end0 %in% seasons))
                                GETterms()$end0 else "20142015")
    output$onload.startseason <-
        renderUI(selectInput(inputId="startseason", label="Starting Season",
                             selected = onload.startdate(), choices=seasons))
    output$onload.endseason <-
        renderUI(selectInput(inputId="endseason", label="Ending Season",
                             selected = onload.enddate(), choices=seasons))


    
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

    
    onload.splitseasons <- reactive (if (!is.null(GETterms()$splitseasons) &&
                                        (GETterms()$splitseasons %in% 0:1))
                                    GETterms()$splitseasons > 0 else TRUE)
    output$onload.splitseasons <-
        renderUI(checkboxInput(inputId="splitseasons", label="Divide Data By Season", value = onload.splitseasons()))


    
    team.3.variables <- reactive(if (input$legacynames) team.variables[,1:3] else team.variables[,c(4,5,3)])
    team.short.match <- reactive(if (input$legacynames) team.variables[,c(1,1)] else team.variables[,c(1,4)])
    team.plot.subset <- reactive({
        out <- team.3.variables()[-(1:5),]; print(head(out)); out
    })
    team.var.choices <- reactive(team.plot.subset()[,2])



    onload.playoffs <-
        reactive (if (!is.null(GETterms()$playoffs) && (GETterms()$playoffs %in% session.choices))
                  GETterms()$playoffs else session.choices[1])
    output$onload.playoffs <- renderUI(selectInput(inputId="playoffs",
                                                   label="Regular/Playoffs",
                                                   choices = session.choices,
                                                   selected = onload.playoffs()))


    


    onload.xaxis <- reactive (if (!is.null(GETterms()$xaxis) && (as.numeric(GETterms()$xaxis) %in% 1:length(team.var.choices())))
                              as.numeric(GETterms()$xaxis) else which(team.var.choices() == "Fraction of Off vs Def Zone Starts"))
    output$onload.xaxis <-
        renderUI(selectInput(inputId="xaxis", label="X Axis Variable",
                             selected = team.var.choices()[onload.xaxis()], choices=team.var.choices()))
    
    onload.yaxis <- reactive (if (!is.null(GETterms()$yaxis) && (as.numeric(GETterms()$yaxis) %in% 1:length(team.var.choices())))
                              as.numeric(GETterms()$yaxis) else which(team.var.choices() == "On-Ice Goal Differential"))
    output$onload.yaxis <-
        renderUI(selectInput(inputId="yaxis", label="Y Axis Variable",
                             selected = team.var.choices()[onload.yaxis()], choices=team.var.choices()))
    
    onload.caxis <- reactive (if (!is.null(GETterms()$caxis) && (as.numeric(GETterms()$caxis) %in% 1:length(team.var.choices())))
                              as.numeric(GETterms()$caxis) else which(team.var.choices() == "PDO (On-Ice SvPct plus On-Ice ShPct)"))
    output$onload.caxis <-
        renderUI(selectInput(inputId="caxis", label="Color Variable",
                             selected = team.var.choices()[onload.caxis()], choices=team.var.choices()))
    
    onload.saxis <- reactive (if (!is.null(GETterms()$saxis) && (as.numeric(GETterms()$saxis) %in% 1:length(team.var.choices())))
                              as.numeric(GETterms()$saxis) else which(team.var.choices() == "Time On Ice"))
    output$onload.saxis <-
        renderUI(selectInput(inputId="saxis", label="Size Variable",
                             selected = team.var.choices()[onload.saxis()], choices=team.var.choices()))


    period.options <- reactive (3)
    output$dynamic.period <-
        renderUI (selectInput(inputId="period", label="Period",
                              selected = "All", choices=c("All", paste(1:period.options()))))
    period.set <- reactive(if (input$period == "All") 0 else as.numeric(input$period))  ## new!
    
   

    outdest <- reactive(paste0 ("http://war-on-ice.com/teamtable.html?",
                                "mansit=",which(input$manchoice == split.man.choices),
                                "&scoresit=",which(input$scorechoice == split.score.choices),
                                "&homeawaysit=",which(input$homechoice == homeaway.choices),
                                "&tablegroup=",which(input$whichPosLoc == screening.choices),
                                "&playoffs=",input$playoffs,
                                "&xaxis=",which(input$xaxis == team.var.choices()),
                                "&yaxis=",which(input$yaxis == team.var.choices()),
                                "&saxis=",which(input$saxis == team.var.choices()),
                                "&caxis=",which(input$caxis == team.var.choices()),
                                "&start0=",input$startseason,
                                "&end0=",input$endseason,
                                "&start1=",input$daterange[1],
                                "&end1=",input$daterange[2],
                                "&splitseasons=",1*(input$splitseasons),
                                "&usedaterange=",1*(input$datemode),
                                "&tablegroup=",which(input$whichPosLoc == screening.choices)

                                #"&team=",input$teamsearch,
                                #"&secondteam=",input$secondteam,
                                #"&windowsize=",input$windowsize,
                                #"&yaxiss=",which(input$yaxiss == var.choices),
                                #"&colors=",which(input$colors == var.choices),
                                #"&sizes=",which(input$sizes == var.choices),
                                #"&panel=",which(input$tabset == tab.choices)
                                ))
    output$shareDestination <- renderText({ 
        outdest()
    })

    tab.choices <- c("Graphical View", "Tabular View")
    onload.tabpanelchoice <- reactive (if (!is.null(GETterms()$tab) && (GETterms()$tab %in% 1:length(tab.choices)))
                                       tab.choices[as.numeric(GETterms()$tab)] else tab.choices[1])

    output$onload.tabset <- renderUI(
        tabsetPanel(id="tabset",
                    tabPanel("Graphical View",
                             fluidRow(
                                 column(3, htmlOutput("onload.xaxis")),
                                 column(3, htmlOutput("onload.yaxis")),
                                 column(3, htmlOutput("onload.caxis")),
                                 column(3, htmlOutput("onload.saxis"))),
                             
                             div (plotOutput(outputId = "scatter", height="700px", width="900px")),
                             sliderInput ("bottomout", "Bottom-Ranked Teams To Omit (Horizontal Axis)", 0, 5, 0)
                             ),
                    
                    tabPanel("Tabular View",
                             dataTableOutput('mytable')
                             ),selected = onload.tabpanelchoice()
                    ) #tabset
        )

    
    score.factor <- reactive(split.score.list[[match(input$scorechoice, split.score.choices)]])
    man.factor <- reactive(split.man.list[[match(input$manchoice, split.man.choices)]])

    full.object <- reactive({
        
        my.daterange <- if (input$datemode) input$daterange else {
            seasonrange <- sort(c(input$startseason,input$endseason))
            c(paste0(substr(seasonrange[1],1,4),"-09-15"),
              paste0(substr(seasonrange[2],5,8),"-06-30"))
        }
        
        gt1 <- team.query (playoffs = input$playoffs %in% c("All", "Playoffs"),
                           regseason = input$playoffs %in% c("All", "Regular"),
                                       
                           daterange = my.daterange,
                           period.set=period.set(),
                           man.factor=man.factor(),
                           score.factor=score.factor(),
                           homechoice = input$homechoice)
                
        return(gt1)   
    })

    datetable <- reactive (full.object()) 
    reduced.datetable <- reactive(datetable())
    
    fulldata <- reactive ({

        dt1 <- as.data.frame(reduced.datetable()) %>% fix.column.names 
        
        ##print(head(dt1,2))
        out <- if (input$splitseasons) {
            if (input$sepgame) {
                dt1 %>% group_by (Team, season, gcode) %>% compress.team
            } else {
                ##dt1$season[substr(dt1$gcode,1,1) == 3] <- paste(dt1$season[substr(dt1$gcode,1,1) == 3], "-Playoffs")
                dt1 %>% group_by (Team, season) %>% compress.team
            }
        } else {
            dt1$Team[dt1$Team %in% c("ATL","WPG")] <- "ATL-WPG"
            dt1$Team[dt1$Team %in% c("PHX","ARI")] <- "PHX-ARI"
            dt1 %>% group_by (Team) %>% compress.team
        }
        
        ##message("prc")
        print(head(out, 2))
        
        out <- add.rates.teams(out)

        ##print(head(out, 2))
        
        #colnames(out)[colnames(out) == "team"] <- "Team"
        out <- out[rev(order(out[["C+/-"]])),]
        ##message("postprc")
        ##print(head(out))
        return(out)
    })
  

    mytable.prime <- reactive ({

        group <- input$whichPosLoc
        out <- fulldata() #teamtable[,c(30:32,1:29)]
        print(dim(out))
        
        out2 <- data.frame(Team=out$Team, Gm=out$Gm)

        print(dim(out2))
        if (input$splitseasons) {
            out2 <- cbind(out2, season=out$season)
            if (input$sepgame) {
                out2 <- out2[,colnames(out2) != "Gm"]
                out2$Date <- gamestest$date[match(paste(out$season, out$gcode),
                                                  paste(gamestest$season, gamestest$gcode))]
            }

        }
        #out2 <- cbind(out2, PDO=out$PDO)

        print(dim(out2))
        
        
        if (group %in% c("Prime", "All"))   
            out2 <- cbind(out2, out[,c("GF","GA","G+/-","CF%","CP60","OFOn%",
                                       "OSh%","OSv%","FO%","PDO","ZSO%")])

        if (group %in% c("High-Danger Chances", "All"))
            out2 <- cbind(out2, out[,c("HSCF%","HSC+/-","HSCF","HSCA","HSCF60","HSCA60","HSCP60")])
    

        if (group %in% c("Scoring Chances", "All"))
            out2 <- cbind(out2, out[,c("SCF%","SC+/-","SCF","SCA","SCF60","SCA60","SCP60")])
        

        if (group %in% c("Corsi", "Fenwick", "All"))
            out2 <- cbind(out2, out[,c("CF%","C+/-","CF","CA","CF60","CA60","CP60",
                                       "FF%","F+/-","FF60","FA60","FF","FA","FP60",
                                       "MSF", "MSA", "BSF","BSA")])

        if (group %in% c("Shot-Based", "Goal-Based", "All"))
            out2 <- cbind(out2, out[,c("SF%","S+/-", "SF60","SA60", "SF","SA",
                                       "GF60", "GA60","GF%", "OSh%", "OFenSh%", "OSv%",
                                       "OCOn%", "OFOn%")])

                
        if (group %in% c("Faceoffs", "Base Counts", "All"))
            out2 <- cbind(out2, out[,c("FO%", "FO_W", "FO_L",
                                       "ZSO","ZSN","ZSD", "HIT", "HIT-",
                                       "PN", "PN-", "PenD", "TOI")])
    
        numeric.columns <- sapply(out2, class) 
        ##print(numeric.columns)
        out2[,numeric.columns=="numeric"] <- round(out2[,numeric.columns=="numeric"], 1)
        out2#[,unique(colnames(out2))]

    })
  
    output$mytable = renderDataTable({
        output <- mytable.prime()
        output$Team <- teamtime.url(output$Team)
        if (input$sepgame & input$splitseasons) output$Date <- woigame.url(output$Date, paste0(fulldata()$season, fulldata()$gcode))

        colnames(output) <- swapout.oldnew (colnames(output), team.short.match()) %>% augment.html.div (team.3.variables())
                    
        ##colnames(output) <- augment.html.div(colnames(output), team.variables)
        output
    }       
        ,options=list(searching=FALSE, scrollX = "100%", scrollY = "100%",
             lengthChange = FALSE, pageLength=30)
        )
    output$downloadData <- downloadHandler (
        filename = paste0("war-on-ice-", Sys.time(),".csv"),
        content = function (file) write.csv (mytable.prime(), file)
        )
  
  
    output$scatter = renderPlot ({
        myplan <- layout (matrix(c(2,1), nrow=1, ncol=2),
                          widths=c(1, 6.5), heights=c(6))
    
    ## Main plot.
        legend.length <- 25
        message ("Starting render for scatter")
        
        matchers <- match(c(input$xaxis, input$yaxis, input$caxis, input$saxis), team.3.variables()[,2])
        
        pieces.1 <- team.3.variables()[matchers, 1]
        pieces <- team.short.match()[match(pieces.1, team.short.match()[,2]), 1]

        subdata <- fulldata()[,c(pieces, "Team", "season")]
        subdata <- subdata[order(subdata[[1]]),]
        if (input$bottomout > 0) subdata <- subdata[-(1:input$bottomout),]
        
        datacat <-  team.3.variables()[matchers, 3]
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

        xrange <- range (subdata[[1]])
        xlim1 <- c(-(xrange[2]-xrange[1]), (xrange[2]-xrange[1]))/20 + xrange
        yrange <- range (subdata[[2]])
        ylim1 <- c(-(yrange[2]-yrange[1]), (yrange[2]-yrange[1]))/20 + yrange
        
        this.cex <- 15*(subdata[[4]]/max(subdata[[4]]))
        plot (subdata[[1]], subdata[[2]], xlab=input$xaxis, ylab=input$yaxis,
              cex=this.cex, col=this.col, pch=16,
              ##main=paste(input$yaxis, "vs.", input$xaxis)
              main = paste(input$xaxis, " vs. ", input$yaxis, "\nColored by ",
                  input$caxis, ", Sized by ", input$saxis, sep = ""),
              xlim=xlim1, ylim=ylim1
              )
        points (subdata[[1]], subdata[[2]], cex=this.cex)
        
        labels <- if (input$splitseasons) paste(subdata[["Team"]], subdata[["season"]], sep="-") else subdata[["Team"]]
        ## print(labels)
        
        text (subdata[[1]], subdata[[2]], labels, cex=(1.2 + 1.2*!input$splitseasons))
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
  
})

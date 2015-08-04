
source("app.R")

######################################################################
##
## server components

server <- shinyServer(function(input, output, session){

    GETterms <- reactive({g1 <- split.GET.to.frame(session$clientData$url_search); g1})
   
    onload.d <- reactive (if (!is.null(GETterms()$team) && any(grepl(GETterms()$team, team.list)))
                          grep(GETterms()$team, team.list, value=TRUE) else "N.J")
    output$nameSelect1 <- renderUI(selectInput (inputId="player1", label="Select Team",
                                           selected = onload.d(), choices = team.list))

    #searchResult1 <- reactive(team.list[grep(tolower(input$nameSearch1), tolower(team.list))])
    #output$nameSelect1 <- renderUI(selectInput ("player1", "", searchResult1()))

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

    
    outdest <- reactive(paste0 ("http://war-on-ice.com/hexteams.html?",
                                "team=",input$player1,
                                "&start0=",input$startseason,
                                "&end0=",input$endseason))

    output$shareDestination <- renderText(outdest())

    ## #############################################################################
    ## Options!

    ## scatter.grid <- function () point.grid()
    scatter.grid <- reactive(point.grid())

    pick <- reactive(sort(match(input$startseason, seasons):match(input$endseason, seasons)))
    seasonpick <- reactive({
        sort(seasons[match(c(input$startseason, input$endseason), seasons)])
    })
    this.shottype <- reactive (if (input$shotclass == "All Shots") shottypes else input$shotclass)
    
    all.data <- reactive({
        ##db.hex <- dbConnect(SQLite(), dbname="common-data/hextally.sqlite")
        ##input <- list(player1="TOR", startseason="20122013", endseason="20142015", homeawayall="All", includeblockmiss=TRUE, includeplayoffs=FALSE, mansituation="Power Play", superbins=TRUE); seasonpick <- function() c("20122013","20132014","20142015")

        reduced.connect <- src_sqlite("../common-data/hextally.sqlite")
        sqlstatement <- paste0('SELECT * FROM shotsteam WHERE Team = "',input$player1,'" ')
        primer <- tbl(reduced.connect, sql(sqlstatement)) %>% filter(season >= seasonpick()[1], season <= seasonpick()[2]) %>% as.data.frame
        sqlstatement2 <- paste0('SELECT * FROM TOIteam WHERE Team = "',input$player1,'" ')
        TOIteam <- tbl(reduced.connect, sql(sqlstatement2)) %>% filter(season >= seasonpick()[1], season <= seasonpick()[2]) %>% as.data.frame
        sqlstatement3 <- paste0('SELECT * FROM baselinedata')
        baseline <- tbl(reduced.connect, sql(sqlstatement3)) %>% filter(season >= seasonpick()[1], season <= seasonpick()[2]) %>% as.data.frame %>% mutate(danger = danger.zone[new.loc.section + 1])

        ## all.data <- function () list(shots=primer, TOI=TOIteam, baseline=baseline)
        return(list(shots=primer, TOI=TOIteam, baseline=baseline))
    })

    
    reduced.data <- reactive({

        ## compress by area.
        these.shots <- all.data()$shots
        if (!input$includeblockmiss) these.shots <- filter (these.shots, etype %in% c("SHOT","GOAL"))
        if (!input$includeplayoffs) these.shots <- filter (these.shots, substr(gcode,1,1) == 2)
        homeaway <- list(1, 0, 0:1)[[match(input$homeawayall, c("Home", "Away", "All"))]]
        these.shots <- filter (these.shots, home %in% homeaway, type %in% this.shottype())
        
        o.state <- c(1,2,4)[match(input$mansituation, c("Even Strength", "Power Play", "Four On Four"))]
        d.state <- c(1,2,4)[match(input$mansituation, c("Even Strength", "Power Play", "Four On Four"))]

        print(c(o.state, d.state))
        
        shots.for.table <- these.shots %>% filter(role=="O", gamestate==o.state) %>%
            group_by(hexblock) %>% summarize(count=n(), goals=sum(etype=="GOAL")) %>% filter (!is.na(hexblock))
        shots.for.count <- cbind(rep(0, length(scatter.grid()$zone.section)),
                                 rep(0, length(scatter.grid()$zone.section)))
        shots.for.count[shots.for.table$hexblock,1] <- shots.for.table$count
        shots.for.count[shots.for.table$hexblock,2] <- shots.for.table$goals
        
        shots.against.table <- these.shots %>% filter(role=="D", gamestate==d.state) %>%
            group_by(hexblock) %>% summarize(count=n(), goals=sum(etype=="GOAL")) %>% filter (!is.na(hexblock))
        shots.against.count <- cbind(rep(0, length(scatter.grid()$zone.section)),
                                     rep(0, length(scatter.grid()$zone.section)))
        shots.against.count[shots.against.table$hexblock,1] <- shots.against.table$count
        shots.against.count[shots.against.table$hexblock,2] <- shots.against.table$goals

        shots.bins <- data.frame(sfc=shots.for.count[,1],
                                 gfc=shots.for.count[,2],
                                 sac=shots.against.count[,1],
                                 gac=shots.against.count[,2],
                                 zs=scatter.grid()$zone.section,
                                 zs2=scatter.grid()$zone.section.bigblock)
        
        shots.16 <- shots.bins %>% group_by(zs) %>%
            summarize(shotsfor=sum(sfc), shotsagn=sum(sac), goalsfor=sum(gfc), goalsagn=sum(gac))
        shots.3 <- shots.bins %>% group_by(zs2) %>%
            summarize(shotsfor=sum(sfc), shotsagn=sum(sac), goalsfor=sum(gfc), goalsagn=sum(gac))
                
        TOI <- all.data()$TOI
        time.O <- sum(TOI$TOI[TOI$gamestate == o.state & TOI$home %in% homeaway])
        time.D <- sum(TOI$TOI[TOI$gamestate == d.state & TOI$home %in% homeaway])

        baseline <- all.data()$baseline
        ref.time.O <- sum(baseline$TOI[baseline$new.loc.section == 0 & baseline$gamestate == o.state])
        ref.time.D <- sum(baseline$TOI[baseline$new.loc.section == 0 & baseline$gamestate == d.state])

        if (!input$includeblockmiss) {baseline$BLOCK <- 0; baseline$MISS <- 0}
        
        ref.shots.O <- baseline %>% filter (gamestate == o.state) %>% group_by (new.loc.section) %>%
            summarize (shots = sum(BLOCK+SHOT+MISS+GOAL), goals=sum(GOAL)) %>%
                filter(new.loc.section > 0) %>% rename (bin = new.loc.section)
        ref.shots.D <- baseline %>% filter (gamestate == d.state) %>% group_by (new.loc.section) %>%
            summarize (shots = sum(BLOCK+SHOT+MISS+GOAL), goals=sum(GOAL)) %>%
                filter(new.loc.section > 0) %>% rename (bin = new.loc.section)
        ref.shots.O3 <- baseline %>% filter (gamestate == o.state) %>% group_by (danger) %>%
            summarize (shots = sum(BLOCK+SHOT+MISS+GOAL), goals=sum(GOAL)) %>%
                filter(danger > 0) %>% rename (bin = danger)
        ref.shots.D3 <- baseline %>% filter (gamestate == d.state) %>% group_by (danger) %>%
            summarize (shots = sum(BLOCK+SHOT+MISS+GOAL), goals=sum(GOAL)) %>%
                filter(danger > 0) %>% rename (bin = danger)

        if (input$superbins)
            output <- list(shots.for.count=shots.for.count,
                           shots.against.count=shots.against.count,
                           shots.group=shots.3,
                           time.O=time.O, time.D=time.D,
                           ref.time.O=ref.time.O, ref.time.D=ref.time.D,
                           ref.shots.O=ref.shots.O3, ref.shots.D=ref.shots.D3) else
            output <- list(shots.for.count=shots.for.count,
                           shots.against.count=shots.against.count,
                           shots.group=shots.16,
                           time.O=time.O, time.D=time.D,
                           ref.time.O=ref.time.O, ref.time.D=ref.time.D,
                           ref.shots.O=ref.shots.O, ref.shots.D=ref.shots.D)

        ## reduced.data <- function () output
        return(output)
        
    })


    output$guidepostgrand <- renderPlot ({

        par(mar=c(0,0,3,0))
        rink.hexplot (scatter.grid(), sizes=0*reduced.data()$shots.for.count,     ##0*player.counts()[[1]][,1],
                      main=paste("Success By Region: Total"),
                      colors=0*reduced.data()$shots.for.count)  ##0*player.counts()[[1]][,1])    #absolute.binning(player.counts()[,3])
        apply(quadsarrayplot[1:4,,], 3, polygon, border=3, col=NA, lwd=2)
        to.print <- paste0(2*reduced.data()$ref.shots.O[["goals"]],"/",2*reduced.data()$ref.shots.O[["shots"]])    #       ref.counts()[[2]][,2]
        locs <- quad.centers; if (input$superbins) locs <- locs[c(1,6,11),]
        text (locs, to.print, cex=0.8)
        
    })
    
    
    output$guidepost <- renderPlot ({
        par(mar=c(0,0,3,0))
        rink.hexplot (scatter.grid(), sizes=0*reduced.data()$shots.for.count[,1],
                      main=paste("Success By Region, ", input$player1),
                      colors=0*reduced.data()$shots.for.count)    #absolute.binning(player.counts()[,3])
        poly.colors <- if (input$superbins) c("#FFFFDD","#FFDDFF","#DDFFFF") else rep(NA, dim(quadsarrayplot)[3])
        sapply(1:dim(quadsarrayplot)[3], function(ss) polygon(quadsarrayplot[1:4,,ss], border=3, col=poly.colors[three.superblocks()[ss]], lwd=2))
        locs <- quad.centers; if (input$superbins) locs <- locs[c(1,6,11),]
        to.print <- paste0(reduced.data()$shots.group[["goalsfor"]], "/", reduced.data()$shots.group[["shotsfor"]])
        text (locs, to.print, cex=1.3)
    })
  

    output$shootingcount <- renderPlot({
        base.colors <- absolute.binning(reduced.data()$ref.shots.O[["goals"]]/
                                        reduced.data()$ref.shots.O[["shots"]])
        colors <- if (input$superbins) base.colors[scatter.grid()$zone.section.bigblock] else base.colors[scatter.grid()$zone.section]
        par(mar=c(0,0,2,0))
        rink.hexplot.auto (scatter.grid(), sizes=rawcounts, ##reduced.data()$shots.for.count,
                           colors=colors, main="League-Wide Success Rate")
    })
    
 
  ## Tab 1, rate plots.
    output$rateplots <- renderPlot ({
        myplan <- layout (matrix(c(2,0,0, 1,3,4), nrow=2, ncol=3, byrow=TRUE),
                          widths=c(5, 0.7, 5), heights=c(1, 6))
        par(mar=c(0,0,2,0))
        
        quant1.prime <- reduced.data()$shots.group[["shotsfor"]]/reduced.data()$time.O
        quant2.prime <- reduced.data()$ref.shots.O[["shots"]]/reduced.data()$ref.time.O
        print(quant1.prime / quant2.prime)
        if (length(quant1.prime) == 3) {
            quant1 <- quant1.prime[scatter.grid()$zone.section.bigblock]
            quant2 <- quant2.prime[scatter.grid()$zone.section.bigblock]
        } else {
            quant1 <- quant1.prime[scatter.grid()$zone.section]
            quant2 <- quant2.prime[scatter.grid()$zone.section]
        }
        
        cols <- if (input$shotclass == "All Shots") {
            if (input$zscoresonplot)
                z.score.binning(z.score.poisson(quant1, quant2)) else
            relative.binning(quant1/quant2)
        } else rep("#22DD22", length(quant1))
        
        rink.hexplot (scatter.grid(), sizes=reduced.data()$shots.for.count[,1],   ##player.counts()[[1]][,1],
                      colors=cols, main=paste("For", input$player1))  #,input$numbersonplot
        if (input$numbersonplot) { # & !input$superbins
            ratio <- quant1.prime/quant2.prime
            ##ratio <- (player.counts()[[2]][,1]/ref.counts()[[2]][,1])*(total.time()/player.time.O())
            locs <- quad.centers; if (input$superbins) locs <- locs[c(1,6,11),]
            if (length(ratio) != nrow(locs)) ratio <- rep(0, nrow(locs))
            text (locs, as.character(signif(ratio, 3)), cex=2, col="black")  
        }
        legend ("bottomright", "war-on-ice.com", bty="n", cex=2)
    
        ## plot 2: Title
        par(mar=c(0,0,0,0))
        plot(c(0,10), c(0,1), ty="n", axes=FALSE)
        text(5, 0.5, paste("Shot Rates Relative To League Average"), cex=2)
        
        ## plot 3: legend
        if (input$shotclass == "All Shots") {
            if (input$zscoresonplot) z.score.spectrum(xlab="Blue: Worse Team Offense -- Red: Better Team Offense") else relative.spectrum(xlab="Blue: Worse Team Offense -- Red: Better Team Offense")
        } else plot(c(0,10), c(0,1), ty="n", axes=FALSE)
        
    
        ## plot 4 rateagainst
        par(mar=c(0,0,2,0))

        quant1.prime <- reduced.data()$shots.group[["shotsagn"]]     ##player.counts()[[1]][,4 + 2*input$superbins]
        quant2.prime <- reduced.data()$ref.shots.D[["shots"]]/reduced.data()$ref.time.D*reduced.data()$time.D    ##ref.counts()[[1]][,4 + 2*input$superbins]/total.time()*player.time.O()
        if (length(quant1.prime) == 3) {
            quant1 <- quant1.prime[scatter.grid()$zone.section.bigblock]
            quant2 <- quant2.prime[scatter.grid()$zone.section.bigblock]
        } else {
            quant1 <- quant1.prime[scatter.grid()$zone.section]
            quant2 <- quant2.prime[scatter.grid()$zone.section]
        }
        cols <- if (input$shotclass == "All Shots") {
            if (input$zscoresonplot) z.score.binning(z.score.poisson(quant1, quant2)) else relative.binning(quant1/quant2) } else rep("#22DD22", length(quant1))
    
        rink.hexplot (scatter.grid(), sizes=reduced.data()$shots.against.count[,1],
                      colors=cols, main=paste("Against", input$player1))
        if (input$numbersonplot) {  #& !input$superbins
            ratio <- quant1.prime/quant2.prime
            locs <- quad.centers; if (input$superbins) locs <- locs[c(1,6,11),]
            if (length(ratio) != nrow(locs)) ratio <- rep(0, nrow(locs))
            text (locs, as.character(signif(ratio, 3)), cex=2, col="black")  
        }
        legend ("bottomright", "war-on-ice.com", bty="n", cex=2)

    })

    

    ## Tab 2: relative success plots.
    output$relativesuccessplots <- renderPlot ({
        
        ##myplan <- layout (matrix(c(0,2,5, 3,1,4), nrow=2, ncol=3, byrow=TRUE),
        ##                  widths=c(0.7, 5, 5), heights=c(1, 6))
        myplan <- layout (matrix(c(2,0,0, 1,3,4), nrow=2, ncol=3, byrow=TRUE),
                          widths=c(5, 0.7, 5), heights=c(1, 6))
        
        ## plot 1: ratefor
        par(mar=c(0,0,2,0))

        quant1.prime <- reduced.data()$shots.group[["goalsfor"]]/reduced.data()$shots.group[["shotsfor"]]   
        quant2.prime <- reduced.data()$ref.shots.O[["goals"]]/reduced.data()$ref.shots.O[["shots"]]   
        message ("For relative success ",paste(quant1.prime / quant2.prime, collapse=" "))
        if (length(quant1.prime) == 3) {
            quant1 <- quant1.prime[scatter.grid()$zone.section.bigblock]
            quant2 <- quant2.prime[scatter.grid()$zone.section.bigblock]
        } else {
            quant1 <- quant1.prime[scatter.grid()$zone.section]
            quant2 <- quant2.prime[scatter.grid()$zone.section]
        }

        cols <- if (input$zscoresonplot) {
            run1 <- z.score.binning(z.score.binomial(cbind(reduced.data()$shots.group[["shotsfor"]],
                                                           reduced.data()$shots.group[["goalsfor"]]),
                                                     cbind(reduced.data()$ref.shots.O[["shots"]],
                                                           reduced.data()$ref.shots.O[["goals"]])))
            if (length(quant1.prime) == 3) run1[scatter.grid()$zone.section.bigblock] else run1[scatter.grid()$zone.section]
        } else relative.binning(quant1/quant2)
        
        rink.hexplot (scatter.grid(), sizes=reduced.data()$shots.for.count[,1],
                      colors=cols, main=paste("For", input$player1))
        if (input$numbersonplot) {
            ratio <- quant1.prime/quant2.prime
            locs <- quad.centers; if (input$superbins) locs <- locs[c(1,6,11),]
            text (locs, as.character(signif(ratio, 3)), cex=2, col="black")  
        }
        legend ("bottomright", "war-on-ice.com", bty="n", cex=2)
        
        ## plot 2: Title
        par(mar=c(0,0,0,0))
        plot(c(0,10), c(0,1), ty="n", axes=FALSE)
        text(5, 0.5, paste("Shooting Percentages Relative To League Average"), cex=2)
        
        ## plot 3: legend
        if (input$zscoresonplot) z.score.spectrum(xlab="Blue: Worse Team Offense -- Red: Better Team Offense") else relative.spectrum(xlab="Blue: Worse Team Offense -- Red: Better Team Offense")



    
        ## plot 4: rateagainst
        par(mar=c(0,0,2,0))

        quant1.prime <- reduced.data()$shots.group[["goalsagn"]]/reduced.data()$shots.group[["shotsagn"]]   
        quant2.prime <- reduced.data()$ref.shots.D[["goals"]]/reduced.data()$ref.shots.D[["shots"]]   
        message ("Against relative success ",paste(quant1.prime / quant2.prime, collapse=" "))
        if (length(quant1.prime) == 3) {
            quant1 <- quant1.prime[scatter.grid()$zone.section.bigblock]
            quant2 <- quant2.prime[scatter.grid()$zone.section.bigblock]
        } else {
            quant1 <- quant1.prime[scatter.grid()$zone.section]
            quant2 <- quant2.prime[scatter.grid()$zone.section]
        }

        cols <- if (input$zscoresonplot) {
            run1 <- z.score.binning(z.score.binomial(cbind(reduced.data()$shots.group[["shotsagn"]],
                                                           reduced.data()$shots.group[["goalsagn"]]),
                                                     cbind(reduced.data()$ref.shots.D[["shots"]],
                                                           reduced.data()$ref.shots.D[["goals"]])))
            if (length(quant1.prime) == 3) run1[scatter.grid()$zone.section.bigblock] else run1[scatter.grid()$zone.section]
        } else relative.binning(quant1/quant2)

        rink.hexplot (scatter.grid(), sizes=reduced.data()$shots.against.count[,1], 
                      colors=cols, main=paste("Against", input$player1))
        if (input$numbersonplot) {
            ratio <- quant1.prime/quant2.prime
            locs <- quad.centers; if (input$superbins) locs <- locs[c(1,6,11),]
            text (locs, as.character(signif(ratio, 3)), cex=2, col="black")  
        }
        legend ("bottomright", "war-on-ice.com", bty="n", cex=2)

    })

    
    output$absolutesuccessplots <- renderPlot ({

#    myplan <- layout (matrix(c(0,2,3,1), 2, 2, byrow=TRUE),
#                      widths=c(1, 6), heights=c(1, 5))
        myplan <- layout (matrix(c(2,0,0, 1,3,4), nrow=2, ncol=3, byrow=TRUE),
                          widths=c(5, 0.7, 5), heights=c(1, 6))

        ## 1: hexplot.
        par(mar=c(0,0,2,0))

        quant1.prime <- reduced.data()$shots.group[["goalsfor"]]
        quant2.prime <- reduced.data()$shots.group[["shotsfor"]]
        if (length(quant1.prime) == 3) {
            quant1 <- quant1.prime[scatter.grid()$zone.section.bigblock]
            quant2 <- quant2.prime[scatter.grid()$zone.section.bigblock]
        } else {
            quant1 <- quant1.prime[scatter.grid()$zone.section]
            quant2 <- quant2.prime[scatter.grid()$zone.section]
        }
        
        rink.hexplot (scatter.grid(), sizes=reduced.data()$shots.for.count[,1],
                      colors=absolute.binning(quant1/quant2),
##      player.counts()[[1]][,5 + 2*input$superbins]/player.counts()[[1]][,4 + 2*input$superbins]),
                      main=paste("For", input$player1))
        legend ("bottomright", "war-on-ice.com", bty="n", cex=2)
    #legend ("topright", "war-on-ice.com", bty="n")
    ## test for sync.
    
        ## 2: Title.
        par(mar=c(0,0,0,0))
        plot(c(0,10), c(0,1), ty="n", axes=FALSE)
        text(5, 0.5, paste("Absolute Shooting Percentage"), cex=2)
        
        ## 3: spectrum guide
        absolute.spectrum(xlab="Darker: Higher Probability of Success")
        
        ## 4: hexplot.
        par(mar=c(0,0,2,0))
        quant1.prime <- reduced.data()$shots.group[["goalsagn"]]
        quant2.prime <- reduced.data()$shots.group[["shotsagn"]]
        if (length(quant1.prime) == 3) {
            quant1 <- quant1.prime[scatter.grid()$zone.section.bigblock]
            quant2 <- quant2.prime[scatter.grid()$zone.section.bigblock]
        } else {
            quant1 <- quant1.prime[scatter.grid()$zone.section]
            quant2 <- quant2.prime[scatter.grid()$zone.section]
        }
        
        rink.hexplot (scatter.grid(),sizes=reduced.data()$shots.against.count[,1],
                      colors=absolute.binning(quant1/quant2),
                      main=paste("Against", input$player1))
        legend ("bottomright", "war-on-ice.com", bty="n", cex=2)
        ## 5: spectrum guide
        ##absolute.spectrum(xlab="Darker: Higher Probability of Success")
        
    })
    
    rate.minutes <- reactive ({
        timeonice <- cbind(c(reduced.data()$time.O, reduced.data()$time.D))/60
        rownames(timeonice) <- c("Offense", "Defense")
        colnames(timeonice) <- "Minutes"
        return(timeonice)
    })
    
    output$rateminutes <- renderTable ({
        r1 <- rate.minutes()
        if (input$mansituation == "Power Play") rownames(r1) <- c("Power Play","Penalty Kill")
        r1
    })
  
})

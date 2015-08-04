
## hextally-player

library(shiny)
library(MASS)
source("global.r")

allshotdata <- lapply(seasons, function(ss) {
    load (paste0("../common-data/hextally-players-",ss,".RData"))
    list(es.quants.p=es.quants.p,     es.quants.s=es.quants.s,
         four.quants.p=four.quants.p, four.quants.s=four.quants.s,
         pp.quants.p=pp.quants.p,     pp.quants.s=pp.quants.s)
})
scatter.grid <- allshotdata[[1]]$es.quants.s$scatter.grid

######################################################################################

shinyServer(function(input, output, session){

    ## User input stage.
    GETterms <- reactive({g1 <- split.GET.to.frame(session$clientData$url_search); print(g1); g1})
    
    ## reduce player list to the actives.
    reduced.roster <- reactive({
        subset(roster.unique, pos != "G")
    })
    #reduced.roster <- reactive(roster.unique)
    playercycle <- c("kessel","tavares","thornton","crosby","ovechkin")
    onload.nameSearch1 <-
        reactive(if (!is.null(GETterms()$name)) gsub("0"," ", GETterms()$name) else sample(playercycle,1))
    output$onload.nameSearch1 <- renderUI(textInput("nameSearch1", "Search For Player", onload.nameSearch1()))

    searchResult1 <- reactive(reduced.roster()$firstlastpos[grep(tolower(input$nameSearch1),
                                                                 tolower(reduced.roster()$firstlastpos))])
    output$nameSelect1 <- renderUI(selectInput ("player1", "", searchResult1()))
    
    pn <- reactive (roster.unique$player.id[grep(paste0("^",input$player1,"$"), roster.unique$firstlastpos)])

     
    onload.mansituation <- reactive (if (!is.null(GETterms()$mansit) && (GETterms()$mansit %in% 1:length(man.choices)))
                              man.choices[as.numeric(GETterms()$mansit)] else "Even Strength")
    output$onload.manchoice <- renderUI(
        selectInput ("mansituation", "Man Situation", man.choices, onload.mansituation()))

#        selectInput(inputId="manchoice", label="Team Strengths", selected = onload.a(), choices=man.choices))
    
#    onload.tabpanelchoice <- reactive (if (!is.null(GETterms()$tab) &&
#                                           (GETterms()$tab %in% 1:length(tab.options)))
#                                       tab.options[as.numeric(GETterms()$tab)] else tab.options[1])

    alt.text <- reactive(c("","","",""))
       # ms <- match(input$mansituation, mansit.options)
       # if (ms %% 2 == 1)
     #       c("Top: Shooting Rates For Team, With minus Without (Red is Better)\nBottom: Shooting Rates Against Team, With minus Without (Blue is Better)",
     #         "Top: Shooting Rates For Team, With Player, Compared To League Average (Red is Better)\nBottom: Shooting Rates For Team, Without Player (Red is Better)",
     #         "Top: Shooting Rates Against Team, With Player, Compared To League Average (Blue is Better)\nBottom: Shooting Rates Against Team, Without (Blue is Better)",
     #         "Top: Shooting Success For Player, Absolute\nBottom: Shooting Success For Player, Relative To League Average") else
     #       c("Top: Shooting Rates For Team on Power Play, With minus Without (Red is Better)\nBottom: Shooting Rates Against A Shorthanded Team, With minus Without (Blue is Better)",
     #         "Top: Shooting Rates For Team on Power Play, With Player, Compared To League Average (Red is Better)\nBottom: Shooting Rates For Team on Power Play, Without Player (Red is Better)",
     #         "Top: Shooting Rates Against A Shorthanded Team, With Player, Compared To League Average (Blue is Better)\nBottom: Shooting Rates Against A Shorthanded Team, Without (Blue is Better)",
     #         "Top: Shooting Success For Player, Absolute\nBottom: Shooting Success For Player, Relative To League Average") 
    #})

    outdest <- reactive(paste0 ("http://war-on-ice.com/hexplayers.html?",
                                "name=",gsub(" ","0",input$player1),
                                "&start0=",input$startseason,
                                "&end0=",input$endseason,
                                "&mansit=",which(input$mansituation == man.choices)
                                #,"&tab=",which(input$righttabs == tab.options)
                                ))
    output$shareDestination <- renderText(outdest())
    
    onload.startdate <- reactive (if (!is.null(GETterms()$start0) &&
                                      (GETterms()$start0 %in% seasons))
                                  GETterms()$start0 else "20132014")
    onload.enddate <- reactive (if (!is.null(GETterms()$end0) &&
                                    (GETterms()$end0 %in% seasons))
                                GETterms()$end0 else "20142015")
    output$onload.startseason <-
        renderUI(selectInput(inputId="startseason", label="Starting Season",
                             selected = onload.startdate(), choices=seasons))
    output$onload.endseason <-
        renderUI(selectInput(inputId="endseason", label="Ending Season",
                             selected = onload.enddate(), choices=seasons))
    
    pick <- reactive({
        sort(match(input$startseason, seasons):match(input$endseason, seasons))
    })

  ## Operations.
  
  #game object

    ##input <- list(mansituation = "Even Strength", include.playoffs=FALSE); pick=6:7
    go <- reactive ({
        message ("Reach 1")
        gg1 <- if (input$mansituation == "Even Strength")
            c(lapply(allshotdata[pick()], function(ll) ll$es.quants.s),
              if (input$include.playoffs) lapply(allshotdata[pick()[pick()!=10]], function(ll) ll$es.quants.p) else NULL) else
        if (input$mansituation == "Power Play")
            c(lapply(allshotdata[pick()], function(ll) ll$pp.quants.s),
              if (input$include.playoffs) lapply(allshotdata[pick()[pick()!=10]], function(ll) ll$pp.quants.p) else NULL) else
        c(lapply(allshotdata[pick()], function(ll) ll$four.quants.s),
          if (input$include.playoffs) lapply(allshotdata[pick()[pick()!=10]], function(ll) ll$four.quants.p) else NULL)[1:13]

        message ("Reach 2")
        
        gg2 <- list(
            shots=do.call(rbind, lapply(gg1, function(ll) ll$shots)),
            
            
            time.on.ice.home.O=rowSums(do.call(cbind, lapply(gg1, function(ll) ll$time.on.ice.home.O))),
            time.on.ice.home.D=rowSums(do.call(cbind, lapply(gg1, function(ll) ll$time.on.ice.home.D))),
            time.on.ice.away.O=rowSums(do.call(cbind, lapply(gg1, function(ll) ll$time.on.ice.away.O))),
            time.on.ice.away.D=rowSums(do.call(cbind, lapply(gg1, function(ll) ll$time.on.ice.away.D))),
            
            time.off.ice.home.O=rowSums(do.call(cbind, lapply(gg1, function(ll) ll$time.off.ice.home.O))),
            time.off.ice.home.D=rowSums(do.call(cbind, lapply(gg1, function(ll) ll$time.off.ice.home.D))),
            time.off.ice.away.O=rowSums(do.call(cbind, lapply(gg1, function(ll) ll$time.off.ice.away.O))),
            time.off.ice.away.D=rowSums(do.call(cbind, lapply(gg1, function(ll) ll$time.off.ice.away.D))),

            home.played=lapply(1:length(gg1[[1]]$home.played), function(this.player)
                unlist(sapply(1:length(gg1), function(this.block) gg1[[this.block]]$home.played[[this.player]]))),
            away.played=lapply(1:length(gg1[[1]]$home.played), function(this.player)
                unlist(sapply(1:length(gg1), function(this.block) gg1[[this.block]]$away.played[[this.player]]))),

            total.time.by.game.home.O=do.call(rbind, lapply(gg1, function(ll) ll$total.time.by.game.home.O)),
            total.time.by.game.away.O=do.call(rbind, lapply(gg1, function(ll) ll$total.time.by.game.away.O)),

            ##  "total.time.by.game.home.O" "total.time.by.game.away.O"
            
            seasonal.counts=sumup(lapply(gg1, function(ll)
                if (length(ll$seasonal.counts) > 0) ll$seasonal.counts[[1]] else NULL)),
            seasonal.bin.counts=sumup(lapply(gg1, function(ll)
                if (length(ll$seasonal.bin.counts) > 0) ll$seasonal.bin.counts[[1]] else NULL)),
            seasonal.superbin.counts=sumup(lapply(gg1, function(ll)
                if (length(ll$seasonal.superbin.counts) > 0) ll$seasonal.superbin.counts[[1]] else NULL))
            )
        message ("Reach 3")
        
        gg2$shots$ev.team <- swapout(gg2$shots$ev.team)
        gg2$shots$hometeam <- swapout(gg2$shots$hometeam)
        gg2$shots$awayteam <- swapout(gg2$shots$awayteam)
        #rownames(gg2$time.on.ice.home.O) <- swapout(rownames(gg2$time.on.ice.home.O))
        #rownames(gg2$time.on.ice.home.D) <- swapout(rownames(gg2$time.on.ice.home.D))
        #rownames(gg2$time.on.ice.away.O) <- swapout(rownames(gg2$time.on.ice.away.O))
        #rownames(gg2$time.on.ice.away.D) <- swapout(rownames(gg2$time.on.ice.away.D))
        ##save(gg1, gg2, file="test-output.RData")
        gg2
    })

    this.shottype <- reactive (if (input$shotclass == "All Shots") shottypes else input$shotclass)
    coordset <- reactive(if (input$useimputedlocations) c("newyc", "newxc") else c("ycoord","xcoord"))
    
    home.sub <- reactive (subset(go()$shots, paste0(season, gcode) %in% go()$home.played[[pn()]]))
    away.sub <- reactive (subset(go()$shots, paste0(season, gcode) %in% go()$away.played[[pn()]]))

    goshots <- reactive ({
        output <- if (input$homeawayall == "Away") away.sub() else if (input$homeawayall == "Home") home.sub() else rbind(home.sub(), away.sub())
        ##save(output, file="outgame.RData")
        output
    })
    player.indicator <- reactive ({
        list(home=apply(home.sub()[,c("h1","h2","h3","h4","h5","h6","home.G")] == pn(), 1, sum) > 0,
             away=apply(away.sub()[,c("a1","a2","a3","a4","a5","a6","away.G")] == pn(), 1, sum) > 0)
    })
    

    borrowed.time <- reactive(input$stabilize*60*input$borrowedstrength)
    
    player.time.O <- reactive ({
        quants <- c(0,0) 
        if (input$homeawayall != "Away")
            quants <- quants + c(sum(go()$time.on.ice.home.O[pn()]),
                                 sum(go()$time.off.ice.home.O[pn()]))
        if (input$homeawayall != "Home") 
            quants <- quants + c(sum(go()$time.on.ice.away.O[pn()]),
                                 sum(go()$time.off.ice.away.O[pn()]))
        #print(borrowed.time())
        c(quants + pmax(borrowed.time() - quants, c(0,0)),
          pmax(borrowed.time() - quants, c(0,0)),
          quants)
    })
    player.time.D <- reactive ({
        quants <- c(0,0)
        if (input$homeawayall != "Away")
            quants <- quants + c(sum(go()$time.on.ice.home.D[pn()]),
                                 sum(go()$time.off.ice.home.D[pn()]))
        if (input$homeawayall != "Home") 
            quants <- quants + c(sum(go()$time.on.ice.away.D[pn()]),
                                 sum(go()$time.off.ice.away.D[pn()]))
        c(quants + pmax(borrowed.time() - quants, c(0,0)),
          pmax(borrowed.time() - quants, c(0,0)),
          quants)
    })
    total.time.O <- reactive ({
        quants <- 0
        if (input$homeawayall != "Away") 
            quants <- quants + sum(go()$total.time.by.game.home.O)
        if (input$homeawayall != "Home")
            quants <- quants + sum(go()$total.time.by.game.away.O)
        quants
    })
    total.time.D <- reactive ({
        quants <- 0
        if (input$homeawayall != "Away") 
            quants <- quants + sum(go()$total.time.by.game.away.O)
        if (input$homeawayall != "Home")
            quants <- quants + sum(go()$total.time.by.game.home.O)
        quants
    })
    
    
    ref.counts <- reactive(list(go()$seasonal.counts,
                                if (!input$superbins) go()$seasonal.bin.counts else
                                go()$seasonal.superbin.counts))
    
  
  #Only shot type changes for the shooting map -- the other contrasts don't necessarily make sense.
    player.counts <- reactive ({
        
        player.events <-
            if (roster.unique$pos[pn()] == "G") {
                subset(goshots(),
                       ((away.G == pn() & ev.team == hometeam) | (home.G == pn() & ev.team == awayteam)) &
                       type %in% this.shottype())
            } else {
                subset(goshots(),
                       ev.player.1 == pn() &
                       type %in% this.shottype())
            }
        list(shot.bin.set (player.events, scatter.grid, coordset()),
             shot.bin.set.blocks (player.events, scatter.grid, coordset(), input$superbins))
    })
    
    team.with.for.proto <- reactive ({
        player.events <- rbind(if (input$homeawayall != "Away") subset(home.sub(),
                                       ((player.indicator()[[1]] & ev.team == hometeam))) else NULL,  
                               if (input$homeawayall != "Home") subset(away.sub(),
                                       ((player.indicator()[[2]] & ev.team == awayteam))) else NULL)
        list(shot.bin.set (player.events, scatter.grid, coordset()),
             shot.bin.set.blocks (player.events, scatter.grid, coordset(), input$superbins))
    })
    team.without.for.proto <- reactive ({
        player.events <- rbind(if (input$homeawayall != "Away") subset(home.sub(),
                                       ((!player.indicator()[[1]] & ev.team == hometeam))) else NULL,
                               if (input$homeawayall != "Home") subset(away.sub(),
                                       ((!player.indicator()[[2]] & ev.team == awayteam))) else NULL)
        list(shot.bin.set (player.events, scatter.grid, coordset()),
             shot.bin.set.blocks (player.events, scatter.grid, coordset(), input$superbins))
    })
    
    team.with.against.proto <- reactive ({
        player.events <- rbind(if (input$homeawayall != "Away") subset(home.sub(),
                                       ((player.indicator()[[1]] & ev.team == awayteam))) else NULL,
                               if (input$homeawayall != "Home") subset(away.sub(),
                                       ((player.indicator()[[2]] & ev.team == hometeam))) else NULL)
        list(shot.bin.set (player.events, scatter.grid, coordset()),
             shot.bin.set.blocks (player.events, scatter.grid, coordset(), input$superbins))
    })
    team.without.against.proto <- reactive ({
        player.events <- rbind(if (input$homeawayall != "Away") subset(home.sub(),
                                       ((!player.indicator()[[1]] & ev.team == awayteam))) else NULL,
                               if (input$homeawayall != "Home") subset(away.sub(),
                                       ((!player.indicator()[[2]] & ev.team == hometeam))) else NULL)
        list(shot.bin.set (player.events, scatter.grid, coordset()),
             shot.bin.set.blocks (player.events, scatter.grid, coordset(), input$superbins))
    })
    
    ## Has to be neutral in the difference to be pooled.
    ref.counts.borrowed <- reactive({
    #orig.counts <- ref.counts()
        #print(player.time.O())
        #print(player.time.D())
        list(wi.O.1=team.without.for.proto()[[1]] * player.time.O()[3]/player.time.O()[6],   #total.time.O(),
             wi.O.2=team.without.for.proto()[[2]] * player.time.O()[3]/player.time.O()[6],
             wo.O.1=team.without.for.proto()[[1]] * player.time.O()[4]/player.time.O()[6],
             wo.O.2=team.without.for.proto()[[2]] * player.time.O()[4]/player.time.O()[6],
             wi.D.1=team.without.against.proto()[[1]] * player.time.D()[3]/player.time.D()[6],
             wi.D.2=team.without.against.proto()[[2]] * player.time.D()[3]/player.time.D()[6],
             wo.D.1=team.without.against.proto()[[1]] * player.time.D()[4]/player.time.D()[6],
             wo.D.2=team.without.against.proto()[[2]] * player.time.D()[4]/player.time.D()[6])
    })
    
    team.with.for <- reactive ({
        list(team.with.for.proto()[[1]] + ref.counts.borrowed()[[1]],
             team.with.for.proto()[[2]] + ref.counts.borrowed()[[2]])
    })
    team.without.for <- reactive ({
        list(team.without.for.proto()[[1]] + ref.counts.borrowed()[[3]],
             team.without.for.proto()[[2]] + ref.counts.borrowed()[[4]])
    })
    team.with.against <- reactive ({
        list(team.with.against.proto()[[1]] + ref.counts.borrowed()[[5]],
             team.with.against.proto()[[2]] + ref.counts.borrowed()[[6]])
    })
    team.without.against <- reactive ({
        list(team.without.against.proto()[[1]] + ref.counts.borrowed()[[7]],
             team.without.against.proto()[[2]] + ref.counts.borrowed()[[8]])
    })
                              
  #output$guidepost <- renderPlot ({
  #  par(mar=c(0,0,3,0))
  #  rink.hexplot (scatter.grid, sizes=0*player.counts()[[1]][,1], main=paste("Success By Region, ", input$player1)#,
#                  colors=0*player.counts()[[1]][,1])    #absolute.binning(player.counts()[,3])
#    apply(quadsarrayplot[1:4,,], 3, polygon, border=3, col=NA, lwd=2)
#    text (quad.centers, paste0(player.counts()[[2]][,2],"/",player.counts()[[2]][,1]), cex=1.3)
#  })
  
    output$shootingcount <- renderPlot({
        par(mar=c(0,0,3,0))
        rink.hexplot.auto (scatter.grid, sizes=ref.counts()[[1]][,1],
                           colors=absolute.binning(ref.counts()[[1]][,5 + 2*input$superbins]/
                               ref.counts()[[1]][,4 + 2*input$superbins]), main="League-Wide Success Rate")
    })

    output$guidepostgrand <- renderPlot ({
        par(mar=c(0,0,3,0))
        rink.hexplot (scatter.grid, sizes=0*player.counts()[[1]][,1], main=paste("Success By Region: Total"),
                      colors=0*player.counts()[[1]][,1])    #absolute.binning(player.counts()[,3])
        apply(quadsarrayplot[1:4,,], 3, polygon, border=3, col=NA, lwd=2)
        to.print <- paste0(ref.counts()[[2]][,2],"/",ref.counts()[[2]][,1])
        locs <- quad.centers; if (input$superbins) locs <- locs[c(1,2,11),]
        text (locs, to.print, cex=0.8)
                                        #    text (quad.centers, to.print, cex=0.8)
    })
  
    output$guidepostnames <- renderPlot ({
        par(mar=c(0,0,0,0))
        rink.hexplot (scatter.grid, sizes=0*player.counts()[[1]][,1], main=paste(""),
                      colors=0*player.counts()[[1]][,1])    #absolute.binning(player.counts()[,3])
        apply(quadsarrayplot[1:4,,], 3, polygon, border=3, col=NA, lwd=2)
        to.print <- c("R-Point","C-Point","L-Point",
                      "R-1","R-2","HighSlot","L-2","L-1",
                      "R-Low","R-Slot","Slot","LowSlot","L-Slot","L-Low",
                      "DownLow","NZone")
        text (quad.centers, to.print, cex=0.8)
    })
 
    output$Errorcheckery <- renderPrint ({
##    print(input$righttabs)
        print(pn())
##    print(team.without.for()[[2]][,1]/player.time.O()[2])
##    print(ref.counts()[[2]][,1])
##    print(ref.counts()[[2]][,1]/total.time.O())

    #print(ref.counts()[[2]][,1])
    
        print("Player Time:")
        print(player.time.O())
        print(player.time.D())
        
        print("Total Time:")
        print(total.time.O())
        print(total.time.D())
    
##    print("Bin counts:")
##    print(team.with.for()[[2]][,1])
##    print(team.with.against()[[2]][,1])
##    print(team.without.for()[[2]][,1])
##    print(team.without.against()[[2]][,1])
    
    })

                             

  ## Plot Panel 1. Difference between the two modes!
    output$ratediff <- renderPlot ({
        
#        myplan <- layout (matrix(c(0,2, 3,1, 5,4), nrow=3, ncol=2, byrow=TRUE),
#                          widths=c(0.7, 5), heights=c(1, 6, 6))
        ## title |        | title
        ## plot1 | legend | plot2 

        myplan <- layout (matrix(c(2,0,5, 1,3,4), nrow=2, ncol=3, byrow=TRUE),
                          widths=c(5, 0.7, 5), heights=c(1, 6))
        
        ## plot 1: ratediff for
        par(mar=c(0,0,2,0))
        quant1 <- team.with.for()[[1]][,4 + 2*input$superbins]
        quant2 <- team.without.for()[[1]][,4 + 2*input$superbins]
        cols <- if (input$zscoresonplot)
            z.score.binning(z.score.poisson.two(quant1, quant2, player.time.O()[1], player.time.O()[2])) else
        diff.binning((quant1/player.time.O()[1] - quant2/player.time.O()[2])*3600)
        
        m.main <- if (input$mansituation != "Power Play") "Shot Rate Differential For" else "Shot Rate Differential For, PP"

       ## write.csv(team.with.for()[[1]], "twf.csv")
       ## write.csv(team.with.against()[[1]], "twa.csv")
       ## write.csv(team.without.for()[[1]], "tof.csv")
       ## write.csv(team.without.against()[[1]], "toa.csv")
        
        
        rink.hexplot (scatter.grid, sizes=team.with.for()[[1]][,1], # + team.without.for()[[1]][,1],
                      colors=cols, main=m.main)  #,input$numbersonplot
        if (input$numbersonplot) {
            diff <- (team.with.for()[[2]][,1]/player.time.O()[1] -
                     team.without.for()[[2]][,1]/player.time.O()[2])*3600
            locs <- quad.centers; if (input$superbins) locs <- locs[c(1,2,11),]
            if (length(diff) != nrow(locs)) diff <- rep(0, nrow(locs))
            
            text (locs, paste0(as.character(signif(diff, 3))), cex=2, col="black")
        }
        
        ## plot 2: Title
        par(mar=c(0,0,0,0))
        plot(c(0,10), c(0,1), ty="n", axes=FALSE)
        text(5, 0.5, paste("Shot Rate Differential\nFor",input$player1), cex=2.5)
        
        ## plot 3: legend
        if (input$zscoresonplot) z.score.spectrum(xlab="Blue: Worse Team Offense -- Red: Better Team Offense") else relative.spectrum(xlab="Blue: Worse Team Offense -- Red: Better Team Offense")
        
        ## plot 4 rateagainst
        par(mar=c(0,0,2,0))
        quant1 <- team.with.against()[[1]][,4 + 2*input$superbins]
        quant2 <- team.without.against()[[1]][,4 + 2*input$superbins]
        
        cols <- if (input$zscoresonplot)
            z.score.binning(z.score.poisson.two(quant1, quant2, player.time.D()[1], player.time.D()[2])) else
        diff.binning((quant1/player.time.D()[1] - quant2/player.time.D()[2])*3600)
        m.main <- if (input$mansituation != "Power Play") "Shot Rate Differential Against" else "Shot Rate Differential Against, SH"
        
        rink.hexplot (scatter.grid, sizes=team.with.against()[[1]][,1],# + team.without.against()[[1]][,1],
                      colors=cols, main=m.main)  #,input$numbersonplot
        if (input$numbersonplot) {
            diff <- (team.with.against()[[2]][,1]/player.time.D()[1] -
                     team.without.against()[[2]][,1]/player.time.D()[2])*3600
            locs <- quad.centers; if (input$superbins) locs <- locs[c(1,2,11),]
            if (length(diff) != nrow(locs)) diff <- rep(0, nrow(locs))
            
            text (locs, paste0(as.character(signif(diff, 3))), cex=2, col="black")
        }
        legend ("bottomright", "war-on-ice.com", bty="n", cex=2)
        
        ## plot 5: legend
        ## if (input$zscoresonplot) z.score.spectrum(xlab="Blue: Worse Team Offense -- Red: Better Team Offense") else relative.spectrum(xlab="Blue: Worse Team Offense -- Red: Better Team Offense")
        ## plot 2: Title
        ## plot 2: Title
        par(mar=c(0,0,0,0))
        plot(c(0,10), c(0,1), ty="n", axes=FALSE)
        text(5, 0.5, paste("Shot Rate Differential\nAgainst",input$player1), cex=2.5)
        
    })
  


  ## Plot Panel 2. Rates for, with and without
    output$ratefor <- renderPlot ({

        ## title |        | title
        ## plot1 | legend | plot2 

        myplan <- layout (matrix(c(2,0,5, 1,3,4), nrow=2, ncol=3, byrow=TRUE),
                          widths=c(5, 0.7, 5), heights=c(1, 6))

        ## plot 1: rate with for
        par(mar=c(0,0,2,0))
        quant1 <- team.with.for()[[1]][,4 + 2*input$superbins]
        quant2 <- ref.counts()[[1]][,4 + 2*input$superbins]/total.time.O()*player.time.O()[1]
        cols <- if (input$zscoresonplot)
            z.score.binning(z.score.poisson(quant1, quant2)) else
        relative.binning(quant1/quant2)
        m.main <- if (input$mansituation != "Power Play") "Relative Shot Rate For, With" else "Relative Shot Rate For, With, PP"
        
        rink.hexplot (scatter.grid, sizes=team.with.for()[[1]][,1],
                      colors=cols, main=m.main)  #,input$numbersonplot
        if (input$numbersonplot) {
            ratio <- (team.with.for()[[2]][,1]/ref.counts()[[2]][,1])*(total.time.O()/player.time.O()[1])
            locs <- quad.centers; if (input$superbins) locs <- locs[c(1,2,11),]
            if (length(ratio) != nrow(locs)) ratio <- rep(0, nrow(locs))
            text (locs, as.character(signif(ratio, 3)), cex=2, col="black")  
        }
        
        ## plot 2: Title
        par(mar=c(0,0,0,0))
        plot(c(0,10), c(0,1), ty="n", axes=FALSE)
        text(5, 0.5, paste0("Relative Shot Rate For\n",
                            input$player1), cex=2.5)
        
        ## plot 3: legend
        if (input$zscoresonplot) z.score.spectrum(xlab="Blue: Worse Team Offense -- Red: Better Team Offense") else relative.spectrum(xlab="Blue: Worse Team Offense -- Red: Better Team Offense")
        
        ## plot 4: rate without for 
        par(mar=c(0,0,2,0))
        quant1 <- team.without.for()[[1]][,4 + 2*input$superbins]
        quant2 <- ref.counts()[[1]][,4 + 2*input$superbins]/total.time.O()*player.time.O()[2]
        cols <- if (input$zscoresonplot)
            z.score.binning(z.score.poisson(quant1, quant2)) else
        relative.binning(quant1/quant2)
        m.main <- if (input$mansituation != "Power Play") "Relative Shot Rate For, Without" else "Relative Shot Rate For, Without, PP"
        
        rink.hexplot (scatter.grid, sizes=team.without.for()[[1]][,1],
                      colors=cols, main=m.main)  #,input$numbersonplot
        if (input$numbersonplot) {
            ratio <- (team.without.for()[[2]][,1]/ref.counts()[[2]][,1])*(total.time.O()/player.time.O()[2])
            locs <- quad.centers; if (input$superbins) locs <- locs[c(1,2,11),]
            if (length(ratio) != nrow(locs)) ratio <- rep(0, nrow(locs))
            text (locs, as.character(signif(ratio, 3)), cex=2, col="black")  
        }
        legend ("bottomright", "war-on-ice.com", bty="n", cex=2)
        
        ## plot 5: legend
        ## plot 2: Title
        par(mar=c(0,0,0,0))
        plot(c(0,10), c(0,1), ty="n", axes=FALSE)
        text(5, 0.5, paste0("Relative Shot Rate For\nWithout ",
                            input$player1), cex=2.5)
#        if (input$zscoresonplot) z.score.spectrum(xlab="Blue: Worse Team Offense -- Red: Better Team Offense") else relative.spectrum(xlab="Blue: Worse Team Offense -- Red: Better Team Offense")
    
    ## plot 5: Title
    #par(mar=c(0,0,0,0))
    #plot(c(0,10), c(0,1), ty="n", axes=FALSE)
    #text(5, 0.5, paste(input$player1, "--", input$season), cex=2.5)   main=paste("Shot Rate Differential Against, With", input$player1, input$season)

    })
  


  ## Plot Panel 3: Rates against, with and without
  output$rateagainst <- renderPlot ({
      ## title |        | title
      ## plot1 | legend | plot2 

      myplan <- layout (matrix(c(2,0,5, 1,3,4), nrow=2, ncol=3, byrow=TRUE),
                        widths=c(5, 0.7, 5), heights=c(1, 6))
#      myplan <- layout (matrix(c(0,2, 3,1, 5,4), nrow=3, ncol=2, byrow=TRUE),
#                        widths=c(0.7, 5), heights=c(1, 6, 6))

      ## plot 1: rate with for
      par(mar=c(0,0,2,0))
      quant1 <- team.with.against()[[1]][,4 + 2*input$superbins]
      quant2 <- ref.counts()[[1]][,4 + 2*input$superbins]/total.time.D()*player.time.D()[1]
      cols <- if (input$zscoresonplot)
          z.score.binning(z.score.poisson(quant1, quant2)) else
      relative.binning(quant1/quant2)
      m.main <- if (input$mansituation != "Power Play") "Relative Shot Rate Against, With" else "Relative Shot Rate Against, With, SH"
      
      rink.hexplot (scatter.grid, sizes=team.with.against()[[1]][,1],
                    colors=cols, main=m.main)  #,input$numbersonplot
      if (input$numbersonplot) {
          ratio <- (team.with.against()[[2]][,1]/ref.counts()[[2]][,1])*(total.time.D()/player.time.D()[1])
          locs <- quad.centers; if (input$superbins) locs <- locs[c(1,2,11),]
          if (length(ratio) != nrow(locs)) ratio <- rep(0, nrow(locs))
          text (locs, as.character(signif(ratio, 3)), cex=2, col="black")  
      }
      
      ## plot 2: Title
      par(mar=c(0,0,0,0))
      plot(c(0,10), c(0,1), ty="n", axes=FALSE)
      text(5, 0.5, paste0("Relative Shot Rate Against\n",
                          input$player1), cex=2.5)
      
      ## plot 3: legend
      if (input$zscoresonplot) z.score.spectrum(xlab="Blue: Worse Team Offense -- Red: Better Team Offense") else relative.spectrum(xlab="Blue: Worse Team Offense -- Red: Better Team Offense")
      
      ## plot 4: rate without for 
      par(mar=c(0,0,2,0))
      quant1 <- team.without.against()[[1]][,4 + 2*input$superbins]
      quant2 <- ref.counts()[[1]][,4 + 2*input$superbins]/total.time.D()*player.time.D()[2]
      cols <- if (input$zscoresonplot)
          z.score.binning(z.score.poisson(quant1, quant2)) else
      relative.binning(quant1/quant2)
      m.main <- if (input$mansituation != "Power Play") "Relative Shot Rate Against, Without" else "Relative Shot Rate Against, Without, SH"
      
      rink.hexplot (scatter.grid, sizes=team.without.against()[[1]][,1],
                    colors=cols, main=m.main)  #,input$numbersonplot
      if (input$numbersonplot) {
          ratio <- (team.without.against()[[2]][,1]/ref.counts()[[2]][,1])*(total.time.D()/player.time.D()[2])
          locs <- quad.centers; if (input$superbins) locs <- locs[c(1,2,11),]
          if (length(ratio) != nrow(locs)) ratio <- rep(0, nrow(locs))
          text (locs, as.character(signif(ratio, 3)), cex=2, col="black")  
      }
      legend ("bottomright", "war-on-ice.com", bty="n", cex=2)
      
      ## plot 5: legend
      ##if (input$zscoresonplot) z.score.spectrum(xlab="Blue: Worse Team Offense -- Red: Better Team Offense") else relative.spectrum(xlab="Blue: Worse Team Offense -- Red: Better Team Offense")
      ## plot 2: Title
      par(mar=c(0,0,0,0))
      plot(c(0,10), c(0,1), ty="n", axes=FALSE)
      text(5, 0.5, paste0("Relative Shot Rate Against\nWithout ",
                          input$player1), cex=2.5)
      
    ## plot 5: Title
    #par(mar=c(0,0,0,0))
    #plot(c(0,10), c(0,1), ty="n", axes=FALSE)
    #text(5, 0.5, paste(input$player1, "--", input$season), cex=2.5)   main=paste("Shot Rate Differential Against, With", input$player1, input$season)

  })
  




  

  ## Plot panel 4.
    output$shootingsuccess <- renderPlot ({

    #myplan <- layout (matrix(c(0,2, 3,1, 5,4), nrow=3, ncol=2, byrow=TRUE),
    #                  widths=c(0.7, 5), heights=c(1, 6, 6))

        ## ()      | title | title | ()
        ## legend1 | plot1 | plot2 } legend2
      
        myplan <- layout (matrix(c(0,2, 6,0, 3,1, 4,5), nrow=2, ncol=4, byrow=TRUE),
                          widths=c(0.7, 5, 5, 0.7), heights=c(1, 5))
    
        quant1 <- player.counts()[[1]][,5 + 2*input$superbins]/player.counts()[[1]][,4 + 2*input$superbins]
        quant2 <- ref.counts()[[1]][,5 + 2*input$superbins]/ref.counts()[[1]][,4 + 2*input$superbins]
    
    ## 1: hexplot.
        par(mar=c(0,0,2,0))
        rink.hexplot (scatter.grid, sizes=player.counts()[[1]][,1],
                      colors=absolute.binning(quant1),
                      bordercolor="grey",
                      main="Absolute")

    ## 2: Title.
        par(mar=c(0,0,0,0))
        plot(c(0,10), c(0,1), ty="n", axes=FALSE)
        text(5, 0.5, paste0("Absolute Shooting Percentages\n", input$player1), cex=2.5)

    ## 3: spectrum guide
        absolute.spectrum(xlab="Darker: Higher Probability of Success")

    ## 4: hexplot.
        par(mar=c(0,0,2,0))
        cols <- if (input$zscoresonplot) z.score.binning(z.score.binomial(player.counts()[[1]][,3+1:2 + 2*input$superbins], ref.counts()[[1]][,3+1:2 + 2*input$superbins])) else relative.binning(quant1/quant2)
        
        rink.hexplot (scatter.grid, sizes=player.counts()[[1]][,1],
                      colors=cols, #relative.binning(quant1/quant2),
                      bordercolor="grey",
                      main="Relative To League")
        legend ("bottomright", "war-on-ice.com", bty="n", cex=2)

    ## 5: Spectrum.
        if (input$zscoresonplot) z.score.spectrum(xlab="Blue: Worse Team Offense -- Red: Better Team Offense") else relative.spectrum(xlab="Blue: Worse Team Offense -- Red: Better Team Offense")
    #relative.spectrum(xlab="Darker: Higher Probability of Success")

    
    ## 6: Title.
        par(mar=c(0,0,0,0))
        plot(c(0,10), c(0,1), ty="n", axes=FALSE)
        text(5, 0.5, paste0("Relative Shooting Percentages\n", input$player1), cex=2.5)
        
    })
    successlabel <- reactive (if (roster.unique$pos[pn()] == "G") "Shooting % Against Goaltender" else "Player Shooting % For Goal")
    output$successlabel <- renderUI(h4(successlabel()))


    netresult.table <- reactive ({
        diff.for <- (team.with.for()[[2]][,1]/player.time.O()[1] -
                     team.without.for()[[2]][,1]/player.time.O()[2])*3600
        
        diff.against <- (team.with.against()[[2]][,1]/player.time.D()[1] -
                         team.without.against()[[2]][,1]/player.time.D()[2])*3600
        
        standard.rate <- ref.counts()[[2]][,2]/ref.counts()[[2]][,1]
        outtable <- rbind(signif(diff.for,3), signif(diff.against,3),
                          signif(standard.rate,3),
                          signif((diff.for - diff.against)*standard.rate, 3))
        outtable <- cbind(outtable, {m1 <- apply(outtable, 1, sum); m1[3] <- mean(outtable[3,]); m1})
        rownames(outtable) <- c("Net Shots For", "Net Shots Against", "Average Conversion", "Net Goals")
        if (!input$superbins) {
            colnames(outtable) <- c("R-Point","C-Point","L-Point",
                                    "R-1","R-2","HighSlot","L-2","L-1",
                                    "R-Low","R-Slot","Slot","LowSlot","L-Slot","L-Low",
                                    "DownLow","NZone"
                                    ,"Total")
            ##    print(outtable)
            outtable <- outtable[,c(17, 1:16)]
        } else {
            colnames(outtable) <- c("LowProb","MediumProb","Slot","Total")
            ##    print(outtable)
            outtable <- outtable[,c(4, 1:3)]
        }
        
        return(outtable)
    })
    
    output$netresult <- renderTable ({
        netresult.table()
    })
  

  rate.minutes.O <- reactive ({
    timeonice <- cbind(c(player.time.O()[1], player.time.O()[2]))/60
    rownames(timeonice) <- c("On Ice", "Off Ice")
    colnames(timeonice) <- "Offense Minutes"
    return(timeonice)
  })
  output$rateminutesO <- renderTable ({
    r1 <- rate.minutes.O()
#    if (input$mansituation == "Power Play") rownames(r1) <- c("Power Play","Penalty Kill")
    r1
  })

  rate.minutes.D <- reactive ({
    timeonice <- cbind(c(player.time.D()[1], player.time.D()[2]))/60
    rownames(timeonice) <- c("On Ice", "Off Ice")
    colnames(timeonice) <- "Defense Minutes"
    return(timeonice)
  })
  output$rateminutesD <- renderTable ({
    r1 <- rate.minutes.D()
#    if (input$mansituation == "Power Play") rownames(r1) <- c("Power Play","Penalty Kill")
    r1
  })
  
})

  



    
    ##input <- list(mansituation = "Even Strength", include.playoffs=FALSE); pick=6:7
    go <- reactive ({
        gg1 <- if (input$mansituation == "Even Strength")
            c(lapply(allshotdata[pick()], function(ll) ll$es.quants.s),
              if (input$include.playoffs) lapply(allshotdata[pick()[pick() != 10]], function(ll) if (nrow(ll$es.quants.p$shots) > 0) ll$es.quants.p else NULL) else NULL) else
        if (input$mansituation == "Power Play")
            c(lapply(allshotdata[pick()], function(ll) ll$pp.quants.s),
              if (input$include.playoffs) lapply(allshotdata[pick()[pick() != 10]], function(ll) if (nrow(ll$es.quants.p$shots) > 0) ll$pp.quants.p else NULL) else NULL) else
        c(lapply(allshotdata[pick()], function(ll) ll$four.quants.s),
          if (input$include.playoffs) lapply(allshotdata[pick()[pick() != 10]], function(ll) if (nrow(ll$es.quants.p$shots) > 0) ll$four.quants.p else NULL) else NULL)

        gg2 <- list(
            shots=do.call(rbind, lapply(gg1, function(ll) ll$shots)),
            
            time.on.ice.home.O=do.call(rbind, lapply(gg1, function(ll) ll$time.on.ice.home.O)),
            time.on.ice.home.D=do.call(rbind, lapply(gg1, function(ll) ll$time.on.ice.home.D)),
            time.on.ice.away.O=do.call(rbind, lapply(gg1, function(ll) ll$time.on.ice.away.O)),
            time.on.ice.away.D=do.call(rbind, lapply(gg1, function(ll) ll$time.on.ice.away.D)),
            
            seasonal.counts=sumup(lapply(gg1, function(ll)
                if (length(ll$seasonal.counts) > 0) ll$seasonal.counts[[1]] else NULL)),
            seasonal.bin.counts=sumup(lapply(gg1, function(ll)
                if (length(ll$seasonal.bin.counts) > 0) ll$seasonal.bin.counts[[1]] else NULL)),
            seasonal.superbin.counts=sumup(lapply(gg1, function(ll)
                if (length(ll$seasonal.superbin.counts) > 0) ll$seasonal.superbin.counts[[1]] else NULL))
            )

        gg2$shots$ev.team <- swapout(gg2$shots$ev.team)
        gg2$shots$hometeam <- swapout(gg2$shots$hometeam)
        gg2$shots$awayteam <- swapout(gg2$shots$awayteam)
        
        rownames(gg2$time.on.ice.home.O) <- swapout(rownames(gg2$time.on.ice.home.O))
        rownames(gg2$time.on.ice.home.D) <- swapout(rownames(gg2$time.on.ice.home.D))
        rownames(gg2$time.on.ice.away.O) <- swapout(rownames(gg2$time.on.ice.away.O))
        rownames(gg2$time.on.ice.away.D) <- swapout(rownames(gg2$time.on.ice.away.D))
        
        ##save(gg1, gg2, file="test-output.RData")
        gg2
    })


    player.counts <- reactive ({
        player.events <- subset(goshots(),
                                ev.team == input$player1 &
                                type %in% this.shottype())
        if (input$homeawayall == "Away") player.events <- subset (player.events, ev.team == awayteam)
        if (input$homeawayall == "Home") player.events <- subset (player.events, ev.team == hometeam)
        output <- list(shot.bin.set (player.events, scatter.grid, coordset()),
                       shot.bin.set.blocks (player.events, scatter.grid, coordset(), input$superbins))
                                        #print(output[[2]])
        return(output)
    })
    
    player.counts.against <- reactive ({
        player.events <- subset(goshots(),
                                (hometeam == input$player1 | awayteam == input$player1) & ev.team != input$player1 &
                                type %in% this.shottype())
        if (input$homeawayall == "Away") player.events <- subset (player.events, ev.team == hometeam)
        if (input$homeawayall == "Home") player.events <- subset (player.events, ev.team == awayteam)
        output <- list(shot.bin.set (player.events, scatter.grid, coordset()),
                       shot.bin.set.blocks (player.events, scatter.grid, coordset(), input$superbins))
                                        #print(output[[2]])
        return(output)
        
    })


do.this.sometimes <- function () {
    png("leaguewiderate.png", width=690, height=600)
    myplan <- layout (matrix(c(2,1), nrow=1, ncol=2, byrow=TRUE),
                      widths=c(0.9, 6))
    par(mar=c(0,0,2,0))
    rink.hexplot.auto (scatter.grid, sizes=ref.counts()[[1]][,1],
                       colors=absolute.binning(ref.counts()[[1]][,5]/ref.counts()[[1]][,4]), main="League-Wide Success Rate")
    absolute.spectrum(xlab="Darker: Higher Probability of Success")
    dev.off()
}


    ref.counts <- reactive({
        reg1 <- go()$seasonal.counts
        reg2 <- if (!input$superbins) go()$seasonal.bin.counts else go()$seasonal.superbin.counts
        list(reg1, reg2)
    })

    ## coordset <- function () c("newyc", "newxc")
    ## coordset <- reactive(c("newyc", "newxc")) ##if (input$useimputedlocations)  else c("ycoord","xcoord"))
    

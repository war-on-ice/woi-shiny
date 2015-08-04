
## hextally-player

library(shiny)
library(MASS)
source("global.r")

######################################################################################

shinyServer(function(input, output, session){

    ## User input stage.
    GETterms <- reactive({g1 <- split.GET.to.frame(session$clientData$url_search); print(g1); g1})
    
    reduced.roster <- reactive({
        subset(roster.unique, pos == "G")
    })
    playercycle <- c("fleury","reimer","quick","miller","luongo")
    onload.nameSearch1 <-
        reactive(if (!is.null(GETterms()$name)) gsub("0"," ", GETterms()$name) else sample(playercycle,1))
    output$onload.nameSearch1 <- renderUI(textInput("nameSearch1", "Search For Player", onload.nameSearch1()))

    searchResult1 <- reactive({
        matches <- grep(tolower(input$nameSearch1),
                        tolower(reduced.roster()$firstlastpos))
        #print(reduced.roster()[matches,])
        sr1 <- reduced.roster()$firstlastpos[matches]
        sr1
    })
    output$nameSelect1 <- renderUI(selectInput ("player1", "", searchResult1()))
    
    pn <- reactive ({
        pn <- roster.unique$woi.id[grep(paste0("^",input$player1,"$"), roster.unique$firstlastpos)]
        pn
    })

    alt.text <- reactive(c("","","",""))

    outdest <- reactive(paste0 ("http://war-on-ice.com/hexgoalies.html?",
                                "name=",gsub(" ","0",input$player1),
                                "&start0=",input$startseason,
                                "&end0=",input$endseason
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

    seasonpick <- reactive({
        sort(seasons[match(c(input$startseason, input$endseason), seasons)])
    })
    
    #pick <- reactive({
    #    sort(match(input$startseason, seasons):match(input$endseason, seasons))
    #})

  ## Operations.
  #game object

    all.data <- reactive({
        ##db.hex <- dbConnect(SQLite(), dbname="common-data/hextally.sqlite")
        ##input <- list(player1="TOR", startseason="20122013", endseason="20142015", homeawayall="All", includeblockmiss=TRUE, includeplayoffs=FALSE, mansituation="Power Play", superbins=TRUE); seasonpick <- function() c("20122013","20132014","20142015")
        ## pn <- function () "quickjo86"

        reduced.connect <- src_sqlite("../common-data/hextally.sqlite")
        sqlstatement <- paste0('SELECT season, gcode, etype, newxc, newyc, type, [new.loc.section], gamestate, hexblock, home FROM shots WHERE role = "G" AND ID = "',pn(),'" ')
        primer <- tbl(reduced.connect, sql(sqlstatement)) %>% filter(season >= seasonpick()[1], season <= seasonpick()[2]) %>% as.data.frame
        
        sqlstatement3 <- paste0('SELECT * FROM baselinedata')
        baseline <- tbl(reduced.connect, sql(sqlstatement3)) %>% filter(season >= seasonpick()[1], season <= seasonpick()[2]) %>% as.data.frame %>% mutate(danger = danger.zone[new.loc.section + 1])

        ## all.data <- function () list(shots=primer, TOI=TOIteam, baseline=baseline)
        return(list(shots=primer, baseline=baseline))
    })


    reduced.data <- reactive({

        ## compress by area.
        these.shots <- all.data()$shots
        ## if (!input$includeblockmiss)
        these.shots <- filter (these.shots, etype %in% c("SHOT","GOAL"))
        if (!input$includeplayoffs) these.shots <- filter (these.shots, substr(gcode,1,1) == 2)
        homeaway <- list(1, 0, 0:1)[[match(input$homeawayall, c("Home", "Away", "All"))]]
        these.shots <- filter (these.shots, home %in% homeaway) ##, type %in% this.shottype())
        
        ##o.state <- c(1,2,4)[match(input$mansituation, c("Even Strength", "Power Play", "Four On Four"))]
        d.state <- c(1,2,4)[match(input$mansituation, c("Even Strength", "Power Play", "Four On Four"))]

        ##print(c(o.state, d.state))
        
        shots.against.table <- these.shots %>% filter(gamestate==d.state) %>%
            group_by(hexblock) %>% summarize(count=n(), goals=sum(etype=="GOAL")) %>% filter (!is.na(hexblock))
        shots.against.count <- cbind(rep(0, length(scatter.grid$zone.section)),
                                     rep(0, length(scatter.grid$zone.section)))
        shots.against.count[shots.against.table$hexblock,1] <- shots.against.table$count
        shots.against.count[shots.against.table$hexblock,2] <- shots.against.table$goals

        shots.bins <- data.frame(sac=shots.against.count[,1],
                                 gac=shots.against.count[,2],
                                 zs=scatter.grid$zone.section,
                                 zs2=scatter.grid$zone.section.bigblock)
        
        shots.16 <- shots.bins %>% group_by(zs) %>%
            summarize(shotsagn=sum(sac), goalsagn=sum(gac))
        shots.3 <- shots.bins %>% group_by(zs2) %>%
            summarize(shotsagn=sum(sac), goalsagn=sum(gac))
                
        ##TOI <- all.data()$TOI
        ##time.O <- sum(TOI$TOI[TOI$gamestate == o.state & TOI$home %in% homeaway])
        ##time.D <- sum(TOI$TOI[TOI$gamestate == d.state & TOI$home %in% homeaway])

        baseline <- all.data()$baseline
        ##ref.time.O <- sum(baseline$TOI[baseline$new.loc.section == 0 & baseline$gamestate == o.state])
        ##ref.time.D <- sum(baseline$TOI[baseline$new.loc.section == 0 & baseline$gamestate == d.state])

        #if (!input$includeblockmiss) {
            baseline$BLOCK <- 0; baseline$MISS <- 0
        #}
        
        ref.shots.D <- baseline %>% filter (gamestate == d.state) %>% group_by (new.loc.section) %>%
            summarize (shots = sum(BLOCK+SHOT+MISS+GOAL), goals=sum(GOAL)) %>%
                filter(new.loc.section > 0) %>% rename (bin = new.loc.section)
        ref.shots.D3 <- baseline %>% filter (gamestate == d.state) %>% group_by (danger) %>%
            summarize (shots = sum(BLOCK+SHOT+MISS+GOAL), goals=sum(GOAL)) %>%
                filter(danger > 0) %>% rename (bin = danger)

        if (input$superbins)
            output <- list(shots.against.count=shots.against.count,
                           shots.group=shots.3,
                           ref.shots.D=ref.shots.D3) else
            output <- list(shots.against.count=shots.against.count,
                           shots.group=shots.16,
                           ref.shots.D=ref.shots.D)

        ## reduced.data <- function () output
        return(output)
        
    })






    output$shootingcount <- renderPlot({
        base.colors <- absolute.binning(reduced.data()$ref.shots.D[["goals"]]/
                                        reduced.data()$ref.shots.D[["shots"]])
        colors <- if (input$superbins) base.colors[scatter.grid$zone.section.bigblock] else base.colors[scatter.grid$zone.section]
        par(mar=c(0,0,2,0))
        rink.hexplot.auto (scatter.grid, sizes=rawcounts, ##reduced.data()$shots.for.count,
                           colors=colors, main="League-Wide Success Rate")
    })
    


    output$guidepostgrand <- renderPlot ({

        par(mar=c(0,0,3,0))
        rink.hexplot (scatter.grid, sizes=0*reduced.data()$shots.against.count, 
                      main=paste("Success By Region: Total"),
                      colors=0*reduced.data()$shots.against.count)
        apply(quadsarrayplot[1:4,,], 3, polygon, border=3, col=NA, lwd=2)
        to.print <- paste0(2*reduced.data()$ref.shots.D[["goals"]],"/",2*reduced.data()$ref.shots.D[["shots"]])    #       ref.counts()[[2]][,2]
        locs <- quad.centers; if (input$superbins) locs <- locs[c(1,6,11),]
        text (locs, to.print, cex=0.8)
        
    })
    
  
    output$guidepostnames <- renderPlot ({
        par(mar=c(0,0,0,0))
        rink.hexplot (scatter.grid, sizes=0*reduced.data()$shots.against.count, main=paste(""),
                      colors=0*reduced.data()$shots.against.count) 
        apply(quadsarrayplot[1:4,,], 3, polygon, border=3, col=NA, lwd=2)
        to.print <- c("R-Point","C-Point","L-Point",
                      "R-1","R-2","HighSlot","L-2","L-1",
                      "R-Low","R-Slot","Slot","LowSlot","L-Slot","L-Low",
                      "DownLow","NZone")
        text (quad.centers, to.print, cex=0.8)
    })
 
  


  

  ## Plot panel 4.
    output$shootingsuccess <- renderPlot ({

    #myplan <- layout (matrix(c(0,2, 3,1, 5,4), nrow=3, ncol=2, byrow=TRUE),
    #                  widths=c(0.7, 5), heights=c(1, 6, 6))

        ## ()      | title | title | ()
        ## legend1 | plot1 | plot2 } legend2
      
        myplan <- layout (matrix(c(0,2, 6,0, 3,1, 4,5), nrow=2, ncol=4, byrow=TRUE),
                          widths=c(0.7, 5, 5, 0.7), heights=c(1, 5))


        quant1.prime <- reduced.data()$shots.group[["goalsagn"]]/reduced.data()$shots.group[["shotsagn"]]
        quant2.prime <- reduced.data()$ref.shots.D[["goals"]]/reduced.data()$ref.shots.D[["shots"]]   
        if (length(quant1.prime) == 3) {
            quant1 <- quant1.prime[scatter.grid$zone.section.bigblock]
            quant2 <- quant2.prime[scatter.grid$zone.section.bigblock]
        } else {
            quant1 <- quant1.prime[scatter.grid$zone.section]
            quant2 <- quant2.prime[scatter.grid$zone.section]
        }
        
 #       quant1 <- player.counts()[[1]][,5 + 2*input$superbins]/player.counts()[[1]][,4 + 2*input$superbins]
 #       quant2 <- ref.counts()[[1]][,5 + 2*input$superbins]/ref.counts()[[1]][,4 + 2*input$superbins]

        
    ## 1: hexplot.
        par(mar=c(0,0,2,0))
        rink.hexplot (scatter.grid, sizes=reduced.data()$shots.against.count[,1],   ##player.counts()[[1]][,1],
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
        cols <- if (input$zscoresonplot) {
            run1 <- z.score.binning(z.score.binomial(cbind(reduced.data()$shots.group[["shotsagn"]],
                                                           reduced.data()$shots.group[["goalsagn"]]),
                                                     cbind(reduced.data()$ref.shots.D[["shots"]],
                                                           reduced.data()$ref.shots.D[["goals"]])))
            if (length(quant1.prime) == 3) run1[scatter.grid$zone.section.bigblock] else run1[scatter.grid$zone.section]
        } else relative.binning(quant1/quant2)
            
##            z.score.binning(z.score.binomial(player.counts()[[1]][,3+1:2 + 2*input$superbins], ref.counts()[[1]][,3+1:2 + 2*input$superbins])) else relative.binning(quant1/quant2)
        
        rink.hexplot (scatter.grid, sizes=reduced.data()$shots.against.count[,1],   ##  player.counts()[[1]][,1],
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
    successlabel <- reactive ("Shooting % Against Goaltender -- Redder Is Worse")
    output$successlabel <- renderUI(h4(successlabel()))


    
    
})

  

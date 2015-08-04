## gamesummary global
## Last edited: August 31, 2014

library(shiny)
library(warbase)

library(jsonlite)

## load (url("http://biscuit.war-on-ice.com/common-data/games/2014201520001.RData"))
con1 <- url("http://biscuit.war-on-ice.com/common-data/woi-common.RData")
load (con1)
close (con1)

##load ("../common-data/woi-common.RData")
dangers <- read.csv("dangerzones.csv")
team.options <- c("All", sort(teams))

game.screening.choices <- c("Prime", "Rates", "Counts", "All")

empty.net.color <- "#EEFFEE"


danger.lines <- function () {
    segments(dangers[-nrow(dangers),1],
             dangers[-nrow(dangers),2],
             dangers[-1,1],
             dangers[-1,2],
             col="#BBBBBB",
             lwd=2, lty=3)
}


crp <- function (points, values) {
  values[is.na(values)] <- 0.5;
  values[values<0 | values > 1] <- 0.5;
  rgb(colorRamp(points)(values), max=255)
}

color.spec <- function (magnitude, share, maxmag=magnitude) {
  #magnitude=shift.bits$el; share=shift.bits$evpct; maxmag=maxy
  colset1 <- rbind(crp(c("#FFEEEE","#FF2222"), magnitude/maxmag), ## red
                   crp(c("#EEFFEE","#22FF22"), magnitude/maxmag), ## green, center
                   crp(c("#EEEEFF","#2222FF"), magnitude/maxmag)) ## blue
  sapply(1:length(share), function (pp) crp(colset1[c(1,1,2,3,3),pp], share[pp]))
  #col1 <- sapply(crp (
}


retrieve.one.game <- function (seasongcode) {
  #seasongcode="2013201420001"
    
    urlname <- paste0("http://biscuit.war-on-ice.com/common-data/games/",seasongcode,".RData")
    ## fname <- paste0("../common-data/games/",seasongcode,".RData")
    ##message (paste(getwd()))
    ##message (fname, " ", file.exists(fname))

    con1 <- url(urlname)
    res <- try ({load(con1); close(con1)}, TRUE)
        
    if (class(res)[1] != "try-error") {
    
        if (substr(seasongcode,9,9)=="2") {
            
            playbyplay$event.length[playbyplay$seconds >= 3900] <- 0
            playbyplay$event.length[playbyplay$seconds == 3900 & playbyplay$etype == "CHANGE"] <- 0
            playbyplay$seconds[playbyplay$seconds >= 3900] <- 3900
            
            score <- c(away=sum(playbyplay$etype=="GOAL" &
                           playbyplay$ev.team == playbyplay$awayteam &
                           playbyplay$period <= 4),
                       home=sum(playbyplay$etype=="GOAL" &
                           playbyplay$ev.team == playbyplay$hometeam &
                           playbyplay$period <= 4))
            score5 <- c(away=sum(playbyplay$etype=="GOAL" &
                            playbyplay$ev.team == playbyplay$awayteam &
                            playbyplay$period == 5),
                        home=sum(playbyplay$etype=="GOAL" &
                            playbyplay$ev.team == playbyplay$hometeam &
                            playbyplay$period == 5))
            if (sum(score5) > 0 & score5[1] > score5[2]) score[1] <- score[1]+1
            if (sum(score5) > 0 & score5[1] < score5[2]) score[2] <- score[2]+1
        } else {
            score <- c(away=sum(playbyplay$etype=="GOAL" & playbyplay$ev.team == playbyplay$awayteam),
                       home=sum(playbyplay$etype=="GOAL" & playbyplay$ev.team == playbyplay$hometeam))
        }
    #print (tail(playbyplay, 15))
        return(list(playbyplay=playbyplay,
                    playerrun=playerrun, #score.diff.cat %in% 0:6 & gamestate %in% 0:6),
                    teamrun=teamrun, #score.diff.cat %in% 0:6 & gamestate %in% 0:6),
                    goalierun=goalierun, #score.diff.cat %in% 0:6 & gamestate %in% 0:6),
                    coplayer=mutate(coplayer, seasongcode=seasongcode),
                    gameroster=mutate(gameroster, seasongcode=seasongcode),
                    score=score))
        
    } else {
        return(list())
    }
}

flip.playbyplay <- function (playbyplay, team1) {

    flips <- which(playbyplay$hometeam == team1)
    playbyplay[flips, c("home.G", "away.G",
                        "hometeam", "awayteam",
                        "a1","a2","a3","a4","a5","a6",
                        "h1","h2","h3","h4","h5","h6",
                        "home.skaters","away.skaters")] <-
                            playbyplay[flips, c("away.G", "home.G",
                                                "awayteam", "hometeam", 
                                                "h1","h2","h3","h4","h5","h6",
                                                "a1","a2","a3","a4","a5","a6","away.skaters",
                                                "home.skaters")]
    playbyplay

}


retrieve.gameset <- function (seasonpick,
                              team1,
                              team2,
                              mode = c("Regular","Playoffs")) {
    ## seasonpick <- "20142015"; team1="TOR"; team2="MTL"; mode= c("Regular","Playoffs")
    if (mode == "Regular/Playoffs") mode <- c("Regular","Playoffs")
    message (seasonpick, " ", team1, " ", team2, " ", mode)
    
    gamepicks <- filter (gamestest,
                         season %in% seasonpick,
                         (hometeam == team1 & awayteam == team2) | (hometeam == team2 & awayteam == team1),
                         session %in% mode)

    return(gamepicks)
}

retrieve.series <- function (seasonpick,
                             team1,
                             team2,
                             mode = c("Regular","Playoffs")) {
    ## seasonpick <- "20142015"; team1="TOR"; team2="MTL"; mode= c("Regular","Playoffs")
    if (mode == "Regular/Playoffs") mode <- c("Regular","Playoffs")
    message (seasonpick, " ", team1, " ", team2, " ", mode)
    
    gamepicks <- filter (gamestest,
                         season %in% seasonpick,
                         (hometeam == team1 & awayteam == team2) | (hometeam == team2 & awayteam == team1),
                         session %in% mode)
    gamelist <- paste0(gamepicks$season, gamepicks$gcode) 
    print(gamelist)
    
    gameinfo <- lapply (gamelist, retrieve.one.game)
    
    playbyplay <- rbind_all (lapply(gameinfo, function(gg) gg$playbyplay)) %>% as.data.frame %>%
        flip.playbyplay(team1)
    playerrun <- rbind_all (lapply(gameinfo, function(gg) gg$playerrun)) %>% as.data.frame
    teamrun <- rbind_all (lapply(gameinfo, function(gg) gg$teamrun)) %>% as.data.frame
    goalierun <- rbind_all (lapply(gameinfo, function(gg) gg$goalierun)) %>% as.data.frame
    coplayer <- rbind_all (lapply(gameinfo, function(gg) gg$coplayer)) %>% as.data.frame
    gameroster <- rbind_all (lapply(gameinfo, function(gg) gg$gameroster)) %>% as.data.frame

    ##print(head(playbyplay,3))
    ##save (playbyplay, file="testee.RData")
    ## print(head(playbyplay,2))
    ## print(head(playerrun,2))
    ## print(head(teamrun,2))
    ## print(head(goalierun,2))
    ## print(head(coplayer,2))
    ## print(head(gameroster,2))
    
    return(list(playbyplay=playbyplay,
                playerrun=playerrun, #score.diff.cat %in% 0:6 & gamestate %in% 0:6),
                teamrun=teamrun, #score.diff.cat %in% 0:6 & gamestate %in% 0:6),
                goalierun=goalierun, #score.diff.cat %in% 0:6 & gamestate %in% 0:6),
                coplayer=coplayer,
                gameroster=gameroster))

}




op <- function(x, pf) 1-(1-x)/pf
fade.color.set <- function(away.color="#0000FF", home.color="#FF0000", pastel.factor=5) {
  c(home.color = rgb(op(colorRamp(c(home.color, home.color))(0.5)/255, pastel.factor)),
    middle.color = rgb(op(colorRamp(c(home.color, away.color))(0.5)/255, pastel.factor)),
    away.color = rgb(op(colorRamp(c(away.color, away.color))(0.5)/255, pastel.factor)))
}



colset <- function (awaycolors, homecolors) {
  
  coldists <- c(sum(abs(col2rgb(awaycolors[1])-col2rgb(homecolors[1]))),
                sum(abs(col2rgb(awaycolors[2])-col2rgb(homecolors[1]))),
                sum(abs(col2rgb(awaycolors[1])-col2rgb(homecolors[2]))),
                sum(abs(col2rgb(awaycolors[2])-col2rgb(homecolors[2]))))
  ahcol <- if (coldists[1] > 200) c(awaycolors[1], homecolors[1]) else if (coldists[2] > 200) c(awaycolors[2], homecolors[1]) else if (coldists[3] > 200) c(awaycolors[1], homecolors[2]) else c(awaycolors[2], homecolors[2])
  ahcol 
  
}


## Sorting scheme: first by position, then by time on ice
## make.shift.pieces () added to "warbase" and precalculated for game files.






image.netplot.precolor <- function (edge.list,
                                    outcome.colors,
                                    null.color="#FFFFFF",
                                    n.nodes=max(c(edge.list)),
                                    lwd=0.1,
                                    
                                    node.labels=1:n.nodes,
                                    label.cex=30/n.nodes,
                                    node.order=1:n.nodes,
                                    ...) {
  
  par(mar=c(2,9,9,2))
  plot(c(0, n.nodes), c(0, n.nodes), ty="n",
       axes=FALSE, xlab="", ylab="",
       xaxs = "i", yaxs = "i",
       ylim=c(n.nodes, 0), ...)
  rect(0, 0, n.nodes, n.nodes, col=null.color)
  
  reordered.edge.list <- edge.list
  
  rect(reordered.edge.list[,2]-1, reordered.edge.list[,1]-1,
       reordered.edge.list[,2], reordered.edge.list[,1], col=outcome.colors, lwd=lwd)
  
  axis(2, 1:n.nodes - 0.5, node.labels[node.order], las=2)
  axis(3, 1:n.nodes - 0.5, node.labels[node.order], las=2)
  
}

image.netplot.justtimes <- function (edge.list,
                                     textbits,
                                     null.color="#FFFFFF",
                                     n.nodes=max(c(edge.list)),
                                     lwd=0.1,
                                    
                                     node.labels=1:n.nodes,
                                     label.cex=30/n.nodes,
                                     node.order=1:n.nodes,
                                     ...) {
  
    par(mar=c(2,9,9,2))
    plot(c(0, n.nodes), c(0, n.nodes), ty="n",
         axes=FALSE, xlab="", ylab="",
         xaxs = "i", yaxs = "i",
         ylim=c(n.nodes, 0), ...)
    rect(0, 0, n.nodes, n.nodes, col=null.color)
    
    reordered.edge.list <- edge.list
    text (reordered.edge.list[,2]-0.5, reordered.edge.list[,1]-0.5,
          textbits, cex=0.6)
          
#         reordered.edge.list[,2], reordered.edge.list[,1], border=8col=outcome.colors, lwd=lwd)
    
    axis(2, 1:n.nodes - 0.5, node.labels[node.order], las=2)
    axis(3, 1:n.nodes - 0.5, node.labels[node.order], las=2)
  
}



time.together.plot <- function (playbyplay,
                                shift.bits=make.coplay.edge.lists (playbyplay),
                                rosters=roster.unique,                                
                                usecolors=FALSE,
                                just.numbers=FALSE, just.ES=FALSE) {

    toi.bit <- shift.bits[shift.bits$p1 == shift.bits$p2,]
    
    away.toi <- toi.bit$el[toi.bit$gr == 2]
    names(away.toi) <- toi.bit$p1[toi.bit$gr == 2]

    home.toi <- toi.bit$el[toi.bit$gr == 1]
    names(home.toi) <- toi.bit$p1[toi.bit$gr == 1]

    ## take from shift.bits
    
    
    mini.roster <- rbind(data.frame(ID=names(away.toi),
                                    TOI=away.toi,
                                    home=0),
                         data.frame(ID=names(home.toi),
                                    TOI=home.toi,
                                    home=1))
  
    mini.roster$pos <- rosters$pos[match(mini.roster$ID, rosters$woi.id)]
    mini.roster$name <- rosters$firstlast[match(mini.roster$ID, rosters$woi.id)]
    mini.roster <- mini.roster[order(mini.roster$home, 1*grepl("D", mini.roster$pos), -mini.roster$TOI),]

#  print(mini.roster)
  
    shift.bits$p1 <- match(shift.bits$p1, mini.roster$ID)
    shift.bits$p2 <- match(shift.bits$p2, mini.roster$ID)
    shift.bits$evpct <- shift.bits$evf/(shift.bits$evf + shift.bits$eva)
    maxy <- max(shift.bits$el)

    actual.time <- if (just.ES) shift.bits$el2 else shift.bits$el
    
    ##shift.bits$colors <- if (usecolors) color.spec(shift.bits$el, shift.bits$evpct, maxy) else crp(c("#FFFFFF","#000000"), shift.bits$el/max(shift.bits$el))
    shift.bits$colors <- if (usecolors) color.spec(actual.time, shift.bits$evpct, maxy) else crp(c("#FFFFFF","#000000"), actual.time/max(actual.time))
  
  
  #print(mini.roster$name)
  #print(head(shift.bits))
  #write.csv(shift.bits, "shiftbits.csv")
  
    if (just.numbers) {
        sc <- paste(round(actual.time %% 60)); sc[nchar(sc) < 2] <- paste0("0",sc[nchar(sc) < 2])
        this.time <- paste (floor(actual.time/60), sc, sep=":")
        header <- if (just.ES) "Head To Head Time-On-Ice (ES 5v5 Only)" else "Head To Head Time-On-Ice (All Situations)"
        image.netplot.justtimes (as.matrix(shift.bits[,1:2]),
                                 this.time,
                                 node.labels=mini.roster$name)
    } else {
        image.netplot.precolor (as.matrix(shift.bits[,1:2]),
                                shift.bits$colors,
                                node.labels=mini.roster$name)
    }
    abline (v=sum(mini.roster$home==0), col=3, lwd=2)
    abline (h=sum(mini.roster$home==0), col=3, lwd=2)
    
    axis(1, c(9,28), c(playbyplay$awayteam[1], playbyplay$hometeam[1]))
    axis(4, c(9,28), c(playbyplay$awayteam[1], playbyplay$hometeam[1]))


    if (!just.numbers) legend ("bottomleft", "war-on-ice.com", bty="n", cex=1)
  
  
}

## AT tests.
## 
## load (url("http://biscuit.war-on-ice.com/common-data/games/2014201520001.RData"))
## load (url("http://biscuit.war-on-ice.com/common-data/woi-common.RData"))
time.together.d3 <- function (playbyplay,
                              shift.bits=make.coplay.edge.lists (playbyplay, woiid=TRUE),
                              rosters=roster.unique,                                
                              usecolors=FALSE,
                              just.numbers=FALSE, just.ES=FALSE) {

    ##print(head(playbyplay))
    ##write.csv (shift.bits, "shift.csv")
    print(shift.bits)
    

  toi.bit <- shift.bits[shift.bits$p1 == shift.bits$p2,]
  
  away.toi <- toi.bit$el[toi.bit$gr == 2]
  names(away.toi) <- toi.bit$p1[toi.bit$gr == 2]
  
  home.toi <- toi.bit$el[toi.bit$gr == 1]
  names(home.toi) <- toi.bit$p1[toi.bit$gr == 1]
    
  mini.roster <- rbind(data.frame(ID=names(away.toi),
                                  TOI=away.toi,
                                  home=0),
                       data.frame(ID=names(home.toi),
                                  TOI=home.toi,
                                  home=1))
  
  mini.roster$pos <- rosters$pos[match(mini.roster$ID, rosters$woi.id)]
  mini.roster$name <- rosters$firstlast[match(mini.roster$ID, rosters$woi.id)]
  mini.roster <- mini.roster[order(mini.roster$home, 1*grepl("D", mini.roster$pos), -mini.roster$TOI),]
  

  shift.bits$p1 <- mini.roster$ID[match(shift.bits$p1, mini.roster$ID)]
  shift.bits$p2 <-mini.roster$ID[match(shift.bits$p2, mini.roster$ID)]
  shift.bits$pp <- paste(shift.bits$p1,shift.bits$p2)
  
  if (just.ES){
    shift.bits$time <- shift.bits$el2
    shift.bits$forEv <- shift.bits$evf2
    shift.bits$againstEv <- shift.bits$eva2
    shift.bits$evpct <- (shift.bits$evf2)/(shift.bits$evf2 + shift.bits$eva2)
  } else{
    shift.bits$time <- shift.bits$el
    shift.bits$forEv <- shift.bits$evf
    shift.bits$againstEv <- shift.bits$eva
    shift.bits$evpct <- (shift.bits$evf)/(shift.bits$evf + shift.bits$eva)    
  }

  mini.roster$TOI <- shift.bits$time[match(paste(mini.roster$ID, mini.roster$ID), shift.bits$pp)]
  mini.roster$forEv <- shift.bits$forEv[match(paste(mini.roster$ID, mini.roster$ID), shift.bits$pp)]
  mini.roster$againstEv <- shift.bits$againstEv[match(paste(mini.roster$ID, mini.roster$ID), shift.bits$pp)]
  mini.roster$evpct <- shift.bits$evpct[match(paste(mini.roster$ID, mini.roster$ID), shift.bits$pp)]

  away.roster <- filter(mini.roster, mini.roster$home == 0)
  home.roster <- filter(mini.roster, mini.roster$home == 1)

  # away v. away
  aa.bits <- select(subset(shift.bits, shift.bits$p1 %in% away.roster$ID & shift.bits$p2 %in% away.roster$ID), p1, p2, time, forEv, againstEv, evpct)
 
  # home v. home
  hh.bits <-  select(subset(shift.bits, shift.bits$p1 %in% home.roster$ID & shift.bits$p2 %in% home.roster$ID), p1, p2, time, forEv, againstEv, evpct)
  
  # away v. home
  ah.bits <-  select(subset(shift.bits, shift.bits$p1 %in% away.roster$ID & shift.bits$p2 %in% home.roster$ID), p1, p2, time, forEv, againstEv, evpct) 
  
  # home v. away
  ha.bits <-  select(subset(shift.bits, shift.bits$p1 %in% home.roster$ID & shift.bits$p2 %in% away.roster$ID), p1, p2, time, forEv, againstEv, evpct) 
  
  #get away/team info for name/coloring
  awayInfo <- filter(team.colors, team.colors$team == playbyplay$awayteam[1])
  homeInfo <- filter(team.colors, team.colors$team == playbyplay$hometeam[1])
 
 outscript <- paste ('
<div class="tooltip"></div>
<div class="row-fluid">
<div class="span12" id="aa-toi-chart" /> 
</div>
<div class="row-fluid">
<div class="span12" id="hh-toi-chart" />
</div>
<div class="row-fluid">
<div class="span12" id="ah-toi-chart" />
</div>
<div class="row-fluid">
<div class="span12" id="ha-toi-chart" />
</div>

<script>
','var awayRoster = ', toJSON(away.roster),';

var homeRoster = ', toJSON(home.roster),';

var awayTeam = ', toJSON(awayInfo),';
var homeTeam = ', toJSON(homeInfo),';

var aaData = ', toJSON(aa.bits),';
var hhData = ', toJSON(hh.bits),';
var haData = ', toJSON(ha.bits),';
var ahData = ', toJSON(ah.bits),';


var aaToiSvg = d3.select("#aa-toi-chart")
                    .append("svg");

var aaToiChart = new ROSTER_GRID({
  matchupData: aaData,
  xRoster: awayRoster,
  yRoster: awayRoster,
  xTeam: awayTeam[0],
  yTeam: awayTeam[0],
  matchupToolTipValue: ROSTER_HELPERS.matchupToolTipText,
  playerToolTipValue: ROSTER_HELPERS.playerToolTipText,
  textValue: ROSTER_HELPERS.timeOnIce,
  parent: aaToiSvg
});

aaToiChart();
                        
var hhToiSVg = d3.select("#hh-toi-chart")
                    .append("svg");

var hhToiChart = new ROSTER_GRID({
  matchupData: hhData,
  xRoster: homeRoster,
  yRoster: homeRoster,
  xTeam: homeTeam[0],
  yTeam: homeTeam[0],
  matchupToolTipValue: ROSTER_HELPERS.matchupToolTipText,
  playerToolTipValue: ROSTER_HELPERS.playerToolTipText,
  textValue: ROSTER_HELPERS.timeOnIce,
  parent: hhToiSVg
});

hhToiChart();

var haToiSvg = d3.select("#ha-toi-chart")
                    .append("svg");

var haToiChart = new ROSTER_GRID({
  matchupData: haData,
  xRoster: homeRoster,
  yRoster: awayRoster,
  xTeam: homeTeam[0],
  yTeam: awayTeam[0],
  matchupToolTipValue: ROSTER_HELPERS.matchupToolTipText,
  playerToolTipValue: ROSTER_HELPERS.playerToolTipText,
  textValue: ROSTER_HELPERS.timeOnIce,
  parent: haToiSvg
});

haToiChart();

var ahToiSvg = d3.select("#ah-toi-chart")
                    .append("svg");

var ahToiChart = new ROSTER_GRID({
  matchupData: ahData,
  xRoster: awayRoster,
  yRoster: homeRoster,
  xTeam: awayTeam[0],
  yTeam: homeTeam[0],
  matchupToolTipValue: ROSTER_HELPERS.matchupToolTipText,
  playerToolTipValue: ROSTER_HELPERS.playerToolTipText,
  textValue: ROSTER_HELPERS.timeOnIce,
  parent: ahToiSvg
});

ahToiChart();

</script>')
 
 outscript
}




## #########################################################################################################
##
## First cut of Shot Attempts F/A

shot.count.d3 <- function (sub.playerrun, team1, team2) {
    ##sub.playerrun = pt1

    print(head(sub.playerrun, 2))
    
    awayevents <- sub.playerrun %>%
        mutate (pos = roster.unique$pos[match(ID, roster.unique$woi.id)]) %>% 
            filter(pos != "G", Team == team1) %>% ## home == 0) %>%
                select(ID, Team, CF, CA, TOI) %>%
                rename (PlayerID = ID, ShotsFor = CF, ShotsAgainst = CA) %>%
                    mutate (PlayerName = roster.unique$firstlast[match(PlayerID, roster.unique$woi.id)],
                            ShotsFor = round(ShotsFor, 1),
                            ShotsAgainst = round(ShotsAgainst, 1),
                            TOI = round (TOI/60,1))

    homeevents <- sub.playerrun %>%
        mutate (pos = roster.unique$pos[match(ID, roster.unique$woi.id)]) %>% 
            filter(pos != "G", Team == team2) %>%    ##home == 1) %>%
                select(ID, Team, CF, CA, TOI) %>%
                rename (PlayerID = ID, ShotsFor = CF, ShotsAgainst = CA) %>%
                    mutate (PlayerName = roster.unique$firstlast[match(PlayerID, roster.unique$woi.id)],
                            ShotsFor = round(ShotsFor, 1),
                            ShotsAgainst = round(ShotsAgainst, 1),
                            TOI = round (TOI/60,1))


    
    awayteam <- team.colors$TeamName[match(awayevents$Team[1], team.colors$team)]
    hometeam <- team.colors$TeamName[match(homeevents$Team[1], team.colors$team)]

    outscript <- paste ('
<div class="tooltip"></div>
<div class="row-fluid">
<div class="span6" id="shacAway" /> <div class="span6" id="shacHome" />
</div>
<script>
','var awayData = ', toJSON(awayevents),';

var homeData = ', toJSON(homeevents),';

var ceChart1SVG = d3.select("#shacAway")
                    .append("svg");

                        var corsiEventChart1 = new SCATTER_NAME_CHART(
                    {   inputData: awayData,
                        xValue: CORSI_EVENT_HELPERS.shotsForPlusAgainst,
                        yValue:  CORSI_EVENT_HELPERS.shotsForMinusAgainst,
                        dataLabelValue: CORSI_EVENT_HELPERS.playerName,
                        dataLink: CORSI_EVENT_HELPERS.playerURL,
                        chartTitle: "', awayteam, 'On-Ice Shot Attempts For/Against",
                        toolTipValue: CORSI_EVENT_HELPERS.expandedDesc,
                        xLabel: "For Plus Against",
                        yLabel: "For Minus Against",
                        linkURL: CORSI_EVENT_HELPERS.playerURL,
                        parent: ceChart1SVG
});
                        corsiEventChart1();
                        
                        var ceChart2SVG = d3.select("#shacHome")
                        .append("svg");
                        
                        var corsiEventChart2 = new SCATTER_NAME_CHART(
                    {   inputData: homeData,
                        xValue: CORSI_EVENT_HELPERS.shotsForPlusAgainst,
                        yValue:  CORSI_EVENT_HELPERS.shotsForMinusAgainst,
                        dataLabelValue: CORSI_EVENT_HELPERS.playerName,
                        dataLink: CORSI_EVENT_HELPERS.playerURL,
                        chartTitle: "', hometeam, 'On-Ice Shot Attempts For/Against",
                        toolTipValue: CORSI_EVENT_HELPERS.expandedDesc,
                        xLabel: "For Plus Against",
                        yLabel: "For Minus Against",
                        linkURL: CORSI_EVENT_HELPERS.playerURL,
                        parent: ceChart2SVG
});
                        
                        corsiEventChart2();

</script>')

    outscript
    
}

## #########################################################################################################
##
## Shot Plot using d3
shot.plot.d3 <- function (allData){
  
  stypes <- c("GOAL","SHOT","BLOCK","MISS")
  shots <- filter(allData,
                  allData$etype %in% stypes)
  
  shots$P1 <- roster.unique$firstlast[match(shots$ev.player.1, roster.unique$woi.id)]
  shots$P2 <- roster.unique$firstlast[match(shots$ev.player.2, roster.unique$woi.id)]
  shots$P3 <- roster.unique$firstlast[match(shots$ev.player.3, roster.unique$woi.id)]
  
  awayshots <- select(subset(shots, ev.team == awayteam), P1, P2, P3, shot.feature, period, seconds, adjusted.distance, etype, newxc, newyc)
  homeshots <- select(subset(shots, ev.team == hometeam), P1, P2, P3, shot.feature, period, seconds, adjusted.distance, etype, newxc, newyc)
  awayteamname <- team.colors$TeamName[match(shots$awayteam[1], team.colors$team)]
  hometeamname <- team.colors$TeamName[match(shots$hometeam[1], team.colors$team)]
  
  
  outscript <- paste ('
  <div class="row-fluid">
  <span style="color:red">Goals</span> are in red. 
  <span style="color:black">Misses</span> are in black. 
  <span style="color:green">Blocks</span> are in green. 
  <span style="color:blue">Saves</span> are in blue.<br>
  <span style="font-size:108%;font-style:italic">Rush attempts</span> are larger and italicized.<br>
  <span style="font-size:108%;">Rebound attempts</span> are larger. 
  </div>
  <div class="row-fluid">
  <div class="span6" id="spAway"/>
  <div class="span6" id="spHome"/>

  </div>
<script>
  var awayShotsJson = ', toJSON(awayshots),';

  var homeShotsJson = ', toJSON(homeshots),';
  var dangerZones = ', toJSON(dangers), ';
                      
                      var awaySVG = d3.select("#spAway")
                      .append("svg");
                      
                      var rinkPlot1 = RINK_MAP({parent: awaySVG, fullRink: false, horizontal: false, data: awayShotsJson, desiredWidth: 380, 
                        toolTipValue: RINK_HELPERS.toolTipText, chartTitle: RINK_HELPERS.titleText("',awayteamname,'"), danger: dangerZones});
                      rinkPlot1();

                      var homeSVG = d3.select("#spHome")
                      .append("svg");
                      
                      var rinkPlot2 = RINK_MAP({parent: homeSVG, fullRink: false, horizontal: false, data: homeShotsJson, desiredWidth: 380, 
                        toolTipValue: RINK_HELPERS.toolTipText, chartTitle: RINK_HELPERS.titleText("',hometeamname,'"), danger: dangerZones});
                      rinkPlot2();
</script>')
  outscript
}

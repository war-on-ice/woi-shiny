## global file for goaltender server

#source("../warbase/R/urls.R")
#source("../warbase/R/variables.R")

library(warbase)

compress.goalietable.game <- function (dftable.sub) #, gamestest2=gamestest, startdate="2013-10-01", enddate="2014-09-30")
{dftable.sub %>% group_by(ID, Name, season, gcode) %>% goalie.compressor}

contracts <- {
    load("../common-data/contracts-complete.RData")
    contracts.complete %>% rename (AAV = aAAV)
}
##pending.FAs <- get(load("../common-data/contracts-FA2015.RData")) ## assemble.contract.data ()

url1 <- "http://war-on-ice.com/cap/current-contract-full.RData"
load (url(url1))
close(url(url1))

FA.one <- filter(team.two, NextYear == 2016)
RFA.list <- FA.one$woiid[grepl("rfa", FA.one$S20152016)]
UFA.list <- FA.one$woiid[grepl("ufa", FA.one$S20152016)]


goalie.var.choices <- goalie.variables[,2]

load ("../common-data/woi-common.RData")
team.options <- c("", sort(teams))

fixSeasons <- function(vec){
  vec <- gsub("-2", "\nRegular", vec)
  vec <- gsub("-3", "\nPlayoffs", vec)
  vec <- paste(substr(vec, 1, 4), "-", substr(vec, 5, nchar(vec)), sep = "")
}


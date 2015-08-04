## playertable global
## Last edited: August 31, 2014

library(shiny)
library(warbase)

load ("../common-data/woi-common.RData")


#load ("../common-data/roster-salaries.RData")
#while (nrow(roster.salaries) < nrow(roster.unique)) roster.salaries <- rbind(roster.salaries, NA)
#roster.salaries.straight <- data.frame (ID=rep(1:nrow(roster.salaries), length(seasons)),
#                                        season=rep(seasons, each=nrow(roster.salaries)),
#                                        Salary=round(c(roster.salaries)/1000000, 2))
#roster.salaries.straight$Name <- roster.unique$firstlast[roster.salaries.straight$ID]
##write.csv(roster.salaries.straight, "rosstr.csv")
##roster.unique$year <- substr(roster.unique$DOB, 1,4)

contracts <- {
    load("../common-data/contracts-complete.RData")
    contracts.complete %>% rename (AAV = aAAV) %>% filter (BoughtOut != "1")
}

##pending.FAs <- get(load("../common-data/contracts-FA2015.RData")) ## assemble.contract.data ()

url1 <- "http://war-on-ice.com/cap/current-contract-full.RData"
load (url(url1))
close(url(url1))

FA.one <- filter(team.two, NextYear == 2016)
RFA.list <- FA.one$woiid[grepl("rfa", FA.one$S20152016)]
UFA.list <- FA.one$woiid[grepl("ufa", FA.one$S20152016)]

team.options <- sort(teams)

## all.adjust
load ("../source-data/rink-counts.RData")

## sam.count <- 0
## Load data, either pre-compressed or otherwise.

data.choices <- c(t(outer(seasons, c("Regular", "Playoffs"), paste)))

#player.plot.variables <- player.variables[-(1:4),]
#player.var.choices <- player.plot.variables[,2]

fixSeasons <- function(vec){
  vec <- gsub("-2", "\nRegular", vec)
  vec <- gsub("-3", "\nPlayoffs", vec)
  vec <- paste(substr(vec, 1, 4), "-", substr(vec, 5, nchar(vec)), sep = "")
}

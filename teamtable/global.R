## global file for team server

library(shiny)
library(warbase)

#source("../warbase/R/urls.R")
#source("../warbase/R/variables.R")

data.choices <- c(t(outer(seasons, c("Regular", "Playoffs"), paste)))
##team.var.choices <- team.variables[,2]

fixSeasons <- function(vec){
  vec <- gsub("-2", "\nRegular", vec)
  vec <- gsub("-3", "\nPlayoffs", vec)
  vec <- paste(substr(vec, 1, 4), "-", substr(vec, 5, nchar(vec)), sep = "")
}

## all.adjust
load ("../source-data/rink-counts.RData")

## given the adjustments calculated in "event-count-bias", pick and choose which ones to add to each statistic.


    

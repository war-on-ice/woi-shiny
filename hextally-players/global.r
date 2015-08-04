
## hextally-player

# load data from file.
library(nhlplot)
source("../warbase/R/urls.R")

load ("../common-data/woi-common.RData")
roster.unique$firstlastpos <- paste0(roster.unique$firstlast, " - ", roster.unique$pos)

seasons <- seasons[-(1:2)]
##season.options <- c(seasons, "All Seasons")
quad.centers <- t(apply(quadsarrayplot[1:4,,], c(2,3), mean))

shottypes <- c("Backhand", "Deflected", "Slap", "Snap",
              "Tip-In", "Wrap", "Wrist", "Unspecified")
shottype.options <- c("All Shots", shottypes)

man.choices <- c("Even Strength", "Power Play", "Four On Four")

sumup <- function(simlist) {
  holder <- 0*unlist(simlist[[1]]); for (kk in 1:length(simlist)) holder <- holder + unlist(simlist[[kk]]); 
  array(holder, dim(as.matrix(simlist[[1]])))
}

swapout <- function (teamvec) {
    teamvec[teamvec %in% c("PHX","ARI")] <- "PHXARI"
    teamvec[teamvec %in% c("ATL","WPG")] <- "ATLWPG"
    teamvec
}

##rowfill <- function (in.matrix, target.rows


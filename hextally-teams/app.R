
## global.R

library(shiny)
library(MASS)
library(nhlplot)
library(warbase)

load ("../common-data/woi-common.RData")
swapout <- function (teamvec) {
    teamvec[teamvec %in% c("PHX","ARI")] <- "PHXARI"
    teamvec[teamvec %in% c("ATL","WPG")] <- "ATLWPG"
    teamvec
}
team.list <- sort(c(teams[!(teams %in% c("PHX","ARI","WPG","ATL"))], "PHXARI", "ATLWPG"))

seasons <- seasons[-(1:2)]
quad.centers <- t(apply(quadsarrayplot[1:4,,], c(2,3), mean))

shottypes <- c("Backhand", "Deflected", "Slap", "Snap",
              "Tip-In", "Wrap", "Wrist", "Unspecified")
shottype.options <- c("All Shots", shottypes)



sumup <- function(simlist) {
  holder <- 0*unlist(simlist[[1]]); for (kk in 1:length(simlist)) holder <- holder + unlist(simlist[[kk]]); 
  array(holder, dim(as.matrix(simlist[[1]])))
}


make.ref.counts <- function () {

    reduced.connect <- src_sqlite("../common-data/hextally.sqlite")
    sqlstatement <- paste0('SELECT hexblock FROM shotsteam')
    primer <- tbl(reduced.connect, sql(sqlstatement)) %>% group_by (hexblock) %>% summarize(count=n()) %>% filter(!is.na(hexblock)) %>% as.data.frame
    rawcounts <- rep(0, 1354); rawcounts[primer$hexblock] <- primer$count
    rawcounts[rawcounts < 20] <- 0
    save(rawcounts, file="rawcounts.RData")

}

load ("../common-data/rawcounts.RData")

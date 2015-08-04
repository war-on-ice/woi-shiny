## global file for team server

library(shiny)
library(warbase)
library(KernSmooth)

## all.adjust
load ("../source-data/rink-counts.RData")


load("../common-data/woi-common.RData")
season.breaks <- read.csv("../common-data/seasonbreaks.csv"); season.breaks[,1] <- as.Date(season.breaks[,1]); season.breaks[,2] <- as.Date(season.breaks[,2])

#main mod:
teams <- c(teams[!(teams %in% c("WPG","ATL","PHX","ARI"))], "ATL-WPG", "PHX-ARI")

#team.plot.variables <- team.variables[-(1:3),]
#team.var.choices <- team.plot.variables[,2]

team.options <- c(sort(teams))
crp <- function (points, values) rgb(colorRamp(points)(values), max=255)




rolling.mean <- function (this.vector, count=20) {
    sapply(1:length(this.vector), function(cc) if (cc >= count) mean(this.vector[max(cc-count+1, 1):cc], na.rm=TRUE) else NA) 
}

pick.columns <- function (in.object, extracols=NULL, group="Prime") {

    ##print(head(in.object))
    out2 <- if (!is.null(extracols)) data.frame(Team=in.object$Team, #in.object$team,
                                                extracols,
                                                PDO=in.object$PDO) else
    data.frame(Team=in.object$Team, PDO=in.object$PDO)
    ##print("pc1"); print(head(out2,2))

    if (group %in% c("Prime", "All"))   
        out2 <- cbind(out2, in.object[,c("GF","GA","G+/-","CF%","CP60","OFOn%","OSh%","OSv%","FO%","ZSO%")])

    
    if (group %in% c("High-Danger Chances", "All"))
        out2 <- cbind(out2, in.object[,c("HSCF%","HSC+/-","HSCF","HSCA","HSCF60","HSCA60","HSCP60")])
    
    if (group %in% c("Scoring Chances", "All"))
        out2 <- cbind(out2, in.object[,c("SCF%","SC+/-","SCF","SCA","SCF60","SCA60","SCP60")])
        
    if (group %in% c("Corsi", "Fenwick", "All"))
        out2 <- cbind(out2, in.object[,c("CF%","C+/-","CF","CA","CF60","CA60","CP60",
                                   "FF%","F+/-","FF60","FA60","FF","FA","FP60",
                                   "MSF", "MSA", "BSF","BSA")])
    
    if (group %in% c("Shot-Based", "Goal-Based", "All"))
        out2 <- cbind(out2, in.object[,c("SF%","S+/-", "SF60","SA60", "SF","SA",
                                   "GF60", "GA60","GF%", "OSh%", "OFenSh%", "OSv%",
                                   "OCOn%", "OFOn%")])
    
    
    if (group %in% c("Faceoffs", "Base Counts", "All"))
        out2 <- cbind(out2, in.object[,c("FO%", "FO_W", "FO_L",
                                   "ZSO","ZSN","ZSD", "HIT", "HIT-",
                                   "PN", "PN-", "PenD", "TOI")])    

    for (kk in 1:ncol(out2)) if (is.numeric(out2[,kk]) && !(names(out2)[kk] %in% c("date","GameDate"))) out2[,kk] <- round(out2[,kk],1)
    ##print("pc2"); print(head(out2,2))
    
    return(out2)

}



fixSeasons <- function(vec){
  vec <- gsub("-2", "\nRegular", vec)
  vec <- gsub("-3", "\nPlayoffs", vec)
  vec <- paste(substr(vec, 1, 4), "-", substr(vec, 5, nchar(vec)), sep = "")
}








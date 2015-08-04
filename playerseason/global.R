## global file for playerseason server
## Last edited: August 31, 2014

library(shiny)
library(warbase)
library(KernSmooth)

#source("../warbase/R/urls.R")
#source("../warbase/R/variables.R")

load ("../common-data/woi-common.RData")
season.breaks <- read.csv("../source-data/seasonbreaks.csv"); season.breaks[,1] <- as.Date(season.breaks[,1]); season.breaks[,2] <- as.Date(season.breaks[,2])
team.options <- c("All", sort(teams))

contracts <- {
    load("../common-data/contracts-complete.RData")
    contracts.complete %>% rename (AAV = aAAV) %>% filter (BoughtOut != "1")
}
## assemble.contract.data ()

player.plot.variables <- player.variables[-(1:4),]
player.var.choices <- player.plot.variables[,2]

## all.adjust
#load ("../source-data/rink-counts.RData")

fixSeasons <- function(vec){
  vec <- gsub("-2", "\nRegular", vec)
  vec <- gsub("-3", "\nPlayoffs", vec)
  vec <- paste(substr(vec, 1, 4), "-", substr(vec, 5, nchar(vec)), sep = "")
}

rolling.mean <- function (this.vector, count=20) {
  sapply(1:length(this.vector), function(cc) if (cc >= count) mean(this.vector[max(cc-count+1, 1):cc], na.rm=TRUE) else NA) 
}

player.pick.columns <- function (in.object, extracols=NULL, group="Prime") {
    
    out2 <- data.frame(Name=gsub(" ",".",roster.unique$firstlast[match(in.object$ID, roster.unique$woi.id)]),
                       extracols)

    ##print(head(in.object,2))
    if (group %in% c("Prime", "All")) {  
        out2$G <- in.object$G
        out2$A <- in.object$A1 + in.object$A2
        out2$P <- out2$G + out2$A
        
        out2$G60 <- in.object$G60
        out2$A60 <- in.object$A60
        out2$P60 <- in.object$P60
        
        out2 <- cbind(out2, in.object[,c("PenD", "CF%", "PDO", "PSh%", "ZSO%Rel", "TOI/Gm")])  #
    }


    if (group %in% c("High-Danger Chances", "All"))
        out2 <- cbind(out2, in.object[,c("iHSC","HSCF%Rel","HSCF%","HSCF%off", "HSC+/-",
                                   "HSCF","HSCA","HSCF60","HSCA60","HSCP60")])
    
    if (group %in% c("Scoring Chances", "All"))
        out2 <- cbind(out2, in.object[,c("iSC","SCF%Rel","SCF%","SCF%off", "SC+/-",
                                   "SCF","SCA","SCF60","SCA60","SCP60")])

    if (group %in% c("Corsi", "All"))
        out2 <- cbind(out2, in.object[,c("CF%Rel","CF%","CF%off",
                                         "C+/-","CF","CA","CF60","CA60","CP60",
                                         "OCOn%", "BK","AB", "iCF")])
    
    if (group %in% c("Fenwick", "All"))
        out2 <- cbind(out2, in.object[,c("FF%Rel", "FF%","FF%off", "F+/-",
                                         "FF","FA","FF60","FA60", "FP60",
                                         "OFOn%", "MS", "iFF")])
    
    if (group %in% c("Shot-Based", "All"))
        out2 <- cbind(out2, in.object[,c("SF%Rel","SF%","SF%off", "S+/-",
                                         "SF60","SA60", "SF","SA", "iSF")])
    
    if (group %in% c("Goal-Based", "All"))
        out2 <- cbind(out2, in.object[,c("GF%Rel","GF60", "GF%off", "GA60",
                                         "GF","GA","G+/-","GF%", "PSh%",
                                         "PFenSh%", "OSh%", "OFenSh%", "OSv%")])
    
    if (group %in% c("Faceoffs", "All"))
        out2 <- cbind(out2, in.object[,c("FO%^", "FO_W", "FO_L", "ZSO%", "ZSO","ZSN","ZSD")])
    
    if (group %in% c("Base Counts", "All"))
        out2 <- cbind(out2, in.object[,c("HIT", "HIT-",
                                   "G", "A1", "A2", "SH", "MS", "BK", "AB",
                                         "GV", "TK",
                                    "PN", "PN-","PenD60",
                                   "TOI", "TOIoff", "TOI%")])
    
    if (group %in% c("Teammate/Competition", "All"))
        out2 <- cbind(out2, in.object[,c("TOIT%", "TOIC%", "CorT%", "CorC%"   ##,"FenT%", "FenC%"
                                ,"tCF60","tCA60","cCF60","cCA60")])

    
    out2 <- out2[,unique(colnames(out2))]
    numeric.columns <- sapply(out2, class) 
    out2[,numeric.columns=="numeric"] <- round(out2[,numeric.columns=="numeric"], 1)

    return(out2)
}






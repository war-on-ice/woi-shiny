## global file for Goalie Season
## Last edited: May 19, 2015

library(shiny)
library(warbase)

load ("../common-data/woi-common.RData")
#contracts <- get(load("../common-data/contracts-complete.RData")) ## assemble.contract.data ()
contracts <- {
    load("../common-data/contracts-complete.RData")
    contracts.complete %>% rename (AAV = aAAV)
}

goalie.var.choices <- goalie.variables[,2]

fixSeasons <- function(vec) return(vec)

rolling.mean <- function (this.vector, count=20) {
    sapply(1:length(this.vector), function(cc) if (cc >= count) mean(this.vector[max(cc-count+1, 1):cc], na.rm=TRUE) else NA) 
}



refine.data <- function (dftable, totals) {  #=total.shots
    
    #dftable <- as.data.frame(dftable)
    #print(head(dftable,2))
    #print(total.shots)
    dftable$Name <- gsub(" ",".",dftable$Name)
    dftable <- mutate(dftable,
                      G=G.L + G.M + G.H,
                      S=S.L + S.M + S.H,
                      Sh=G + S,
                      "Sv%"=round(S/Sh * 100, 2),
                      "Sv%L"=round(S.L/(G.L+S.L) * 100, 2),
                      "Sv%M"=round(S.M/(G.M+S.M) * 100, 2),
                      "Sv%H"=round(S.H/(G.H+S.H) * 100, 2),
                      SA60=round(Sh/TOI*60, 2))

    dftable[["AdSv%"]] <- round((dftable[["Sv%L"]]*totals[1] +
                                 dftable[["Sv%M"]]*totals[2] +
                                 dftable[["Sv%H"]]*totals[3])/sum(totals), 2)
    dftable
}





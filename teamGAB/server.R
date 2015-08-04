## GAB server
## acthomas, 8-20-14

library(shiny)
load("teamGAB.RData")


fixSeasons <- function(vec){
  vec <- gsub("-2", "\nRegular", vec)
  vec <- gsub("-3", "\nPlayoffs", vec)
  vec <- paste(substr(vec, 1, 4), "-", substr(vec, 5, nchar(vec)), sep = "")
}


full.object <- cbind(full.object.es, full.object.pp[,-(1:2)])
full.object$TotalValue <- full.object$GAB.Net + full.object$GAB.PPSHNet

shinyServer(function(input, output, session){
  addPercent <- function(dtf, colname)  return(paste(dtf[,colname], "%", sep = ""))
  addPercentAndPlus <- function(dtf, colname)  return(paste(ifelse(dtf[,colname] >=0, "+", ""), dtf[,colname], "%", sep = ""))
  addPlus <- function(dtf, colname)  return(paste(ifelse(dtf[,colname] >=0, "+", ""), dtf[,colname], sep = ""))
  
  output$mytable = renderDataTable({
    out <- full.object[rev(order(full.object[,11])),]
    #out[,6:11] <- round(out[,6:11],1)
    #out[,3:5] <- round(out[,3:5],2)
    out[,-(1:2)] <- round(out[,-(1:2)], 1)
    
    if (!input$detail)
      out <- out[,c("Season","Team","TotalValue",
                                     "GAB.Net",
                                     "GAB.Offense","GAB.Defense",
                                     #"GAB.PPSHNet",
                                     "GAB.PPNet", "GAB.SHNet")]
    
    for(ii in 3:ncol(out))  out[,ii] <- round(out[,ii], 2)
    out$Season <- fixSeasons(out$Season)
    
    out
  }
  #        ,options=list(aoColumns=list(list(bSearchable=TRUE),list(bSearchable=TRUE),)
  )
})

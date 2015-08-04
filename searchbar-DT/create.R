
library(warbase)

teamtime.url.raw <- function (team)
    paste0 ("http://www.war-on-ice.com/teambygame.html?team=",team)
teamhex.url.raw <- function (team)
    paste0 ("http://www.war-on-ice.com/hexteams.html?team=",team)

playerseason.url.raw <- function (player)
    paste0 ("http://www.war-on-ice.com/playerseason.html?name1=",
            gsub("[ \\.]","0",player))    
playerhex.url.raw <- function (player)
    paste0 ("http://www.war-on-ice.com/hexplayers.html?name=",
            gsub("[ \\.]","0",player))

playersalary.url.raw <- function (woiid)
    paste0 ("http://www.war-on-ice.com/cap/",woiid,".html")    


goalieseason.url.raw <- function (player)
    paste0 ("http://www.war-on-ice.com/goalieseason.html?name1=",
            gsub("[ \\.]","0",player))
goaliehex.url.raw <- function (player)
    paste0 ("http://www.war-on-ice.com/hexgoalies.html?name=",
            gsub("[ \\.]","0",player))




get.initial.table <- function () {
    load ("../common-data/woi-common.RData")
    load ("../source-data/raw-contracts-downloaded.RData")

    newplayer <- d1$Player[,-2] %>% rename(woi.id=woiid)
    roster.unique <- rbind_list (roster.unique,
                                 subset(newplayer, woi.id %in% setdiff(newplayer$woi.id, roster.unique$woi.id)) %>% select (woi.id, last, first, firstlast, pos, DOB))


    
    roster.unique$pos[is.na(roster.unique$pos)] <- ""
    teambits <- read.csv ("../source-data/teamcolors.csv")
    
    players <- data.frame(display=roster.unique$firstlast,
                          statlink=playerseason.url.raw(roster.unique$firstlast),
                          salarylink=playersalary.url.raw(roster.unique$woi.id),
                          hexlink=playerhex.url.raw(roster.unique$firstlast),
                          stringsAsFactors=FALSE)
    
    players$statlink[roster.unique$pos == "G"] <- goalieseason.url.raw(players$display[roster.unique$pos == "G"])
    players$hexlink[roster.unique$pos == "G"] <- goaliehex.url.raw(players$display[roster.unique$pos == "G"])

    teams <- data.frame(display=teambits$TeamName,
                        statlink=teamtime.url.raw(teambits$team),
                        salarylink=playersalary.url.raw(teambits$team),
                        hexlink=teamhex.url.raw(teambits$team),
                        stringsAsFactors=FALSE)                        

    player.out <- data.frame (ID = players$display,
                              Statistics = paste0 ('<a href="',players$statlink,'" target="_blank">Statistics</a>'),
                              Contracts = paste0 ('<a href="',players$salarylink,'" target="_blank">Contracts</a>'),
                              Hextally = paste0 ('<a href="',players$hexlink,'" target="_blank">Hextally</a>'))

    team.out <- data.frame (ID = teams$display,
                            Statistics = paste0 ('<a href="',teams$statlink,'" target="_blank">Statistics</a>'),
                            Contracts = paste0 ('<a href="',teams$salarylink,'" target="_blank">Contracts</a>'),
                            Hextally = paste0 ('<a href="',teams$hexlink,'" target="_blank">Hextally</a>'))

    output <- rbind(team.out, player.out[-1,])
    colnames(output) <- c("Player/Team","","","")
    output               

}

prime <- get.initial.table()
make.datatable.whole (prime, pageLength=4, outfile="search.html")

#library(DT)
#datatable(prime, escape=FALSE, rownames=FALSE, colnames=rep("",ncol(prime)), style="bootstrap")

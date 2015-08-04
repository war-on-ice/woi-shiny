

teamtime.url <- function (team)
    paste0 ("<a href=\"http://www.war-on-ice.com/teambygame.html?team=",team,"\" target=\"_blank\">",team,"</a>")

playerseason.url <- function(player)
    paste0 ("<a href=\"http://www.war-on-ice.com/playerseason.html?player1=",
            gsub("[ \\.]","0",player),"\" target=\"_blank\">",player,"</a>")
    

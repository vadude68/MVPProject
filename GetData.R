##This function takes a vector of boxscore webpages (acquired through
##getboxscores) and returns all on/off/net data from those games

getdata <- function(boxscores) {
        options(warn = -1)
        suppressMessages(library(stringr))
        suppressMessages(library(rvest))
        suppressMessages(library(XML))
        linkstart <- "http://www.basketball-reference.com/boxscores/plus-minus/"
        plusminuslinks <- vector()
        for (i in 1:length(boxscores)) {
                dateteam <- str_sub(boxscores[i], start = -17)
                plusminuslinks[i] <- str_c(linkstart, dateteam)
        }
        data <- vector()
        for (i in 1:length(plusminuslinks)) {
                plusminuspage <- html(plusminuslinks[i])
                plusminusnodes <- html_nodes(plusminuspage, ".padding_half.x_small_text")
                netfigures <- html_text(plusminusnodes)
                data <- c(data, netfigures)
        }
        return(data)
}
##This function takes a date (in the form mmddyy), goes to 
##basketball-reference.com, and returns all the web addresses 
##of the box scores for that day.

getboxscores <- function(date) {
        options(warn = -1)
        suppressMessages(library(lubridate))
        suppressMessages(library(stringr))
        suppressMessages(library(rvest))
        suppressMessages(library(XML))
        date <- mdy(date)
        m <- month(date)
        d <- day(date)
        y <- year(date)
        link <- str_c("http://www.basketball-reference.com/boxscores/index.cgi?month=", m, "&day=", d, "&year=", y)
        dailygamepage <- html(link)
        alllinks <- xpathSApply(dailygamepage, "//a/@href")
        boxscores <- grep("boxscores/20", alllinks, value = TRUE)
        boxscores <- unique(boxscores)
        for (i in 1:length(boxscores)) {
                boxscores[i] <- str_c("http://www.basketball-reference.com", boxscores[i])
        }
        return(boxscores)
}
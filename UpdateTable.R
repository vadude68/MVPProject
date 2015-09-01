##This function takes a data frame (acquired through cleandata) and an
##optional text file with on/off/net information to date.  It then 
##updates the file with the clean data and.  If no file exists, it saves
##the passed data frame as "onoffnet.txt".  Finally, it returns the 
##updated or new file as a data frame.

updatetable <- function(df, file = NULL, alpha = FALSE) {
        options(warn = -1)
        suppressMessages(library(plyr))
        if (is.null(file)) {
                updateddf <- df
                write.table(updateddf, "onoffnet.txt")
        } else {
                olddata <- read.table(file)
                newdf <- rbind(df, olddata)
                updateddf <- ddply(newdf, "Player", summarise, On = sum(On), Off = sum(Off), Net = sum(Net))
                updateddf <- updateddf[order(updateddf$Net, decreasing = TRUE), ]
                write.table(updateddf, file)
        }##this part doesn't work
        if (alpha == TRUE) {
                updateddf <- updateddf[sort(updateddf$Player), ]
        }
        return(updateddf)
}
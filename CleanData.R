##This function takes the raw on/off/net data (acquired through getdata)
##and returns the same data in a tidy dataframe.

cleandata <- function(data) {
        options(warn = -1)
        suppressMessages(library(stringr))
        df <- data.frame(Player = character(0), On = integer(0), Off = integer(0), Net = integer(0), stringsAsFactors = F)
        splitstrings <- strsplit(data, "\\(")
        for (i in 1:length(data)) {
                onoffnetfind <- gregexpr("-*[0-9]+", data[i])
                onoffnet <- as.numeric((unlist(regmatches(data[i], onoffnetfind))))
                df[i, 2] <- onoffnet[1]
                df[i, 3] <- onoffnet[2]
                df[i, 4] <- onoffnet[3]
                df[i, 1] <- str_trim(splitstrings[[i]][1])
        }
        return(df)
}
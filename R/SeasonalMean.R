#' Calculate seasonal means from data.table
#'
#' @param data data table containing the columns date (Date object) and value (the variable)
#' @param season string descripting the season to average, e.g., 'DJF'
#' @param na.rm a logical value indicating whether 'NA' values should be 
#'        stripped before the computation proceeds.
#' @return data table with seasonal averaged values.
#' @examples
#'  mydata <- data.frame(date = seq.Date(as.Date('1990-01-01'),
#'                                   as.Date('2000-12-31'),
#'                                   by = '1 months'),
#'                                   value = rnorm(11*12))
#' SeasonalMean(mydata, 'DJF')
#' SeasonalMean(mydata, 'DJFMAM')

SeasonalMean <- function(data, season, na.rm=FALSE){
    library(stringr)
    months <- c('J','F','M','A','M','J','J','A','S','O','N','D')
    # check if season is valid
    if(length(str_split(season, '')[[1]])>12){
        stop(paste(season, 'not valid'))
    }
    monjoin = paste(months, collapse = '')
    monjoin = paste(monjoin, monjoin, sep = '')
    if (any(is.na(str_match(monjoin, season)))){
        stop(paste(season, 'not valid'))
    }
    # get month indices for each months
    idx <- str_locate(monjoin, season)
    months_idx <- seq(idx[1], idx[2])
    # check if special case (e.g., DJF, where season covers two years)
    normal <- TRUE
    if(max(months_idx)>=13){
        normal <- FALSE
    }
    # prepare output array
    data$year <- as.POSIXlt(data$date)$year + 1900
    data$mon <- as.POSIXlt(data$date)$mon + 1
    years_unique <- unique(data$year)
    newdata <- NULL
    # TODO:
    #   allocate newdata , remove rbind's (probably faster)
    if(normal){
        for(y in years_unique){
            subd <- subset(data, year == y & mon %in% months_idx)
            if(nrow(subd) < length(months_idx)){
               warning(paste("WARNING: missing months in year: ", y, '[', nrow(subd),'/',
                            length(months_idx),']'))
            }
            newdata <- rbind(newdata,
                             data.frame(date = as.Date(paste(y, months_idx[length(months_idx)], 
                                                             '15', sep = '-')),
                                        value = mean(subd$value, na.rm = na.rm)))
        }
    } else {
        nyear <- length(years_unique)
        nextyrmonths <- months_idx[months_idx>12] - 12
        thisyrmonths <- months_idx[months_idx<=12] 
        for(i in seq(1, nyear)){
            thisyear <- years_unique[i]
            if(i==1){
                subd <- subset(data, year == thisyear & mon %in%  nextyrmonths)
                if(nrow(subd) < length(months_idx)){
                    warning(paste("WARNING: missing months in year: ", thisyear, '[', nrow(subd),'/',
                                length(months_idx),']'))
                }
                newdata <- rbind(newdata,
                             data.frame(date = as.Date(paste(thisyear, 
                                                             nextyrmonths[length(nextyrmonths)], 
                                                             '15', sep = '-')),
                                        value = mean(subd$value, na.rm = na.rm)))
            }
            if(i<nyear){
                nextyear <- years_unique[i+1]
                subd1 <- subset(data, year == thisyear & mon %in%  thisyrmonths)
                subd2 <- subset(data, year == nextyear & mon %in%  nextyrmonths)
                subd <- rbind(subd1, subd2)
                if(nrow(subd) < length(months_idx)){
                    warning(paste("WARNING: missing months in year: ", thisyear, 'or', 
                                nextyear, '[', nrow(subd),'/', length(months_idx),']'))
                }
                newdata <- rbind(newdata,
                             data.frame(date = as.Date(paste(nextyear, nextyrmonths[length(nextyrmonths)], 
                                                             '15', sep='-')),
                                        value = mean(subd$value, na.rm=na.rm)))
            } else {
                subd <- subset(data, year == thisyear & mon %in%  thisyrmonths)
                if(nrow(subd) < length(months_idx)){
                    warning(paste("WARNING: missing months in year: ", thisyear, 
                                '[', nrow(subd),'/', length(months_idx),']'))
                }
                newdata <- rbind(newdata,
                             data.frame(date = as.Date(paste(thisyear, thisyrmonths[length(thisyrmonths)], 
                                                             '15', sep='-')),
                                        value = mean(subd$value, na.rm=na.rm)))
            }
        }
   }
    return(newdata)
}




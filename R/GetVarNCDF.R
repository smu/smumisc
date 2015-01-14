
#' Tries to convert date information, e.g, from netcdf file, to Date or Datetime object.
#'  
#' @param time vector containing the dates
#' @param unit string description the date format
#' @return vector of Date or Datetime objects in case of success
#'
#' 
#' @examples
#'    ConvertTime(seq(0,1000), 'days since 1900-01-01 00:00:00')
#'
#'
ConvertTime  <- function(time, unit){
    if (class(time) == 'Date'){
        return(time)
    }
    if (length(grep('day', unit, ignore.case = TRUE)) > 0){
        if (length(grep('days since', unit, ignore.case = TRUE)) > 0){
            #relative time scale
            first_time = sub(".* (\\d+-\\d+-\\d+) .*", "\\1", unit, perl = TRUE)
            time <- as.Date(time, origin = as.Date(first_time))
            return(time)
        } else {
            if (length(grep('day as', unit)) > 0){
                # absolute timescale
                # force length==8 (for years < 1000)
                #  time <- sprintf("%08i", time)
                time <- as.Date(as.character(time), format = '%Y%m%d')
                return(time)
            } else {
                stop("ERROR: timescale unknown: ", time.unit)
            }
        }
    } else {
        if (length(grep('minutes since', unit)) > 0)  {
            #first_time <- sub(".* (\\d+-\\d+-\\d+ \\d{2}:\\d{2}:\\d{2}).*", "\\1", unit, perl=TRUE)
            #time = time * 60 # da R sekunden erwartet
            #time <- as.POSIXlt(time, origin=first_time)
            first_time <- sub(".* (\\d+-\\d+-\\d+) \\d{2}:\\d{2}:\\d{2}.*", "\\1", unit, perl = TRUE)
            time <- time/1440
            time <- as.Date(time, origin = first_time)
            return(time)
         } else {
            if (length(grep('hours since', unit)) > 0)  {
                first_time <- sub(".* (\\d+-\\d+-\\d+ \\d{2}:\\d{2}:\\d{2}).*", "\\1", unit, perl = TRUE)
                time = time * 60 * 60 # R expects seconds here
                time <- as.POSIXlt(time, origin = first_time)
                return(time)
            } else {
                if (length(grep('months since', unit)) > 0)  {
                    first_time <- sub(".* (\\d+-\\d+-\\d+ \\d{2}:\\d{2}:\\d{2}).*", "\\1", unit, perl = TRUE)
                    #  first_time <- as.Date(as.POSIXlt(first_time))
                    #  time <- seq.Date( first_time, length.out = length(time), by='month')
                    time = time * 24 * 60 * 60 * 365.2425/12     # R expects seconds
                    time <- as.POSIXlt(time, origin = first_time)
                    return(time)
                } else {
                    stop("ERROR: timescale unknown: ", time.unit)
                    # absolute time
                    #time <- sprintf("%08i", time)
                    # strange behavior: YYYY0229 gets NA
                    # hack
                    #time  <- gsub('0229', '0228', time)
                    #time <- as.Date(time, format="%Y%m%d")
                }
            }
        }
    }
}# end function: convert_time


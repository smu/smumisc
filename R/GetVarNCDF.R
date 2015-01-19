
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
            first_time = sub("days since (\\d+-\\d+-\\d+)[ .]?", "\\1", unit, perl = TRUE)
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



# the next one is the general function, shortcuts for different
# cases follow below.

#' Extracts data from netcdf file and returns a data.frame.
#'
#' @param ncfile filename of the netcdf file
#' @param varname name of the variable to extract
#' @param vector of dimension variables, e.g, c('time','lat','lev'). 
#'        If all dimensions are zero, e.g., global mean values, and empty vector or NULL 
#'        should be used.
#' @param convert_date Convert time information into Date/Datetime object. Defaults to TRUE.
#'        This parameter is only used, when a date/time variable is found.
#' @return A data frame containing at least a single column (value) for the variable value(s) 
#'         and possible additional columns for each dimension variable.
#' @examples
#'      
#'      ncfile <- 'example.nc'
#'      d <- GetVarNCDF(ncfile, 'slp', c('lon','lat','time'))
#'
GetVarNCDF <- function(ncfile, varname, finddim, convert_date = TRUE){
    require(ncdf, quietly = TRUE)
    f = open.ncdf(ncfile)
    # get variable
    variable <- get.var.ncdf(f, varname)
    dims <- dim(variable)
    ndim <- length(finddim)
    if ((length(dims) != ndim) & (!is.null(finddim))) {
        msg <- paste('Unexpected number of dimensions : got', 
            length(dim(variable)), 'instead of ',ndim )
        stop(msg)
    }
    # find the dimensions
    dims.found <- rep(NA, ndim)
    j <- 1 
    for(i in seq_along(f$var[[varname]]$dim)){
        vdim <- f$var[[varname]]$dim[[i]]
        if((vdim$len > 1) & (vdim$name %in% finddim)){
            values <- get.var.ncdf(f, vdim$name)
            if ((vdim$name == 'time') & convert_date){
                time.unit <- att.get.ncdf(f, 'time', 'units')
                values <- as.Date(ConvertTime(values, time.unit[2]))
            }
            assign(vdim$name, values)
            dims.found[j] <- vdim$name
            j <- j+1
        }
    }
    if (any(is.na(dims.found))){
        msg <- paste("Couldn't find dimension variable(s): ", 
            paste(dims.found, collapse = ','))
        stop(msg)
    }
    close.ncdf(f)
    # check that all finddim were found
    for (dimname in finddim){
        if (dimname %in% dims.found){
            next
        } else {
            msg <- paste("Couldn't find", dimname, "dimension.\ndims found:",
                         paste(dims.found, collapse = ', '))
            stop(msg)
        }
    }
    # TODO: find a more general way for the code below!
    if(ndim == 0){
        d <- data.frame(value = as.numeric(variable))
    }
    if(ndim == 1){
        dim1 <- get(dims.found[1])
        d <- data.frame(dim1 = dim1, value = as.numeric(variable))
        names(d)[1] <- dims.found
    }
    if(ndim == 2){
        dim1 <- get(dims.found[1])
        dim2 <- get(dims.found[2])
        # create data.frame
        d <- data.frame(dim1 = rep(dim1, length(dim2)), 
            dim2 = rep(dim2, each = length(dim1)), 
            value = as.numeric(variable))
        names(d)[1:2] <- dims.found
    }
    if(ndim == 3){
        dim1 <- get(dims.found[1])
        dim2 <- get(dims.found[2])
        dim3 <- get(dims.found[3])
        d <- data.frame(dim1 = rep(dim1, length(dim2)*length(dim3)),
                dim2 = rep(rep(dim2, each = length(dim1)), length(dim3)),
                dim3 = rep(dim3, each = length(dim1)*length(dim2)),
                value = as.numeric(variable)
            )
        names(d)[1:3] <- dims.found
    }
    if(ndim == 4){
        dim1 <- get(dims.found[1])
        dim2 <- get(dims.found[2])
        dim3 <- get(dims.found[3])
        dim4 <- get(dims.found[4])
        d <- data.frame(
                    dim1 = rep(dim1, length(dim2)*length(dim3)*length(dim4)),
                    dim2 = rep(rep(dim2, each = length(dim1)), length(dim3)*length(dim4)),
                    dim3 = rep(rep(dim3, each = length(dim1)*length(dim2)), length(dim4)),
                    dim4 = rep(dim4, each = length(dim1)*length(dim2)*length(dim3)),
                    value = as.numeric(variable)
            )
        names(d)[1:4] <- dims.found
    }
    if(ndim>4){
        msg <- 'More than 4 dimensions are currently not supported. sorry!'
        stop(msg)
    }
    if('time' %in% dims.found){
        names(d)[which(names(d)=='time')] <- 'date'
    }
    return(d)
}


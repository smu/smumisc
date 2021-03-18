
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
            first_time <- sub(".* (\\d+-\\d+-\\d+ \\d{2}:\\d{2}:\\d{2}).*", "\\1", unit, perl = TRUE)
            #time <- time/1440
            time <- time * 60
            # TODO: consider timezone
            time <- as.POSIXlt(time, origin = first_time)
            return(time)
         } else {
            if (length(grep('hours since', unit)) > 0)  {
                first_time <- sub(".* (\\d+-\\d+-\\d+ \\d{2}:\\d{2}:\\d{2}).*", "\\1", unit, perl = TRUE)
                time = time * 60 * 60 # R expects seconds here
                # TODO: consider timezone
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


useNC4 <- TRUE

if(!require(ncdf4)) {
  require(ncdf)
  useNC4 <- FALSE
} else {
}

# the next one is the general function, shortcuts for different
# cases follow below.

#' Extracts data from netcdf file and returns a data.frame.
#'
#' @param ncfile filename of the netcdf file.
#' @param varname name of the variable to extract. 
#' @param finddim vector of dimension variables, e.g, c('time','lat','lev'). 
#'        For dimensionless variables, e.g, global mean values, and empty vector or NULL
#'        should be used.
#' @param convert_date Convert time information into Date/Datetime object. Defaults to TRUE.
#'        This parameter is only used, when a date/time variable is found.
#' @return A data frame containing at least a single column (value) for the variable value(s) 
#'         and possible additional columns for each dimension variable.
#' @examples
#'      
#'      ncfile <- system.file("extdata", "example_tll.nc", package = "smumisc")
#'      d <- GetVarNCDF(ncfile, 'slp', c('lon','lat','time'))
#'
GetVarNCDF <- function(ncfile, varname, finddim, convert_date = TRUE){
    if (!useNC4) {
      require(ncdf, quietly = TRUE)
      f = open.ncdf(ncfile)
      # get variable
      variable <- get.var.ncdf(f, varname)
    } else {
      require(ncdf4, quietly = TRUE)
      f = nc_open(ncfile)
      # get variable
      variable <- ncvar_get(f, varname)
    }
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
            if (!useNC4) {
              values <- get.var.ncdf(f, vdim$name)
            } else {
              values <- ncvar_get(f, vdim$name)
            }
            if ((vdim$name == 'time') & convert_date){
                if (!useNC4) {
                  time.unit <- att.get.ncdf(f, 'time', 'units')
                } else {
                  time.unit <- ncatt_get(f, 'time', 'units')
                }
                values <- ConvertTime(values, time.unit[2])
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
    if (!useNC4) {
      close.ncdf(f)
    } else {
      nc_close(f)
    }
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


#' Returns dimension of a netcdf variable.
#'
#' @param ncfile filename of the netcdf file.
#' @param varname name of the variable to extract. 
#' @return a vector of dimensions names.
#'
GetDimNCDF <- function(ncfile, varname){
    if (!useNC4) {
      require(ncdf, quietly = TRUE)
      f = open.ncdf(ncfile)
      # get variable
      variable <- get.var.ncdf(f, varname)
    } else {
      require(ncdf4, quietly = TRUE)
      f = nc_open(ncfile)
      # get variable
      variable <- ncvar_get(f, varname)
    }
    dims.found <- NULL
    for(i in seq_along(f$var[[varname]]$dim)){
        vdim <- f$var[[varname]]$dim[[i]]
        if(vdim$len > 1){
            dims.found <- c(dims.found, vdim$name)
        }
    }
    if (!useNC4) {
      close.ncdf(f)
    } else {
      nc_close(f)
    }
    return(dims.found)
}



#' GetVarNCDFT is a shortcut for GetVarNCDF(ncfile, varname, c('time')
#'
#' @param ncfile filename of the netcdf file
#' @param varname name of the variable to extract
#' @param convert_date Convert time information into Date/Datetime object. Defaults to TRUE.
#' @return A data frame containing two columns (date, value)
#' @examples
#'      
#'      ncfile <- system.file("extdata", "example_t.nc", package = "smumisc")
#'      d <- GetVarNCDFT(ncfile, 'slp')
#'
#'
GetVarNCDFT <- function(ncfile, varname, convert_date = TRUE) {
    return(GetVarNCDF(ncfile, varname, c('time'), 
                        convert_date = convert_date))
}


#' GetVarNCDFTLL extract variable with time dimensions and two of lat|lon|lev.
#'
#' @param ncfile filename of the netcdf file
#' @param varname name of the variable to extract
#' @param convert_date Convert time information into Date/Datetime object. Defaults to TRUE.
#' @return A data frame containing four columns.
#' @examples
#'      
#'      ncfile <- system.file("extdata", "example_tll.nc", package = "smumisc")
#'      d <- GetVarNCDFTLL(ncfile, 'slp')
#'
#'
GetVarNCDFTLL <- function(ncfile, varname, convert_date = TRUE) {
    dim.names <- GetDimNCDF(ncfile, varname)
    finddims <- c()
    LATNAMES <- c('lat','Lat','latitude','Latitude')
    for(lat in LATNAMES){
        if(lat %in% dim.names){
            finddims <- c(finddims, lat)
        }
    }
    LONNAMES <- c('lon','Lon','longitude','Longitude')
    for(lon in LONNAMES){
        if(lon %in% dim.names){
            finddims <- c(finddims, lon)
        }
    }
    LEVNAMES <- c('lev','Lev','plev')
    for(lev in LEVNAMES){
        if(lev %in% dim.names){
            finddims <- c(finddims, lev)
        }
    }
    # more than two of lat|lon|lev were found
    if(length(finddims)!=2){
        msg <- paste("Found more|less than two dimensions:", paste(finddims, collapse=','))
        stop(msg)
    }
    finddims <- c(finddims, 'time')
    return(GetVarNCDF(ncfile, varname, finddims, 
                        convert_date = convert_date))
}


#'
#' GetVarNCDFLL extract variable with two dimensions of lat|lon|lev.
#'
#' @param ncfile filename of the netcdf file
#' @param varname name of the variable to extract
#' @return A data frame containing three columns.
#'      
#'
#'
GetVarNCDFLL <- function(ncfile, varname) {
    dim.names <- GetDimNCDF(ncfile, varname)
    finddims <- c()
    LATNAMES <- c('lat','Lat','latitude','Latitude')
    for(lat in LATNAMES){
        if(lat %in% dim.names){
            finddims <- c(finddims, lat)
        }
    }
    LONNAMES <- c('lon','Lon','longitude','Longitude')
    for(lon in LONNAMES){
        if(lon %in% dim.names){
            finddims <- c(finddims, lon)
        }
    }
    LEVNAMES <- c('lev','Lev','plev')
    for(lev in LEVNAMES){
        if(lev %in% dim.names){
            finddims <- c(finddims, lev)
        }
    }
    # more than two of lat|lon|lev were found
    if(length(finddims)!=2){
        msg <- paste("Found more|less than two dimensions:", paste(finddims, collapse=','))
        stop(msg)
    }
    return(GetVarNCDF(ncfile, varname, finddims))
}




#'
#' GetVarNCDFTL extract variable with time dimension and one of lat|lon|lev.
#'
#' @param ncfile filename of the netcdf file
#' @param varname name of the variable to extract
#' @param convert_date Convert time information into Date/Datetime object. Defaults to TRUE.
#'        This parameter is only used, when a date/time variable is found.
#' @return A data frame containing three columns.
#'      
#'
#'
GetVarNCDFTL <- function(ncfile, varname, convert_date = TRUE) {
    dim.names <- GetDimNCDF(ncfile, varname)
    finddims <- c()
    LATNAMES <- c('lat','Lat','latitude','Latitude')
    for(lat in LATNAMES){
        if(lat %in% dim.names){
            finddims <- c(finddims, lat)
        }
    }
    LONNAMES <- c('lon','Lon','longitude','Longitude')
    for(lon in LONNAMES){
        if(lon %in% dim.names){
            finddims <- c(finddims, lon)
        }
    }
    LEVNAMES <- c('lev','Lev','plev')
    for(lev in LEVNAMES){
        if(lev %in% dim.names){
            finddims <- c(finddims, lev)
        }
    }
    # more than two of lat|lon|lev were found
    if(length(finddims)!=1){
        msg <- paste("Found more|less than one dimensions:", paste(finddims, collapse=','))
        stop(msg)
    }
    finddims <- c(finddims, 'time')

    return(GetVarNCDF(ncfile, varname, finddims, 
                        convert_date = convert_date))
}

#'
#' GetVarNCDFLLL extract variable with lat, lon, and lev dimension.
#'
#' @param ncfile filename of the netcdf file
#' @param varname name of the variable to extract
#' @return A data frame containing four columns.
#'      
#'
#'
GetVarNCDFLLL <- function(ncfile, varname) {
    dim.names <- GetDimNCDF(ncfile, varname)
    finddims <- c()
    LATNAMES <- c('lat','Lat','latitude','Latitude')
    for(lat in LATNAMES){
        if(lat %in% dim.names){
            finddims <- c(finddims, lat)
        }
    }
    LONNAMES <- c('lon','Lon','longitude','Longitude')
    for(lon in LONNAMES){
        if(lon %in% dim.names){
            finddims <- c(finddims, lon)
        }
    }
    LEVNAMES <- c('lev','Lev','plev')
    for(lev in LEVNAMES){
        if(lev %in% dim.names){
            finddims <- c(finddims, lev)
        }
    }
    # more than two of lat|lon|lev were found
    if(length(finddims)!=3){
        msg <- paste("Found more|less than three dimensions:", paste(finddims, collapse=','))
        stop(msg)
    }
    return(GetVarNCDF(ncfile, varname, finddims))
}


#'
#' GetVarNCDFTLLL extract variable with time, lat, lon, and lev dimension.
#'
#' @param ncfile filename of the netcdf file
#' @param convert_date Convert time information into Date/Datetime object. Defaults to TRUE.
#'        This parameter is only used, when a date/time variable is found.
#' @param varname name of the variable to extract
#' @return A data frame containing five columns.
#'      
#'
#'
GetVarNCDFTLLL <- function(ncfile, varname, convert_date = TRUE) {
    dim.names <- GetDimNCDF(ncfile, varname)
    finddims <- c()
    LATNAMES <- c('lat','Lat','latitude','Latitude')
    for(lat in LATNAMES){
        if(lat %in% dim.names){
            finddims <- c(finddims, lat)
        }
    }
    LONNAMES <- c('lon','Lon','longitude','Longitude')
    for(lon in LONNAMES){
        if(lon %in% dim.names){
            finddims <- c(finddims, lon)
        }
    }
    LEVNAMES <- c('lev','Lev','plev')
    for(lev in LEVNAMES){
        if(lev %in% dim.names){
            finddims <- c(finddims, lev)
        }
    }
    # more than two of lat|lon|lev were found
    if(length(finddims)!=3){
        msg <- paste("Found more|less than three dimensions:", paste(finddims, collapse=','))
        stop(msg)
    }
    finddims <- c(finddims, 'time')
    return(GetVarNCDF(ncfile, varname, finddims, 
                        convert_date = convert_date))
}

#'
#' GetVarNCDFL extract variable with one dimension of lat|lon|lev.
#'
#' @param ncfile filename of the netcdf file
#' @param varname name of the variable to extract
#' @return A data frame containing two columns.
#'      
#'
#'
GetVarNCDFL <- function(ncfile, varname) {
    dim.names <- GetDimNCDF(ncfile, varname)
    finddims <- c()
    LATNAMES <- c('lat','Lat','latitude','Latitude')
    for(lat in LATNAMES){
        if(lat %in% dim.names){
            finddims <- c(finddims, lat)
        }
    }
    LONNAMES <- c('lon','Lon','longitude','Longitude')
    for(lon in LONNAMES){
        if(lon %in% dim.names){
            finddims <- c(finddims, lon)
        }
    }
    LEVNAMES <- c('lev','Lev','plev')
    for(lev in LEVNAMES){
        if(lev %in% dim.names){
            finddims <- c(finddims, lev)
        }
    }
    # more than two of lat|lon|lev were found
    if(length(finddims)!=1){
        msg <- paste("Found more|less than one dimension:", paste(finddims, collapse=','))
        stop(msg)
    }
    return(GetVarNCDF(ncfile, varname, finddims))
}

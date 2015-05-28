#' Simple linear detrending of a vector. The detrending approach tries to 
#' remove a systematic drift in the data, which is found, for instances, sometimes
#' in climate model simulations. 
#'
#' @param d vector to detrend
#' @return detrended vector
#'
#'


DetrendLinear <- function(d){
    #   
    y <- seq(1,length(d))
    lm.regression <- lm(d ~ y)
    slope <- coefficients(lm.regression)[[2]]
    z <- slope * y 
    z <- z - z[1] # move, so that no adjustment in the first timestep.
    # otherwise first values may be moved to higher values i
    # (slope line z is zero in the middle, neg < middle, pos > middle )
    return(d - z)
}


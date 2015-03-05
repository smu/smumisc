#' Calculate weighted global mean from data.table. Weighting is done by latitudes.
#'  TODO: consider lon values
#'
#' @param d data table containing the column lat (latitude) and value (the variable)
#' @return vector with global mean values.


GlobalMean <- function(d){
    lats <- unique(d$lat)          
    rlats   = lats*pi/180         
    colats <- cos(rlats)     
    weights <- colats / sum(colats)   
    gmean  <- sum(d$value * weights)
    return(c('value'=gmean))
}     

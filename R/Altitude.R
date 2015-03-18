#' Calculate the altitude from pressure level in Pa.
#'  TODO: consider lon values
#'
#' @param p pressure in Pa/mbar
#' @return vector with altitudes in m


Altitude <- function(p){
    # http://en.wikipedia.org/wiki/Atmospheric_pressure#Altitude_atmospheric_pressure_variation
    p0 <- 100000
    R <- 8.31447 # J/(mol•K)
    T0 <- 288.15 # K 
    g <- 9.80665 # m/s2
    M <- 0.0289644 # kg/mol
    alt <- (-1* log(p/p0, base = exp(1)) * R * T0 ) / ( g * M  )
    return(alt)
}


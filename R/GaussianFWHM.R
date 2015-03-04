
#' One dimensional gaussian filter using FWHM definition to cut the weights.
#'
#'   compare: http://en.wikipedia.org/wiki/Full_width_at_half_maximum
#'            http://en.wikipedia.org/wiki/Gaussian_function
#'
#' @param x data vector 
#' @param win window size
#' @param sides left sided (1) or two-sided (2, default) filter. 
#'        Right sided filter is not yet implemented.
#' @return vector with filtered values.

GaussianFWHM <- function(x, win, sides = 2){ 
    if (length(x) < sides*win){
        warning("GaussianFWHM: windowssize to small")
        return (rep(NA, length(x)))
    }   
    # steps
    if (sides == 2) { 
        steps <- seq(-win, win, 1)
        FWHM <- 2*win + 1 
    } else { 
        #  steps <- seq(-win, 0, 1) 
        steps <- seq(0, win, 1)  
        FWHM <- win + 1 
    }   
    .gfcoeffs <- function(s, t) {
        # http://en.wikipedia.org/wiki/Gaussian_filter#Definition
        return ( exp(-(t^2/(2*s^2)))/sqrt(2*pi*s^2) )
    }   
    # calculate sigma based on FWHM (aka, win)
    # see: http://en.wikipedia.org/wiki/Full_width_at_half_maximum
    # http://en.wikipedia.org/wiki/Gaussian_function
    sigma <-  win / sqrt(8*log(2))
    weight <- .gfcoeffs(sigma, steps)
    weight <- weight / sum(weight)
    stats::filter(x, weight, sides = sides)
}


#' One dimensional gaussian filter using FWHM definition to cut the weights.
#'
#'   compare: http://en.wikipedia.org/wiki/Full_width_at_half_maximum
#'            http://en.wikipedia.org/wiki/Gaussian_function
#'
#' @param x data vector 
#' @param win filter window 
#' @param sides left sided (1), right sided (-1), or two-sided (2, default) filter. 
#'        Right sided filter is not yet implemented.
#' @return vector with filtered values.

GaussianFWHM <- function(x, win, sides = 2){ 
    if (length(x) < abs(sides*win)){
        warning("GaussianFWHM: windowssize to small")
        return (rep(NA, length(x)))
    }   
    if (sides %in% c(-1,1,2)) {
        # steps
        if (sides == 2) { # centered
            winhalf <- floor(win/2)
            if ( (win %% 2) == 0 ){
                steps <- seq(-1 * winhalf, winhalf-1, 1 )
            } else {
                steps <- seq(-1 * winhalf, winhalf, 1 )
            }
        } 
        if(sides == 1) { # left
            steps <- seq(0, win-1, 1)  
        }   
        if(sides == -1){ # right
            steps <- seq(win-1, 0, 1)  
        }   
    } else {
        stop(paste(sides, 'not supported.'))
    }

    .gfcoeffs <- function(sigma, x) {
        # http://en.wikipedia.org/wiki/Gaussian_filter#Definition
        return ( exp(-(x^2/(2*sigma^2)))/sqrt(2*pi*sigma) )
    }   
    # calculate sigma based on FWHM (aka, win)
    # see: http://en.wikipedia.org/wiki/Full_width_at_half_maximum
    # http://en.wikipedia.org/wiki/Gaussian_function
    sigma <-  win / sqrt(8*log(2))
    weight <- .gfcoeffs(sigma, steps)
    weight <- weight / sum(weight)
    return(as.numeric(stats::filter(x, weight, sides = sides)))
}

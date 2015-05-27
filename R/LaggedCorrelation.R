
#' Calculate pearson correlation coefficient between two data sets for different lags.
#'
#' @param x vector containing the independent variable
#' @param y vector containing the depended variable
#' @param lags vector of lag
#' @param independent.obs number of independent observations. Defaults to length(x), when x or y is smoothed, the number of independent observations may be lower.
#' @param parallel calculate in parallel, using the foreach package (default True)
#' @return data frame containing the lag applied, the correlation coefficient (r), the p-value (p) and the number of independent observations used to calculate the p-value.


LaggedCorrelation <- function(x, y, lags, independent.obs = length(x), parallel = T){ 
    # A function to correlated two 1D-Variables with different lags.
    # correlation is done in parallel if doMC is loaded and intialized
    # order is important:
    #   x: independent variable
    #   y: depended variable
    #   
    .corr <- function(xlag, x, y){ 
        if (length(y) != length(x)){
            stop("Correlate1D: y and x do not contain the same number of observations")
        }   
        # add lag
        if (xlag < 0){ 
            # y is leading
            x <- c(x, rep(NA, abs(xlag)))
            y <- c(rep(NA, abs(xlag)), y)
        } else {
            # x is leading
            x <- c(rep(NA, xlag), x)
            y <- c(y, rep(NA, xlag))
        }   
        if (length(na.omit(x)) < 5){ 
            warning(paste('Correlate1D: Too few values for correlation with lag', 
                    xlag, '.'))
            r <- NA
            p <- NA
        } else {
            r <- cor(x, y, use = 'p')
            # test with lower degree of freedom:
            p <- MyCorTest(r, independent.obs)
        }   
        return(data.frame(xlag = xlag, r = r, p = p, n = independent.obs))
    }   
    require(foreach, quietly = TRUE)
    if ("package:doMC" %in% search() & parallel){
        foreach(l = lags, .combine = rbind) %dopar% .corr(l, x, y)
    } else {
        foreach(l = lags, .combine = rbind) %do% .corr(l, x, y)
    }   
}

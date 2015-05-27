#'
#' Calculate p-value for a given correlation coefficient an estimate for the number of independent observations.
#'
#' @param r correlation coefficient
#' @param n number of independent observations
#' @param conf.level (optional) test estimated p value against a given confidence level. Returns True or False.
#' @param alternative a character string specifying the alternative hypothesis, must be one of ‘"two.sided"’ (default), ‘"greater"’ or ‘"less"’. So far, only two.sided is implemented.
#' @return p value or True/False, when conf.level is specified.
#'

MyCorTest <- function(r, n, conf.level = NULL, alternative = 'two.sided'){
    t.hat <- abs(r) * sqrt((n-2)/(1-r^2))
    p.value <- 1 - pt(t.hat, df = n-2)
    if (alternative == "two.sided"){
        p.value <- 2 * p.value
    } else {
	stop('not implemented yet.')
    }
    if (!is.null(conf.level)){
        return(p.value <= 1 - conf.level)
    } else {
        return(p.value)
    }
}

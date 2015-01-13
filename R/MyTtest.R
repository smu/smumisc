#' Calculate p value Of Student's t-test for significant differences based on mean value,
#' standard deviation, and number of independend observations.
#'
#' @param m1 mean value of first data set
#' @param s1 standard deviation of first data set
#' @param n1 number of (independed) observations of first data set.
#' @param m2 mean value of second data set
#' @param s2 standard deviation of second data set
#' @param n2 number of (independed) observations of second data set.
#' @return p value.
#' @examples
#'  
#'  d1 = rnorm(100)
#'  d2 = rnorm(100)
#'  
#'  MyTtest(mean(d1), sd(d1), length(d1), mean(d2), sd(d2), length(d2))
#' 


MyTtest <- function(m1, s1, n1, m2, s2, n2){
    # https://en.wikipedia.org/wiki/Student%27s_t-test
    # calculate pooled variance: assumption: s1 equals s2
    sp <- (((n1-1)*(s1**2)) + ((n2-1)*(s2**2))) / (n1 + n2 - 2)
    t.val <- (m1-m2) / sqrt( sp * (1/n1 + 1/n2) )
    # abs: mean(x) < mean(y) not needed
    # 2* for two sided test
    # TODO: make two-side test a parameter.
    p.val <- 2*pt(-abs(t.val), df = n1+n2-2)
    return(p.val)
}


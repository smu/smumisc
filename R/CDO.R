#' System call of the cdo binary.
#'
#' @param ...  one or more arguments of type string, which can be concatenated
#'             using the paste function (e.g. '-yseasmean', '-fldmean').
#' @param sep character used to seperate the terms. Used in the paste function.
#' @param debug print cdo command before execution and call cdo without '-s' argument.
#' @examples
#'      infile <- "~/data/u_50hPa_era40_DJF.grb"
#'      tmpfile <- tempfile()
#'      CDO('-fldmean -selyear,1980/1990',infile,tmpfile,sep=' ')
#'
#' 
#' 

CDO <- function(..., sep = '', debug = FALSE){
    if (debug){
        return(SystemCmd('cdo -f nc ', ..., sep = sep, debug = debug))
    } else {
        return(SystemCmd('cdo -s -f nc ', ..., sep = sep, debug = debug))
    }
}

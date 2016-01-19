#' System call of the cdo binary.
#'
#' @param ...  one or more arguments of type string, which can be concatenated
#'             using the paste function (e.g. '-yseasmean', '-fldmean').
#'             Additionally, parameter for the system function can be passed, e.g.,
#'             \code{intern = TRUE} to capture the output of the cdo command.
#' @param sep character used to seperate the terms. Used in the paste function.
#' @param debug print cdo command before execution and call cdo without '-s' argument.
#' @examples
#'      infile <- "~/data/u_50hPa_era40_DJF.grb"
#'      tmpfile <- tempfile()
#'      CDO('-fldmean -selyear,1980/1990',infile,tmpfile,sep=' ')
#'
#' @section Details:
#'      The behaviour of some cdo commands is control by environment variables.
#'      These need to defined prior to the CDO call using \code{Sys.setenv(...)}, e.g,
#'      \code{Sys.setenv('CDO_WEIGHT_MODE' = 'off')}
#' 

CDO <- function(..., sep = '', debug = FALSE){
    if (debug){
        return(SystemCmd('cdo -f nc ', ..., sep = sep, debug = debug))
    } else {
        return(SystemCmd('cdo -s -f nc ', ..., sep = sep, debug = debug))
    }
}

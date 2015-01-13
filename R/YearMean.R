#' Calculate annual means from data.table
#'
#' @param data data table containing the columns date (Date object) and value (the variable)
#' @param varname name of the variable, defaults to 'value''
#' @param na.rm a logical value indicating whether 'NA' values should be 
#'        stripped before the computation proceeds. Defaults to TRUE
#' @return data table with seasonal averaged values.
#' @examples
#'  mydata <- data.frame(date = seq.Date(as.Date('1990-01-01'),
#'                                   as.Date('2000-12-31'),
#'                                   by = '1 months'),
#'                                   value = rnorm(11*12))
#' YearMean(mydata,)


YearMean <- function(data, varname = 'value', na.rm = TRUE) {
    d.ym <- stack(tapply(data[, varname], cut.Date(data$date, 'years'), mean, na.rm = na.rm))
    names(d.ym) <- c(varname, 'date')
    d.ym$date <- as.Date(as.character(d.ym$date))
    return(d.ym)
}

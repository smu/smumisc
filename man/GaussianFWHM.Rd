% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GaussianFWHM.R
\name{GaussianFWHM}
\alias{GaussianFWHM}
\title{One dimensional gaussian filter using FWHM definition to cut the weights.}
\usage{
GaussianFWHM(x, win, sides = 2)
}
\arguments{
\item{x}{data vector}

\item{win}{filter window}

\item{sides}{left sided (1), right sided (-1), or two-sided (2, default) filter. 
Right sided filter is not yet implemented.}
}
\value{
vector with filtered values.
}
\description{
compare: http://en.wikipedia.org/wiki/Full_width_at_half_maximum
           http://en.wikipedia.org/wiki/Gaussian_function
}

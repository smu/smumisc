
library(smumisc)
library(plyr)
context('GlobalMean')


# compare results against CDO('-fldmean')
fname = system.file("extdata", "example_tll.nc", package = "smumisc")
test_that('Example netcdf file exists', {
          expect_that(fname != '', is_true())
})


# since longitudes are not considered in GlobalMean (so far), we need to calculate the zonal mean first
tmpfile1 <- tempfile(pattern='1')
CDO('-zonmean ',fname,' ',tmpfile1)

# cdo result
tmpfile2 <- tempfile(pattern='2')
CDO('-fldmean ',tmpfile1,' ',tmpfile2)
d_cdo <- GetVarNCDF(tmpfile2, 'slp', 'time')


# R result
d <- GetVarNCDFTL(tmpfile1, 'slp')
#  d_ <- na.omit(YearMean(d))
d_R <- ddply(d, .(date), GlobalMean)



test_that('YearMean == "cdo -fldmean"', {
          expect_equal(d_R$value[1], d_cdo$value[1], tolerance = 1e-6)
          expect_equal(d_R$value[nrow(d_R)], d_cdo$value[nrow(d_cdo)], tolerance = 1e-6)
})



unlink(tmpfile1)
unlink(tmpfile2)

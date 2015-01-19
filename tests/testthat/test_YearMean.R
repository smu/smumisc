library(smumisc)
context('YearMeans')

mydata <- data.frame(date = seq.Date(as.Date('1990-01-01'),
                                   as.Date('2000-12-31'),
                                   by = '1 years'),
                                   value = rnorm(11))


test_that('Annual mean of annual data equals annual data.', {
        expect_that(all(mydata$value == YearMean(mydata)$value), is_true())
          })


# compare results against CDO('-seasmean')
fname = system.file("extdata", "example_tll.nc", package = "smumisc")
test_that('Example netcdf file exists', {
          expect_that(fname != '', is_true())
})

tmpfile <- tempfile(pattern='1')
CDO('-yearmean -fldmean ',fname,' ',tmpfile)
tmpfile2 <- tempfile(pattern='2')
CDO('-fldmean ',fname,' ',tmpfile2)

d_ym_cdo <- GetVarNCDF(tmpfile, 'slp', 'time')
d <- GetVarNCDF(tmpfile2, 'slp', 'time')
d_ym <- na.omit(YearMean(d))

test_that('YearMean == "cdo -yearmean"', {
          expect_equal(d_ym$value[1], d_ym_cdo$value[1], tolerance = 1e-7)
          expect_equal(d_ym$value[nrow(d_ym)], d_ym_cdo$value[nrow(d_ym_cdo)], tolerance = 1e-7)
})



unlink(tmpfile)

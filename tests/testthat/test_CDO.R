library(smumisc)
context('CDO')


fname = system.file("extdata", "example_tll.nc", package = "smumisc")
test_that('Example netcdf file exists', {
          expect_that(fname != '', is_true())
})

test_that('cdo is available', {
          expect_that(SystemCmd('which cdo', ignore.stdout=TRUE) == 0, is_true())
          expect_that(SystemCmd('which cdo', intern=TRUE) != '', is_true())
})

tmpfile <- tempfile()
test_that('tmpfile should not be present', {
          expect_that(file.exists(tmpfile), is_false())
})

test_that('CDO command is executed', {
          expect_equal(CDO('-fldmean -yearmean ',fname,' ',tmpfile), 0)
})

test_that('CDO commands output exists', {
          expect_that(file.exists(tmpfile), is_true())
})

unlink(tmpfile)

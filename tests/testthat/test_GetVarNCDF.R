library(smumisc)
context('GetVarNCDF')

# TODO: test ConvertTime


fname = system.file("extdata", "example_tll.nc", package = "smumisc")
test_that('Example netcdf file exists', {
          expect_that(fname != '', is_true())
})

ymfldmeanfile <- tempfile(pattern='1')
CDO('-yearmean -fldmean ',fname,' ', ymfldmeanfile)
timemeanfile <- tempfile(pattern='2')
CDO('-timmean ',fname,' ', timemeanfile)
allmeanfile <- tempfile(pattern='3')
CDO('-timmean -fldmean ',fname,' ', allmeanfile)

dorig <- GetVarNCDF(fname, 'temp2', c('lat','lon','time'))
dymfld <- GetVarNCDF(ymfldmeanfile, 'temp2', c('time'))
dtimemean <- GetVarNCDF(timemeanfile, 'temp2', c('lat','lon'))
dallmean <- GetVarNCDF(allmeanfile, 'temp2', NULL )


test_that('Variable names are correct', {
          expect_that(all(names(dorig) == c('lon','lat','date','value')), is_true())
          expect_that(all(names(dymfld) == c('date','value')), is_true())
          expect_that(all(names(dtimemean) == c('lon','lat','value')), is_true())
})

# test failures
test_that('Test wrong dimensions with name', {
    expect_error(GetVarNCDF(fname, 'temp2', c('lat','lon','date')),
            "Error in GetVarNCDF")
})

# will produce some output, therefore deactivated for the moment.
#  test_that('Test wrong variable name', {
    #  expect_error(GetVarNCDF(fname, 'temp23', c('lat','lon','time')),
            #  "Variable not found")
#  })

# ordering of dimensions should not affect the result?
test_that('Dimensions order does not matter.',{
          expect_that(all(
            GetVarNCDF(fname, 'temp2', c('time','lon','lat')) == 
            GetVarNCDF(fname, 'temp2', c('lat', 'lon','time')) ), is_true())
})




# test with different example netcdf files from
# http://www.unidata.ucar.edu/software/netcdf/examples/files.html
require(RCurl)

# From the Community Climate System Model (CCSM), one time step of precipitation
# flux, air temperature, and eastward wind. 
URL <- "https://www.unidata.ucar.edu/software/netcdf/examples/sresa1b_ncar_ccsm3-example.nc"
curl <- getCurlHandle()
bfile <- getBinaryURL(URL, curl = curl,noprogress = TRUE)
writeBin(bfile, "example1.nc")

# Sea surface temperatures collected by PCMDI for use by the IPCC. 
URL <- "http://www.unidata.ucar.edu/software/netcdf/examples/tos_O1_2001-2002.nc"
bfile <- getBinaryURL(URL, curl = curl,noprogress = TRUE)
writeBin(bfile, "example2.nc")
rm(curl, bfile)

ex2tmp <- tempfile(pattern='ex2')

dex1_tas <- GetVarNCDF('example1.nc','tas', c('lat','lon'))
dex1_ua  <- GetVarNCDF('example1.nc','ua', c('lat','lon','plev'))
dex2_tos <- GetVarNCDF('example2.nc','tos', c('time','lat','lon'))

#  print(names(dex2_tos))
test_that('Test dimensions in netcdf example files', {
          expect_that(all(names(dex1_tas) == c('lon','lat','value')), is_true())
          expect_that(all(names(dex1_ua)  == c('lon','lat','plev','value')), is_true())
          expect_that(all(names(dex2_tos) == c('lon','lat','date','value')), is_true())
})





# take example file, calculate annual mean and compare against cdo result
library(plyr)
# subset, otherwise ddply call is very slow...
dex2_tos <- subset(dex2_tos, lat == 0.5 & lon == 1)
ym <- ddply(dex2_tos, .(lat,lon), YearMean)
CDO('-yearmean ','example2.nc',' ',ex2tmp)
ym.cdo <- GetVarNCDF(ex2tmp, 'tos',c('time','lat','lon'))
ym.cdo <- subset(ym.cdo, lat == 0.5 & lon == 1)


test_that('Combination of GetVarNCDF and Yearmean equals cdo results', {
          expect_equal(ym$value[1], ym.cdo$value[1], tolerance = 1e-7)
          expect_equal(ym$value[nrow(ym)], ym.cdo$value[nrow(ym.cdo)], tolerance = 1e-7)
})






# test shortcut functions
test_that('GetVarNCDFT works as expected',{
          expect_that(all(dymfld == GetVarNCDFT(ymfldmeanfile, 'temp2')), is_true())
          #  expect_that(all(dtimemean == GetVarNCDFLL(timemeanfile, 'temp2')), is_true())
})

d <- GetVarNCDFTLL('example2.nc', 'tos')
d <- subset(d, lat == 0.5 & lon == 1)

test_that('GetVarNCDFTLL works as expected',{
          expect_that(all(dorig == GetVarNCDFTLL(fname, 'temp2')), is_true())
          expect_that(all(dex2_tos == d), is_true())
})

# GetVarNCDFLL with lat|lev dimension
tmp <- tempfile(pattern='ex1a')
CDO('-zonmean example1.nc ',tmp)
d2 <- GetVarNCDFLL(tmp, 'ua')
unlink(tmp)

test_that('GetVarNCDFLL works as expected',{
          expect_that(all(names(d2) == c('lat','plev','value')), is_true())
})

# GetVarNCDFLLL with lon|lat|lev dimension
test_that('GetVarNCDFLL works as expected',{
          expect_that(all(names(GetVarNCDFLLL('example1.nc', 'ua')) == c('lon','lat','plev','value')), is_true())
})



# clean up 
unlink(ymfldmeanfile)
unlink(timemeanfile)
unlink(ex2tmp)
unlink('example1.nc')
unlink('example2.nc')



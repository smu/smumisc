library(smumisc)
context('SeasonalMean')

mydata <- data.frame(date = seq.Date(as.Date('1990-01-01'),
                                   as.Date('2000-12-31'),
                                   by = '1 months'),
                     value = rnorm(11*12))


warn = getOption("warn") 
options( warn = -1 )
djf <- SeasonalMean(mydata, 'DJF')
jja <- SeasonalMean(mydata, 'JJA')
djfmam <- SeasonalMean(mydata, 'DJFMAM')


options( warn = 0 )
test_that('Should produce a warning due to missing months', {
          expect_warning(SeasonalMean(mydata, 'DJF'))
})


#  test_that('Should produce a warning due to missing months', {
          #  expect_warning(SeasonalMean(mydata, 'JJA'))
#  })

test_that('Returned data type is data.frame', {
          expect_is(djf, 'data.frame')
          expect_is(djfmam, 'data.frame')
})


test_that('Error with invalid seasons', {
          expect_error(SeasonalMean(mydata, 'DJFA'))
          expect_error(SeasonalMean(mydata, 'ACDC'))
})


CalcSeason <- function(data, indices) {
    # simplified seasonal averages (works only when years are complete, thats 
    # why the more sophisticated SeasonalMean is needed)
    tmp <- mydata[indices, ]
    nyears <- length(unique(as.POSIXlt(data$date)$year))
    tmp.indices <- rep(indices, nyears)
    i <- rle(tmp.indices)
    pos <- 1
    avg <- NULL
    for(j in seq_along(i$values)) {
        if(i$values[j]){
            avg <- rbind(avg, mean(data[pos:(pos+i$length[j]-1),]$value, na.rm = T))
        }
        pos <- pos + i$length[j]
    }
    return(avg)
}

#  calculate DJF avg by hand
djf.indices <- c(TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,TRUE)
jja.indices <- c(FALSE,FALSE,FALSE,FALSE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE)

test_that('Verify computations', {
          expect_that(all(djf$value == CalcSeason(mydata, djf.indices)), is_true())
          expect_that(all(jja$value == CalcSeason(mydata, jja.indices)), is_true())
})



# one-month-seasons  (e.g., 'D') should be the same as the monthly values
dec.values <- mydata[as.POSIXlt(mydata$date)$mon == 11, ]$value
jan.values <- mydata[as.POSIXlt(mydata$date)$mon == 0, ]$value
test_that('one month seasons should be the same as the monthly values', {
        expect_that(all(dec.values == SeasonalMean(mydata, 'D')$value), is_true())
        expect_that(all(jan.values == SeasonalMean(mydata, 'J')$value), is_true())
})


# TODO compare results against CDO('-seasmean')


# test different variables names
mydata2 <- mydata
names(mydata2)[2] <- 'newvar'

test_that('same result for different variable names', {
          expect_that(all(SeasonalMean(mydata, 'DJF')$value ==
                          SeasonalMean(mydata2, 'DJF', variable = 'newvar')$value),
                      is_true())


})



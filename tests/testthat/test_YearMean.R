library(smumisc)
context('YearMeans')

mydata <- data.frame(date = seq.Date(as.Date('1990-01-01'),
                                   as.Date('2000-12-31'),
                                   by = '1 years'),
                                   value = rnorm(11))


test_that('Annual mean of annual data equals annual data.', {
        expect_that(all(mydata$value == YearMean(mydata)$value), is_true())
          })

# TODO compare results against CDO('-seasmean')

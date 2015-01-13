library(smumisc)
context('MyTtest')

# Compare against results of t.test



d1 = rnorm(100)
d2 = rnorm(100)


r.test <- t.test(d1,d2)
my.pvalue <- MyTtest(mean(d1), sd(d1), length(d1), mean(d2), sd(d2), length(d2))


# some differences to t.test() exists, therefore tolerance is 
# a bit higher.
test_that('MyTtest equals results of t.test function', {
          expect_equal(r.test$p.value, my.pvalue, tolerance = 1e-4)
})

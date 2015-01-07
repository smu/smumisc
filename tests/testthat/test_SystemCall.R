
library(smumisc)
context('SystemCmd')




test_that('Successful system calls (might fail on non-linux systems)', {
          expect_equal(SystemCmd('ls /', ignore.stdout = TRUE), 0)
          expect_equal(SystemCmd('sleep 0.01', ignore.stdout = TRUE), 0)
})

test_that('Unsuccessful system calls)', {
          expect_equal(SystemCmd('ls /sadfad', ignore.stdout = TRUE,
                                 ignore.stderr = TRUE), 2)
})

test_that('Test output of command', {
          expect_match(SystemCmd('echo test', intern = TRUE), 'test')
          expect_match(SystemCmd('echo ', 'test', intern = TRUE), 'test')
          expect_match(SystemCmd('echo', 'test', sep = ' ', intern = TRUE), 'test')
})

context("Checking if errors an error when P is not a matrix")
test_that("Error when P is not matrix", 
          expect_error(FitStats(y = seq(1, 100), 
                             P = seq(1, 100),
                             r = seq(3, 200, length.out = 100)))
)
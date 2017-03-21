context("Checking object class")
test_that("Correct object is produced", 
          expect_is(FitStats(y = seq(1, 100), 
                             P = matrix(c(seq(1, 100), seq(2, 101), seq(2, 1010, length.out = 100)), ncol = 3),
                             r = seq(3, 200, length.out = 100)),  "matrix")
          )
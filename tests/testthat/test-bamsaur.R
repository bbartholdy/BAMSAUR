# test for bamsaur functions

# add test for ability to deal with missing values
library(BAMSAUR)

wear <- runif(10, min = 0, max = 16)
wear_out <- bamsaur(wear)
random_pop <- MBsimple[sample(1:nrow(MBsimple), size = 25, replace = F),]
wear_out2 <- bamsaur(wear, ref_pop = random_pop, degree = 1, level = 0.9)

valid_out <- validataur(Age ~ Wear, MBsimple)

testthat::test_that("Output equal input length", {
  testthat::expect_type(wear_out, "bamsaur")
  testthat::expect_equal(length(wear_out$estimate[,1]), length(wear))
})

testthat::test_that("Warning message for bamsaur()", {
  testthat::expect_warning(wear_out, c("*One or more of the estimated*"))
})

testthat::test_that("AIC and BIC", {
  testthat::expect_type(valid_out$models, "list")
  testthat::expect_length(valid_out$aic, 3)
})

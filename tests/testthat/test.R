library(pbapply)
context("pbapply")

x <- sapply(1:10, function(x) runif(10))
y <- lapply(1:10, function(x) runif(10))

test_that("Outputs are identical", {
  expect_identical(pbapply(x, 1, mean), apply(x, 1, mean))
  expect_identical(pbapply(x, 2, mean), apply(x, 2, mean))
  expect_identical(pbsapply(y, mean), sapply(y, mean))
  expect_identical(pblapply(y, mean), lapply(y, mean))
})

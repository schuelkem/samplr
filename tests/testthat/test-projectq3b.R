context("test-projectq3b")
library(samplr)

test_that("projectq3b returns a numeric vector of correct length", {
  u <- projectq3b(n = 3, pdf = dunif, a = 0, b = 1)

  expect_is(object = u, class = "numeric")
  expect_true(object = is.vector(u))
  expect_true(object = length(u) == 3)
})

test_that("projectq3b passes additional parameters to the pdf", {
  b <- projectq3b(
    n = 10000,
    pdf = dbeta,
    a = 0,
    b = 1,
    shape1 = 2,
    shape2 = 2
  )

  expect_true(object = 0 <= b && b <= 1)
})

test_that("projectq3b returns same as last time", {
  set.seed(42)

  b <- projectq3b(
    n = 10,
    pdf = dbeta,
    a = 0,
    b = 1,
    shape1 = 2,
    shape2 = 5
  )

  expect_equal_to_reference(object = b,
                            file = "test-projectq3b.ref")
})

context("test-projectq3c")
library(samplr)

test_that("projectq3c returns a data.frame", {
  u <- projectq3c(
    n = 1,
    jpdf = jdunif
  )

  expect_is(object = u, class = "data.frame")
})

test_that("projectq3c returns correct dimensions", {
  # n = 1
  u1 <- projectq3c(
    n = 1,
    jpdf = jdunif
  )

  expect_true(object = nrow(u1) == 1)
  expect_true(object = ncol(u1) == 2)

  # n = 11
  u11 <- projectq3c(
    n = 11,
    jpdf = jdunif
  )
  expect_true(object = nrow(u11) == 11)
  expect_true(object = ncol(u11) == 2)

  # n = 111
  u111 <- projectq3c(
    n = 111,
    jpdf = jdunif
  )

  expect_true(object = nrow(u111) == 111)
  expect_true(object = ncol(u111) == 2)
})

test_that("projectq3c passes additional parameters to the jpdf", {
  # uniform
  u <- projectq3c(
    n = 1,
    jpdf = jdunif,
    min = 0,
    max = 10
  )

  d <- jdunif(x = u[1, 1],
              y = u[1, 2],
              min = 0,
              max = 10)

  expect_true(object = d == 1 / (10 - 0)^2)

  # beta
  b <- projectq3c(
    n = 1,
    jpdf = jdbeta,
    shape1 = 2,
    shape2 = 2
  )

  d <- jdbeta(x = b[1, 1],
              y = b[1, 2],
              shape1 = 2,
              shape2 = 2)

  expect_true(object = 0 <= d && d <= 1.5)
})

test_that("projectq3c returns same as last time", {
  set.seed(42)

  b <- projectq3c(
    n = 10,
    jpdf = jdbeta,
    shape1 = 2,
    shape2 = 2
  )

  expect_equal_to_reference(object = b,
                            file = "test-projectq3c.ref")
})

test_that("jdnorm returns numeric vector of correct length", {
  d <- jdnorm(x = rnorm(n = 3), y = rnorm(n = 3))

  expect_is(object = d, class = "numeric")
  expect_true(object = is.vector(d))
  expect_true(object = length(d) == 3)
})

test_that("jdnorm passes additional parameters to the pdf", {
  d <- jdnorm(x = 0, y = 0, mean = c(1, 1))

  expect_true(object = round(d, 4) == 0.0585)
})

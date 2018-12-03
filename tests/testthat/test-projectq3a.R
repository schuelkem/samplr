context("test-projectq3a")
library(samplr)

test_that("projectq3a returns a data.frame", {
  u <- projectq3a(
    n = 1,
    jpdf = jdunif,
    a = 0,
    b = 1,
    C = 1
  )

  expect_is(object = u, class = "data.frame")
})

test_that("projectq3a returns correct dimensions", {
  # n = 1
  u1 <- projectq3a(
    n = 1,
    jpdf = jdunif,
    a = 0,
    b = 1,
    C = 1
  )

  expect_true(object = nrow(u1) == 1)
  expect_true(object = ncol(u1) == 2)

  # n = 11
  u11 <- projectq3a(
    n = 11,
    jpdf = jdunif,
    a = 0,
    b = 1,
    C = 1
  )
  expect_true(object = nrow(u11) == 11)
  expect_true(object = ncol(u11) == 2)

  # n = 111
  u111 <- projectq3a(
    n = 111,
    jpdf = jdunif,
    a = 0,
    b = 1,
    C = 1
  )

  expect_true(object = nrow(u111) == 111)
  expect_true(object = ncol(u111) == 2)
})

test_that("projectq3a passes additional parameters to the jpdf", {
  # uniform
  u <- projectq3a(
    n = 1,
    jpdf = jdunif,
    a = 0,
    b = 10,
    C = 1 / 10,
    min = 0,
    max = 10
  )

  d <- jdunif(x = u[1, 1],
               y = u[1, 2],
               min = 0,
               max = 10)

  expect_true(object = d == 1 / (10 - 0)^2)

  # beta
  b <- projectq3a(
    n = 1,
    jpdf = jdbeta,
    a = 0,
    b = 1,
    C = 1.5,
    shape1 = 2,
    shape2 = 2
  )

  d <- jdbeta(x = b[1, 1],
              y = b[1, 2],
              shape1 = 2,
              shape2 = 2)

  expect_true(object = 0 <= d && d <= 1.5)
})

test_that("projectq3a returns same as last time", {
  set.seed(8675309)

  b <- projectq3a(
    n = 10,
    jpdf = jdbeta,
    a = 0,
    b = 1,
    C = 1.5,
    shape1 = 2,
    shape2 = 2
  )

  expect_equal_to_reference(object = b,
                            file = "test-projectq3a.ref")
})

test_that("jdunif returns numeric vector of correct length", {
  d <- jdunif(x = runif(3), y = runif(3))

  expect_is(object = d, class = "numeric")
  expect_true(object = is.vector(d))
  expect_true(object = length(d) == 3)
})

test_that("jdunif passes additional parameters to the pdf", {
  d <- jdunif(x = 5, y = 5, min = 0, max = 10)

  expect_true(object = d == 0.01)
})

test_that("jdbeta returns numeric vector of correct length", {
  d <- jdbeta(x = runif(3), y = runif(3), shape1 = 2, shape2 = 2)

  expect_is(object = d, class = "numeric")
  expect_true(object = is.vector(d))
  expect_true(object = length(d) == 3)
})

test_that("jdbeta passes additional parameters to the pdf", {
  d <- jdbeta(x = runif(1), y = runif(1), shape1 = 2, shape2 = 5)

  expect_true(object = 0 <= d && d <= 2.5)
})

test_that("jdcirclecontour returns numeric vector of correct length", {
  d <- jdcirclecontour(x = runif(n = 3, min = -1, max = 1),
                       y = runif(n = 3, min = -1, max = 1))

  expect_is(object = d, class = "numeric")
  expect_true(object = is.vector(d))
  expect_true(object = length(d) == 3)
})

context("test-projectq3a")
library(samplr)

test_that("projectq3a returns a data.frame", {
  u <- projectq3a(
    n = 1,
    jpdf = d2dunif,
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
    jpdf = d2dunif,
    a = 0,
    b = 1,
    C = 1
  )

  expect_true(object = nrow(u1) == 1)
  expect_true(object = ncol(u1) == 2)

  # n = 11
  u11 <- projectq3a(
    n = 11,
    jpdf = d2dunif,
    a = 0,
    b = 1,
    C = 1
  )
  expect_true(object = nrow(u11) == 11)
  expect_true(object = ncol(u11) == 2)

  # n = 111
  u111 <- projectq3a(
    n = 111,
    jpdf = d2dunif,
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
    jpdf = d2dunif,
    a = 0,
    b = 10,
    C = 1 / 10,
    min = 0,
    max = 10
  )

  d <- d2dunif(x = u[1, 1],
               y = u[1, 2],
               min = 0,
               max = 10)

  expect_true(object = d == 1 / (10 - 0)^2)

  # beta
  b <- projectq3a(
    n = 1,
    jpdf = d2dbeta,
    a = 0,
    b = 1,
    C = 1.5,
    shape1 = 2,
    shape2 = 2
  )

  d <- dbeta(x = b[1, 1],
             y = b[1, 2],
             shape1 = 2,
             shape2 = 2)

  expect_true(object = 0 <= d && d <= 1.5)
})

test_that("projectq3a returns same as last time", {
  set.seed(8675309)

  b <- projectq3a(
    n = 10,
    jpdf = d2dbeta,
    a = 0,
    b = 1,
    C = 1.5,
    shape1 = 2,
    shape2 = 2
  )

  expect_equal_to_reference(object = b,
                            file = "test-projectq3a.ref")
})

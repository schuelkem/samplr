context("test-projectq2a")
library(samplr)

test_that("projectq2a returns a numeric vector", {
  u <- projectq2a(
    n = 1,
    pdf = dunif,
    a = 0,
    b = 1,
    C = 1
  )

  expect_is(object = u, class = "numeric")

  expect_true(object = is.vector(u))
})

test_that("projectq2a returns correct length", {
  # n = 1
  u1 <- projectq2a(
    n = 1,
    pdf = dunif,
    a = 0,
    b = 1,
    C = 1
  )

  expect_true(object = length(u1) == 1)

  # n = 11
  u11 <- projectq2a(
    n = 11,
    pdf = dunif,
    a = 0,
    b = 1,
    C = 1
  )
  expect_true(object = length(u11) == 11)

  # n = 111
  u111 <- projectq2a(
    n = 111,
    pdf = dunif,
    a = 0,
    b = 1,
    C = 1
  )

  expect_true(object = length(u111) == 111)
})

test_that("projectq2a passes additional parameters to the pdf", {
  # uniform
  u <- projectq2a(
    n = 1,
    pdf = dunif,
    a = 0,
    b = 10,
    C = 1 / 10,
    min = 0,
    max = 10
  )

  d <- dunif(x = u,
             min = 0,
             max = 10)

  expect_true(object = d == 1 / 10)

  # beta
  b <- projectq2a(
    n = 1,
    pdf = dbeta,
    a = 0,
    b = 1,
    C = 1.5,
    shape1 = 2,
    shape2 = 2
  )

  d <- dbeta(x = b,
             shape1 = 2,
             shape2 = 2)

  expect_true(object = 0 <= d && d <= 1.5)
})

test_that("projectq2a returns same as last time", {
  set.seed(42)

  b <- projectq2a(
    n = 10,
    pdf = dbeta,
    a = 0,
    b = 1,
    C = 1.5,
    shape1 = 2,
    shape2 = 2
  )

  expect_equal_to_reference(object = b,
                            file = "test-projectq2a.ref")
})

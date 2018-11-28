#' projectq3a
#'
#' @description This function generates pairs of random deviates from a 2D continuous distribution defined on a square with the supplied joint probability density function via rejection sampling.
#'
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param jpdf a function that is the joint pdf of the distribution. Should use x and y as quantile arguments.
#' @param a,b lower and upper limits of the sides of the distribution. Must be finite such that \eqn{P(a \le X \le b \cap a \le Y \le b) = 1}.
#' @param C a numeric such that \eqn{f(x,y) \le C} for all values of x and y.
#' @param ... further arguments passed to or from other methods.
#'
#' @return data.frame of random deviate pairs
#' @export
#'
#' @examples
#' plot(projectq3a(n = 10000, jpdf = jdunif, a = 0, b = 1, C = 1))
#' plot(projectq3a(n = 10000, jpdf = jdunif, a = 0, b = 2, C = 1/4, min = 0, max = 2))
#' plot(projectq3a(n = 10000, jpdf = jdcirclecontour, a = -1, b = 1, C = 1))
projectq3a <- function(n, jpdf, a, b, C, ...) {
  assertive::assert_is_numeric(n)
  if(length(n) > 1)
    n <- length(n)

  assertive::assert_is_function(jpdf)

  assertive::assert_is_numeric(a)
  if(length(a) > 1)
    a <- min(a)

  assertive::assert_is_numeric(b)
  if(length(b) > 1)
    b <- max(b)

  assertive::assert_is_numeric(C)
  if(length(C) > 1)
    C <- max(C)

  random.samples <- data.frame(x = numeric(0),
                               y = numeric(0))
  for(i in 1:n) {
    rejecting <- TRUE
    while(rejecting) {
      quantile.candidates <- stats::runif(n = 2, min = a, max = b)
      quantile.candidates.density <- do.call(jpdf, list(x = quantile.candidates[1], y = quantile.candidates[2], ...))
      rejection.sampling.critical.value <- stats::runif(n = 1, min = 0, max = C)
      rejecting <- quantile.candidates.density < rejection.sampling.critical.value
    }
    random.samples <- rbind(random.samples,
                            data.frame(x = quantile.candidates[1],
                                       y = quantile.candidates[2]))
  }
  random.samples
}

#' jdunif
#'
#' @param x,y vector of quantiles
#' @param min,max lower and upper limits of the distribution for each side. Must be finite.
#'
#' @return numeric vector of joint densities
#' @export
#'
#' @examples
#' jdunif(x = 0.5, y = 0.5)
#' jdunif(x = 5, y = 5, min = 0, max = 10)
#' jdunif(x = runif(3), y = runif(3))
jdunif <- function(x, y, min = 0, max = 1) {
  f <- Vectorize(function(x, y, min, max) {
    if(min <= x && x <= max && min <= y && y <= max)
      (max - min)^(-2)
    else
      0
  })
  f(x = x, y = y, min = min, max = max)
}

#' jdbeta
#'
#' @param x,y vector of quantiles
#' @param shape1,shape2 non-negative parameters of the Beta distribution.
#'
#' @return numeric vector of joint densities
#' @export
#'
#' @examples
#' jdbeta(x = 0.5, y = 0.5, shape1 = 2, shape2 = 2)
#' jdbeta(x = 0.2, y = 0.2, shape1 = 2, shape2 = 5)
#' jdbeta(x = runif(3), y = runif(3), shape1 = 2, shape2 = 2)
jdbeta <- function(x, y, shape1, shape2) {
  f <- Vectorize(function(x, y, shape1, shape2) {
    if(0 <= x && x <= 1 && 0 <= y && y <= 1)
      (stats::dbeta(x = x, shape1 = shape1, shape2 = shape2) +
         stats::dbeta(x = y, shape1 = shape1, shape2 = shape2)) / 2
    else
      0
  })
  f(x = x, y = y, shape1 = shape1, shape2 = shape2)
}

#' jdcirclecontour
#'
#' @param x,y vector of quantiles
#'
#' @return numeric vector of joint densities
#' @export
#'
#' @examples
#' jdcirclecontour(x = 0, y = 0)
#' jdcirclecontour(x = 1, y = -1)
#' jdcirclecontour(x = runif(n = 3, min = -1, max = 1),
#'                 y = runif(n = 3, min = -1, max = 1))
jdcirclecontour <- function(x, y) {
  f <- Vectorize(function(x, y) {
    if(-1 <= x && x <= 1 && -1 <= y && y <= 1)
      (3/8)*(x^2 + y^2)
    else
      0
  })
  f(x = x, y = y)
}

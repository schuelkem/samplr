#' Homework 7 Question 2 Distribution
#'
#' @description These functions provide information about the Homework 7
#' Question 2 distribution. dhw7q2 gives the density, phw7q2 gives the
#' distribution function, qhw7q2 gives the quantile function, and rhw7q2
#' generates random deviates.
#'
#' @param x,q vector of quantiles
#' @param p vector of probabilites
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required
#' @param log,log.p logical; if TRUE, probabilities p are given as log(p)
#' @param lower.tail logical; if TRUE (default), probabilites are \eqn{P[X \le x]} otherwise, \eqn{P[X > x]}
#'
#' @details This distribution has density \deqn{f(x) = 6x(1-x)} for 0 < x < 1
#'
#' @return dhw7q2 gives the density, phw7q2 gives the distribution function, qhw7q2 gives the quantile function, and rhw7q2 generates random deviates.
#' The length of the result is determined by n for rnorm, and is the maximum of the lengths of the numerical arguments for the other functions.
#' The numerical arguments other than n are recycled to the length of the result. Only the first elements of the logical arguments are used.
#'
#' @note The characteristics of output from pseudo-random number generators
#' (such as precision and periodicity) vary widely. See .Random.seed for more
#' information on R's random number generation algorithms.
#'
#' @seealso \code{\link[stats]{Uniform}}
#'
#' @export
#'
#' @examples
#' ## density function
#' plot(dhw7q2(x = seq(0, 1, 0.0001)))
#'
#' ## distribution function
#' plot(phw7q2(q = seq(0, 1, 0.0001)))
#'
#' ## quantile function
#' qhw7q2(p = 0)
#' qhw7q2(p = 0, lower.tail = FALSE)
#' qhw7q2(p = seq(0, 1, 0.25))
#' plot(qhw7q2(p = seq(0, 1, 0.001)))
#'
#' ## random deviates
#' rhw7q2(n = 10)
#'
#' @name hw7q2
NULL
#> NULL

#' @rdname hw7q2
dhw7q2 <- function(x, log = FALSE) {
  d <- sapply(X = x, FUN = function(x) ifelse(0 < x & x < 1, 6 * x * (1 - x), 0))
  sapply(X = d, FUN = function(x) ifelse(log, log(x), x))
}

#' @rdname hw7q2
phw7q2 <- function(q, lower.tail = TRUE, log.p = FALSE) {
  p <- sapply(X = q, FUN = function(x) ifelse(lower.tail, 3 * x^2 - 2 * x^3, 1 - (3 * x^2 - 2 * x^3)))
  sapply(X = p, FUN = function(x) ifelse(log.p, log(x), x))
}

#' @rdname hw7q2
qhw7q2 <- function(p, lower.tail = TRUE, log.p = FALSE) {
  assertive::assert_all_are_in_closed_range(x = p,
                                            lower = 0,
                                            upper = 1)

  p <- sapply(X = p, FUN = function(x) ifelse(log.p, exp(x), x))
  p <- sapply(X = p, FUN = function(x) ifelse(lower.tail, x, 1 - x))

  inv_cdf_fun <- function(x, p) 3 * x^2 - 2 * x^3 - p
  inv_cdf <- function(p) uniroot(f = inv_cdf_fun,
                                 interval = c(0, 1),
                                 p = p)[["root"]]

  sapply(X = p , FUN = inv_cdf)
}

#' @rdname hw7q2
rhw7q2 <- function(n) {
  qhw7q2(p = runif(n = n))
}

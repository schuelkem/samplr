#' Homework 7 Question 2 Distribution
#'
#' @description These functions provide information about the Homework 7
#' Question 2 distribution. dhw7q2 gives the density, phw7q2 gives the
#' distribution function, qhw7q2 gives the quantile function, and rhw7q2
#' generates random deviates.
#'
#' @usage dhw7q2(x, log = FALSE)
#' phw7q2(q, lower.tail = TRUE, log.p = FALSE)
#' qhw7q2(p, lower.tail = TRUE, log.p = FALSE)
#' rhw7q2(n)
#'
#' @param [x, q] vector of quantiles
#' @param p vector of probabilites
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required
#' @param [log, log.p] logical; if TRUE, probabilities p are given as log(p)
#' @param lower.tail logical; if TRUE (default), probabilites are $P[X \le x]$ otherwise, $P[X > x]$
#'
#' @details This distribution has density $$f(x) = 6x(1-x)$$ for 0 < x < 1
#'
#' @return dnorm gives the density, pnorm gives the distribution function, qnorm gives the quantile function, and rnorm generates random deviates.
#' The length of the result is determined by n for rnorm, and is the maximum of the lengths of the numerical arguments for the other functions.
#' The numerical arguments other than n are recycled to the length of the result. Only the first elements of the logical arguments are used.
#'
#' @note The characteristics of output from pseudo-random number generators
#' (such as precision and periodicity) vary widely. See .Random.seed for more
#' information on R's random number generation algorithms.
#'
#' @export
#'
#' @examples
#' ## density function
#' plot(dhw7q2(seq(0, 1, 0.0001)))
#'
#' ## distribution function
#' plot(phw7q2(seq(0, 1, 0.0001)))
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
  d <- if(0 < x & x < 1) 6 * x * (1 - x) else 0
  ifelse(log, log(d), d)
}

#' @rdname hw7q2
phw7q2 <- function(q, lower.tail = TRUE, log.p = FALSE) {
  p <- if(lower.tail) 3 * q^2 - 2 * q^3 else 1 - (3 * q^2 - 2 * q^3)
  ifelse(log.p, log(p), p)
}

#' @rdname hw7q2
qhw7q2 <- function(p, lower.tail = TRUE, log.p = FALSE) {
  assertive::assert_all_are_in_closed_range(x = p,
                                            lower = 0,
                                            upper = 1)

  p <- ifelse(log.p, exp(p), p)

  p <- if(lower.tail) p else 1 - p

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

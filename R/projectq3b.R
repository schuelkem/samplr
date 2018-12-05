#' find_support_element
#'
#' @param pdf a function that is the pdf of the random variable.
#' @param ... further arguments passed to or from other methods.
#'
#' @return a quantile in the support of the distribution
find_support_element <- function(pdf, ...) {
  q <- stats::rnorm(n = 1)
  while(do.call(pdf, list(x = q, ...)) == 0) {
    q <- stats::rnorm(n = 1)
  }
  q
}

#' projectq3b
#'
#' @description This function generates semi-random deviates from a continuous random variable with the supplied probability density function via Markov Chain Monte Carlo.
#'
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param pdf a function that is the pdf of the random variable.
#' @param a,b optional lower and upper limits of the distribution. If provided, must be finite such that \eqn{P(a \le X \le b) = 1}.
#' @param ... further arguments passed to or from other methods.
#'
#' @return numeric vector of semi-random deviates
#' @export
#'
#' @examples
#' ## sample from standard uniform
#' deviates <- projectq3b(n = 10000, pdf = dunif, a = 0, b = 1)
#' hist(deviates, probability = TRUE)
#' curve(dunif(x), col = "red", add = TRUE)
#'
#' ## sample from beta(2, 2)
#' deviates <- projectq3b(n = 10000, pdf = dbeta, a = 0, b = 1, shape1 = 2, shape2 = 2)
#' hist(deviates, probability = TRUE)
#' curve(dbeta(x = x, shape1 = 2, shape2 = 2), col = "red", add = TRUE)
#'
#' ## sample from custom random variable
#' dcustom <- function(x) x / 2
#' deviates <- projectq3b(n = 10000, pdf = dcustom, a = 0, b = 2)
#' hist(deviates, probability = TRUE)
#' curve(dcustom, col = "red", add = TRUE)
#'
#' ## sample from exponential with rate = 2
#' deviates <- projectq3b(n = 10000, pdf = dexp, rate = 2)
#' hist(deviates, probability = TRUE)
#' curve(dexp(x, rate = 2), col = "red", add = TRUE)
#'
#' ## sample from chi-square with df = 1
#' deviates <- projectq3b(n = 10000, pdf = dchisq, df = 1)
#' hist(deviates, probability = TRUE)
#' curve(dchisq(x, df = 1), col = "red", add = TRUE)
projectq3b <- function(n, pdf, a = NA, b = NA, ...) {
  assertive::assert_is_numeric(n)
  if(length(n) > 1)
    n <- length(n)

  assertive::assert_is_function(pdf)

  use_limits <- !is.na(a) && !is.na(b)

  if(use_limits) {
    assertive::assert_is_numeric(a)
    if(length(a) > 1)
      a <- min(a)

    assertive::assert_is_numeric(b)
    if(length(b) > 1)
      b <- max(b)
  }

  random.samples <- numeric(n)

  quantile.current <- ifelse(use_limits,
                             stats::runif(n = 1, min = a, max = b),
                             find_support_element(pdf, ...))

  for(i in 1:n) {
    quantile.proposed <- ifelse(use_limits,
                                stats::runif(n = 1, min = a, max = b),
                                find_support_element(pdf, ...))

    density.proposed <- do.call(pdf, list(x = quantile.proposed, ...))
    density.current <- do.call(pdf, list(x = quantile.current, ...))

    acceptance.ratio <- ifelse(use_limits,
                               density.proposed / density.current,
                               (density.proposed * stats::dnorm(x = quantile.current)) /
                                 (density.current * stats::dnorm(x = quantile.proposed)))

    accept <- stats::runif(n = 1) <= acceptance.ratio

    quantile.current <- ifelse(accept, quantile.proposed, quantile.current)

    random.samples[i] <- quantile.current
  }

  random.samples
}

#' projectq2a
#'
#' @description This function generates random deviates from a continuous random variable with the supplied probability density function via rejection sampling.
#'
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param pdf a function that is the pdf of the random variable.
#' @param a,b lower and upper limits of the distribution. Must be finite such that \eqn{P(a \le X \le b) = 1}.
#' @param C a numeric such that \eqn{f(x) \le C} for all values of x.
#' @param ... further arguments passed to or from other methods.
#'
#' @return numeric vector of random deviates
#' @export
#'
#' @examples
#' ## sample from standard uniform
#' projectq2a(n = 1, pdf = dunif, a = 0, b = 1, C = 1, min = 0, max = 1)
#'
#' ## plot many samples from standard uniform as densities
#' hist(projectq2a(n = 10000, pdf = dunif, a = 0, b = 1, C = 1, min = 0, max = 1),
#'      probability = TRUE)
#' curve(dunif, col = "red", add = TRUE)
#'
#' ## sample from beta(2, 2)
#' projectq2a(n = 1, pdf = dbeta, a = 0, b = 1, C = 1.5, shape1 = 2, shape2 = 2)
#'
#' ## plot many samples from beta(2, 2)
#' hist(projectq2a(n = 10000, pdf = dbeta, a = 0, b = 1, C = 1.5, shape1 = 2, shape2 = 2),
#'      probability = TRUE)
#' curve(dbeta(x, shape1 = 2, shape2 = 2), col = "red", add = TRUE)
projectq2a <- function(n, pdf, a, b, C, ...) {
  assertive::assert_is_numeric(n)
  if(length(n) > 1)
    n <- length(n)

  assertive::assert_is_function(pdf)

  assertive::assert_is_numeric(a)
  if(length(a) > 1)
    a <- min(a)

  assertive::assert_is_numeric(b)
  if(length(b) > 1)
    b <- max(b)

  assertive::assert_is_numeric(C)
  if(length(C) > 1)
    C <- max(C)

  random.samples <- numeric(n)
  for(i in 1:n) {
    rejecting <- TRUE
    while(rejecting) {
      quantile.candidate <- stats::runif(n = 1, min = a, max = b)
      quantile.candidate.density <- do.call(pdf, list(x = quantile.candidate, ...))
      rejection.sampling.critical.value <- stats::runif(n = 1, min = 0, max = C)
      rejecting <- quantile.candidate.density < rejection.sampling.critical.value
    }
    random.samples[i] <- quantile.candidate
  }
  random.samples
}

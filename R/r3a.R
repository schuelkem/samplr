#' d2dunif
#'
#' @description This function returns the density for a 2D uniform distribution defined on a square.
#'
#' @param x,y vector of quantiles.
#' @param min,max lower and upper limits of the distribution on either side. Must be finite.
#'
#' @return a numeric density
#' @export
#'
#' @examples
#' d2dunif(x = 0.5, y = 0.5)
#' d2dunif(x = 0, y = 2, min = 0, max = 2)
d2dunif <- function(x, y, min = 0, max = 1) {
  if(min <= x && x <= max && min <= y && y <= max)
    (max - min)^(-2)
  else
    0
}

d2dunif(x = 0, y = 0) # 1
d2dunif(x = 1, y = 1, min = 0, max = 2) # 1/4

#' r3a
#'
#' @description This function generates pairs of random deviates from a 2D continuous distribution defined on a square with the supplied joint probability density function via rejection sampling.
#'
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param pdf a function that is the joint pdf of the distribution. Should use x and y as quantile arguments.
#' @param a,b lower and upper limits of the sides of the distribution. Must be finite such that \eqn{P(a \le X \le b \cap a \le Y \le b) = 1}.
#' @param C a numeric such that \eqn{f(x,y) \le C} for all values of x and y.
#' @param ... further arguments passed to or from other methods.
#'
#' @return data.frame of random deviate pairs
#' @export
#'
#' @examples
#' plot(r3a(n = 10000, pdf = d2dunif, a = 0, b = 1, C = 1))
#' plot(r3a(n = 10000, pdf = d2dunif, a = 0, b = 2, C = 1/4, min = 0, max = 2))
r3a <- function(n, pdf, a, b, C, ...) {
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

  random.samples <- data.frame(x = numeric(0),
                               y = numeric(0))
  for(i in 1:n) {
    rejecting <- TRUE
    while(rejecting) {
      quantile.candidate <- runif(n = 2, min = a, max = b)
      quantile.candidate.density <- do.call(pdf, list(x = quantile.candidate[1], y = quantile.candidate[2], ...))
      rejection.sampling.critical.value <- runif(n = 1, min = 0, max = C)
      rejecting <- quantile.candidate.density < rejection.sampling.critical.value
    }
    random.samples <- rbind(random.samples,
                            data.frame(x = quantile.candidate[1],
                                       y = quantile.candidate[2]))
  }
  random.samples
}

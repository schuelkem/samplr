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
#' plot(projectq3a(n = 10000, jpdf = d2dunif, a = 0, b = 1, C = 1))
#' plot(projectq3a(n = 10000, jpdf = d2dunif, a = 0, b = 2, C = 1/4, min = 0, max = 2))
#' plot(projectq3a(n = 10000, jpdf = d2dcirclecontour, a = -1, b = 1, C = 1))
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
      quantile.candidates <- runif(n = 2, min = a, max = b)
      quantile.candidates.density <- do.call(jpdf, list(x = quantile.candidates[1], y = quantile.candidates[2], ...))
      rejection.sampling.critical.value <- runif(n = 1, min = 0, max = C)
      rejecting <- quantile.candidates.density < rejection.sampling.critical.value
    }
    random.samples <- rbind(random.samples,
                            data.frame(x = quantile.candidates[1],
                                       y = quantile.candidates[2]))
  }
  random.samples
}

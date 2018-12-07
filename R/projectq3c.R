#' find_quantile_2d
#'
#' @description This function trials quantiles until one is found with positive density.
#'
#' @param jpdf a function that is the joint pdf of the random variables.
#' @param ... further arguments passed to or from other methods.
#'
#' @return a quantile in the support of the distribution
find_quantile_2d <- function(jpdf, ...) {
  q <- rnorm(n = 2)
  while(do.call(jpdf, list(x = q[1], y = q[2], ...)) == 0) {
    q <- rnorm(n = 2)
  }
  q
}

#' projectq3c
#'
#' @description This function generates semi-random deviates from a continuous bivariate distribution with the supplied joint probability density function via Markov Chain Monte Carlo.
#'
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param jpdf a function that is the joint pdf of the distribution
#' @param ... further arguments passed to or from other methods.
#'
#' @return data.frame of semi-random deviates
#' @export
#'
#' @examples
#' plot(projectq3c(n = 10000, jpdf = jdunif))
#' plot(projectq3c(n = 10000, jpdf = jdunif, min = -1, max = 1))
#' plot(projectq3c(n = 10000, jpdf = jdcirclecontour))
#' plot(projectq3c(n = 10000, jpdf = jdnorm, r = 0.75))
projectq3c <- function(n, jpdf, ...) {
  assertive::assert_is_numeric(n)
  if(length(n) > 1)
    n <- length(n)

  assertive::assert_is_function(jpdf)

  random.samples <- data.frame(x = numeric(0),
                               y = numeric(0))

  quantile.current <- find_quantile_2d(jpdf = jpdf, ...)

  for(i in 1:n) {
    quantile.proposed <- find_quantile_2d(jpdf = jpdf, ...)

    density.current <- do.call(jpdf, list(x = quantile.current[1], y = quantile.current[2], ...))

    density.proposed <- do.call(jpdf, list(x = quantile.proposed[1], y = quantile.proposed[2], ...))

    acceptance.ratio <-
      (density.proposed * stats::dnorm(x = quantile.current[1]) * stats::dnorm(x = quantile.current[2])) /
      (density.current * stats::dnorm(x = quantile.proposed[1]) * stats::dnorm(x = quantile.proposed[2]))

    accept <- stats::runif(n = 1) <= acceptance.ratio

    if(accept)
      quantile.current <- quantile.proposed

    random.samples <- rbind(random.samples, data.frame(x = quantile.current[1],
                                                       y = quantile.current[2]))
  }

  random.samples
}

#' jdnorm
#'
#' @description This function returns the joint density from a bivariate random normal.
#'
#' @param x,y vector of quantiles
#' @param mean vector of both means
#' @param sd vector of both standard deviations
#' @param rho correlation between the two normals
#'
#' @return numeric vector of joint densities
#' @export
#'
#' @examples
#' ## center of standard bivariate normal
#' jdnorm(x = 0, y = 0)
#'
#' ## change scale
#' jdnorm(x = 0, y = 0, sd = c(10, 10))
#'
#' ## change location
#' jdnorm(x = 5, y = 5, mean = c(5, 5))
#'
#' ## scale-location transform
#' jdnorm(x = 42, y = 42, mean = c(42, 42), sd = c(10, 10))
#'
#' ## multiple samples from correlated
#' jdnorm(x = c(1, -1, -1, 1), y = c(1, 1, -1, -1), rho = 0.5)
jdnorm <- function(x, y, mean = c(0, 0), sd = c(1, 1), rho = 0) {
  f <- Vectorize(FUN = function(x, y, mean, sigma) {
    mvtnorm::dmvnorm(x = c(x, y),
                     mean = mean,
                     sigma = sigma)
  }, vectorize.args = c("x", "y"))

  cov <- rho*sd[1]*sd[2]
  sigma = matrix(c(sd[1]^2, cov, cov, sd[2]^2), ncol = 2)

  f(x = x, y = y, mean = mean, sigma = sigma)
}

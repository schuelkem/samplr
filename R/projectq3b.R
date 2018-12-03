#' projectq3b
#'
#' @description This function generates semi-random deviates from a continuous random variable with the supplied probability density function via Markov Chain Monte Carlo.
#'
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param pdf a function that is the pdf of the random variable.
#' @param a,b lower and upper limits of the distribution. Must be finite such that \eqn{P(a \le X \le b) = 1}.
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
#' curve(dcustom, add = TRUE)
projectq3b <- function(n, pdf, a, b, ...) {
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
  
  random.samples <- numeric(n)
  random.samples[1] <- runif(n = 1, min = a, max = b)
  for(i in 2:n) {
    quantile.candidate <- runif(n = 1, min = a, max = b)
    density.candidate <- do.call(pdf, list(x = quantile.candidate, ...))
    density.current <- do.call(pdf, list(x = random.samples[i-1], ...))
    density.ratio <- density.candidate / density.current
    random.samples[i] <- ifelse(runif(1) < density.ratio, 
                                quantile.candidate, 
                                random.samples[i-1])
  }
  random.samples
}

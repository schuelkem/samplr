## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(ggplot2)
library(samplr)

## ------------------------------------------------------------------------
df_uniform_samples <- 
  data.frame(u = rdist(n = 10000, 
                       pdf = dunif, 
                       a = 0, 
                       b = 1, 
                       C = 1, 
                       min = 0, 
                       max = 1))

ggplot(df_uniform_samples, aes(x = u)) + 
  geom_density() + 
  stat_function(aes(x = 0), fun = dunif, args = list(min = 0, max = 1), color = "red")

## ------------------------------------------------------------------------
df_beta_samples <- 
  data.frame(b = rdist(n = 10000, 
                       pdf = dbeta, 
                       a = 0, 
                       b = 1, 
                       C = 1.5, 
                       shape1 = 2, 
                       shape2 = 2))

ggplot(df_beta_samples, aes(x = b)) + 
  geom_density() + 
  stat_function(aes(x = 0), fun = dbeta, args = list(shape1 = 2, shape2 = 2), color = "red")

## ------------------------------------------------------------------------
dcustom <- function (x) {
  sapply(X = x, 
         FUN = function(x) {
           ifelse(0 < x & x < 1, 
                  (gamma(2+5)/(gamma(2)*gamma(5)))*(x^(2-1)*(1-x)^(5-1)), 
                  0)
         })
}

df_custom_samples <- 
  data.frame(b = rdist(n = 10000, 
                       pdf = dcustom, 
                       a = 0, 
                       b = 1, 
                       C = 2.5))

ggplot(df_custom_samples, aes(x = b)) + 
  geom_density() + 
  stat_function(aes(x = 0), fun = dcustom, color = "red")


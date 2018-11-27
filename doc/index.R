## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

library(dplyr)
library(ggplot2)
library(MASS)
library(plotly)
library(samplr)

## ---- fig.align='center'-------------------------------------------------
df_uniform_samples <- 
  data.frame(u = projectq2a(n = 10000, 
                       pdf = dunif, 
                       a = 0, 
                       b = 1, 
                       C = 1, 
                       min = 0, 
                       max = 1))

ggplot(df_uniform_samples, aes(x = u)) + 
  geom_density(color = NA, fill = "#35a09c") + 
  stat_function(aes(x = 0), fun = dunif, args = list(min = 0, max = 1), color = "#fff735", size = 1) + 
  labs(x = "X", 
       y = "Density") + 
  theme_minimal()

## ---- fig.align='center'-------------------------------------------------
df_beta_samples <- 
  data.frame(b = projectq2a(n = 10000, 
                       pdf = dbeta, 
                       a = 0, 
                       b = 1, 
                       C = 1.5, 
                       shape1 = 2, 
                       shape2 = 2))

ggplot(df_beta_samples, aes(x = b)) + 
  geom_density(color = NA, fill = "#35a09c") + 
  stat_function(aes(x = 0), fun = dbeta, args = list(shape1 = 2, shape2 = 2), color = "#fff735", size = 1) + 
  labs(x = "X", 
       y = "Density") + 
  theme_minimal()

## ------------------------------------------------------------------------
dcustom <- function (x) {
  sapply(X = x, 
         FUN = function(x) {
           ifelse(0 < x & x < 1, 
                  (gamma(2+5)/(gamma(2)*gamma(5)))*(x^(2-1)*(1-x)^(5-1)), 
                  0)
         })
}

## ---- fig.align='center'-------------------------------------------------
df_custom_samples <- 
  data.frame(b = projectq2a(n = 10000, 
                       pdf = dcustom, 
                       a = 0, 
                       b = 1, 
                       C = 2.5))

ggplot(df_custom_samples, aes(x = b)) + 
  geom_density(color = NA, fill = "#35a09c") + 
  stat_function(aes(x = 0), fun = dcustom, color = "#fff735", size = 1) + 
  labs(x = "X", 
       y = "Density") + 
  theme_minimal()

## ------------------------------------------------------------------------
d2dunif <- function(x, y, min = 0, max = 1) {
  if(min <= x && x <= max && min <= y && y <= max)
    (max - min)^(-2)
  else
    0
}

## ---- out.width='100%', out.height='400px'-------------------------------
l <- list(x = seq(0, 1, 0.5), 
          y = seq(0, 1, 0.5))

z <- matrix(rep(NA, length(l$x) * length(l$y)), 
            nrow = length(l$x))
for(r in 1:nrow(z)) {
  for(c in 1:ncol(z)) {
    z[r, c] <- d2dunif(x = l$x[r], y = l$y[c])
  }
}
l$z <- z

plot_ly(x = l$x, y = l$y, z = l$z) %>% 
  add_surface() %>% 
  layout(scene = list(xaxis = list(title = "X"), 
                      yaxis = list(title = "Y"), 
                      zaxis = list(title = "Density")))

## ---- out.width='100%', out.height='400px'-------------------------------
df <- projectq3a(n = 10000, jpdf = d2dunif, a = 0, b = 1, C = 1)
kd <- with(df, kde2d(x, y, n = 100))
plot_ly(x = kd$x, y = kd$y, z = kd$z) %>% 
  add_surface() %>% 
  layout(scene = list(xaxis = list(title = "X"), 
                      yaxis = list(title = "Y"), 
                      zaxis = list(title = "Density")))

## ------------------------------------------------------------------------
d2dbeta <- function(x, y, shape1, shape2) {
  if(0 <= x && x <= 1 && 0 <= y && y <= 1)
    (dbeta(x = x, shape1 = shape1, shape2 = shape2) + 
       dbeta(x = y, shape1 = shape1, shape2 = shape2)) / 2
  else
    0
}

## ---- out.width='100%', out.height='400px'-------------------------------
l <- list(x = seq(0, 1, 0.01), 
          y = seq(0, 1, 0.01))

z <- matrix(rep(NA, length(l$x) * length(l$y)), 
            nrow = length(l$x))
for(r in 1:nrow(z)) {
  for(c in 1:ncol(z)) {
    z[r, c] <- d2dbeta(x = l$x[r], y = l$y[c], shape1 = 2, shape2 = 2)
  }
}
l$z <- z

plot_ly(x = l$x, y = l$y, z = l$z) %>% 
  add_surface() %>% 
  layout(scene = list(xaxis = list(title = "X"), 
                      yaxis = list(title = "Y"), 
                      zaxis = list(title = "Density")))

## ---- out.width='100%', out.height='400px'-------------------------------
df <- projectq3a(n = 10000, jpdf = d2dbeta, a = 0, b = 1, C = 1.5, shape1 = 2, shape2 = 2)
kd <- with(df, kde2d(x, y, n = 100))
plot_ly(x = kd$x, y = kd$y, z = kd$z) %>% 
  add_surface() %>% 
  layout(scene = list(xaxis = list(title = "X"), 
                      yaxis = list(title = "Y"), 
                      zaxis = list(title = "Density")))

## ------------------------------------------------------------------------
d2dcirclecontour <- function(x, y) {
  if(-1 <= x && x <= 1 && -1 <= y && y <= 1)
    (3/8)*(x^2 + y^2)
  else
    0
}

## ---- out.width='100%', out.height='400px'-------------------------------
l <- list(x = seq(-1, 1, 0.1), 
          y = seq(-1, 1, 0.1))

z <- matrix(rep(NA, length(l$x) * length(l$y)), 
            nrow = length(l$x))
for(r in 1:nrow(z)) {
  for(c in 1:ncol(z)) {
    z[r, c] <- d2dcirclecontour(x = l$x[r], y = l$y[c])
  }
}
l$z <- z

plot_ly(x = l$x, y = l$y, z = l$z) %>% 
  add_surface() %>% 
  layout(scene = list(xaxis = list(title = "X"), 
                      yaxis = list(title = "Y"), 
                      zaxis = list(title = "Density")))

## ---- out.width='100%', out.height='400px'-------------------------------
df <- projectq3a(n = 10000, jpdf = d2dcirclecontour, a = -1, b = 1, C = 0.75)
kd <- with(df, kde2d(x, y, n = 100))
plot_ly(x = kd$x, y = kd$y, z = kd$z) %>% 
  add_surface() %>% 
  layout(scene = list(xaxis = list(title = "X"), 
                      yaxis = list(title = "Y"), 
                      zaxis = list(title = "Density")))


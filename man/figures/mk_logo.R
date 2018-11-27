library(hexSticker)
library(magick)

n <- 5000
cols <- c(teal = "#35A09C",
          olive = "#423E28",
          yellow = "#fff735")
lwd <- 1
set.seed(63123)

run <- function(i, P, n) {
  res <- integer(n)
  for (t in seq_len(n))
    res[[t]] <- i <- sample(nrow(P), 1, pr = P[i, ])
  res
}

P <- rbind(c(.5,  .25, .25),
           c(.1,  .3,  .6),
           c(.25, .25, .5))

v <- eigen(t(P), FALSE)$vectors[, 1]
v <- v / sum(v) # normalise eigenvector

samples <- run(1, P, n)

sticker(
  expression(
    plot(
      cummean(samples == 1),
      type = "l",
      col = cols[1],
      ylim = c(0, 1),
      log = "x",
      xlab = "",
      ylab = "",
      las = 1,
      axes = F,
      lwd = lwd
    ),
    lines(
      cummean(samples == 2),
      col = cols[2],
      lwd = lwd
    ),
    lines(
      cummean(samples == 3),
      col = cols[3],
      lwd = lwd
    ),
    abline(
      h = v,
      lty = 2,
      col = cols,
      lwd = lwd
    )
  ),
  package = "samplr",
  p_size = 18,
  p_color = "#35A09C",
  s_x = 0.75,
  s_y = 0.6,
  s_width = 1.75,
  s_height = 1.75,
  filename = "img/samplr_logo.png",
  h_fill = NA,
  h_color = "#35A09C",
  url = "github.com/schuelkem/samplr",
  u_color = "#35A09C",
  u_size = 5
)

# make small version (120x139)
logo <- image_read(path = "img/samplr_logo.png")
logo_small <- image_scale(image = logo,
                          geometry = "120x")
image_write(image = logo_small,
            path = "img/samplr_logo_small.png")

# put full size copy in vignette folder
image_write(image = logo,
            path = "vignettes/samplr_logo.png")

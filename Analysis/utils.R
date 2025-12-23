# Define Fuzzy Function (Monotonically Decreasing)
fuzzy_decrease <- function(x, max_dist) {
  val <- 1 - (x / max_dist)
  val[val < 0] <- 0
  return(val)
}

fuzzy_increase <- function(x, min_dist) {
  val <- (x - min_dist) / (max(x) - min_dist)
  val[val < 0] <- 0
  return(val)
}

# Define Fuzzy Function (Symmetric / Trapezoidal)
fuzzy_symmetric <- function(x, a, b, c, d) {
  y <- rep(0, length(x))

  # Rising limb (a to b)
  idx_rise <- x > a & x < b
  y[idx_rise] <- (x[idx_rise] - a) / (b - a)

  # Plateau (b to c) - The "Sweet Spot"
  idx_flat <- x >= b & x <= c
  y[idx_flat] <- 1

  # Falling limb (c to d)
  idx_fall <- x > c & x < d
  y[idx_fall] <- 1 - (x[idx_fall] - c) / (d - c)

  return(y)
}

curve(fuzzy_decrease(x, max_dist=10000), from=0, to=20000)
curve(fuzzy_increase(x, min_dist=5000), from=0, to=20000)
curve(fuzzy_symmetric(x, a=2000, b=4000, c=6000, d=8000), from=0, to=10000)

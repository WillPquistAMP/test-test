# Basic math functions

add <- function(x, y) {
  x + y
}

subtract <- function(x, y) {
  x - y
}

multiply <- function(x, y) {
  x * y
}

divide <- function(x, y) {
  if (any(y == 0)) {
    stop("division by zero")
  }
  x / y
}

power <- function(x, y) {
  x ^ y
}

square_root <- function(x) {
  if (any(x < 0)) {
    stop("square_root is undefined for negative values")
  }
  sqrt(x)
}

factorial_n <- function(n) {
  if (length(n) != 1 || is.na(n) || n < 0 || n != as.integer(n)) {
    stop("n must be a single non-negative integer")
  }
  if (n <= 1) 1 else prod(seq_len(n))
}

is_even <- function(n) {
  n %% 2 == 0
}

mean_value <- function(x) {
  mean(x)
}

median_value <- function(x) {
  median(x)
}

mode_value <- function(x) {
  counts <- table(x)
  values <- names(counts)[counts == max(counts)]
  if (is.numeric(x)) as.numeric(values) else values
}

variance <- function(x) {
  var(x)
}

std_dev <- function(x) {
  sd(x)
}

range_value <- function(x) {
  max(x) - min(x)
}

absolute <- function(x) {
  abs(x)
}

sum_all <- function(x) {
  sum(x)
}

product_all <- function(x) {
  prod(x)
}

round_to <- function(x, digits = 0) {
  round(x, digits)
}

clamp <- function(x, lower, upper) {
  if (lower > upper) {
    stop("lower must not be greater than upper")
  }
  pmin(pmax(x, lower), upper)
}

gcd <- function(a, b) {
  a <- abs(a)
  b <- abs(b)
  while (b != 0) {
    t <- b
    b <- a %% b
    a <- t
  }
  a
}

lcm <- function(a, b) {
  if (a == 0 || b == 0) {
    return(0)
  }
  abs(a * b) / gcd(a, b)
}

is_prime <- function(n) {
  if (length(n) != 1 || is.na(n) || n != as.integer(n)) {
    stop("n must be a single integer")
  }
  if (n < 2) {
    return(FALSE)
  }
  if (n < 4) {
    return(TRUE)
  }
  if (n %% 2 == 0) {
    return(FALSE)
  }
  divisor <- 3
  while (divisor * divisor <= n) {
    if (n %% divisor == 0) {
      return(FALSE)
    }
    divisor <- divisor + 2
  }
  TRUE
}

fibonacci <- function(n) {
  if (length(n) != 1 || is.na(n) || n < 0 || n != as.integer(n)) {
    stop("n must be a single non-negative integer")
  }
  if (n == 0) {
    return(numeric(0))
  }
  out <- numeric(n)
  out[1] <- 0
  if (n > 1) {
    out[2] <- 1
  }
  for (i in seq_len(n)[-(1:2)]) {
    out[i] <- out[i - 1] + out[i - 2]
  }
  out
}

log_base <- function(x, base = exp(1)) {
  if (any(x <= 0)) {
    stop("x must be positive")
  }
  if (base <= 0 || base == 1) {
    stop("base must be positive and not equal to 1")
  }
  log(x, base = base)
}

exponential <- function(x) {
  exp(x)
}

percent_change <- function(old, new) {
  if (any(old == 0)) {
    stop("old value must not be zero")
  }
  (new - old) / abs(old) * 100
}

percent_of <- function(part, whole) {
  if (any(whole == 0)) {
    stop("whole must not be zero")
  }
  part / whole * 100
}

hypotenuse <- function(a, b) {
  sqrt(a ^ 2 + b ^ 2)
}

circle_area <- function(radius) {
  if (any(radius < 0)) {
    stop("radius must be non-negative")
  }
  pi * radius ^ 2
}

deg_to_rad <- function(degrees) {
  degrees * pi / 180
}

rad_to_deg <- function(radians) {
  radians * 180 / pi
}

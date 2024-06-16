#' @export

golden_ratio <- 1.618033988749894

#' Fractals
#'
#' @param base  x
#' @param new x
#' @param strength  x
#' @param ... idk
#'
#' @return x
#' @export



## 8,6,4

frac1 <- function(base, new, strength, ...) {
  base + (strength^new)
}
#' @export
frac2 <- function(base, new, strength, ...) {
  base + (new^log(abs(strength)))
}
#' @export
frac3 <- function(base, new, strength, ...) {
  base + (2 * sin(new) - 1) * exp(abs(strength))
}
attr(frac3, "finalise") <- function(x) x + 0.5

#' @export
frac4 <- function(base, new, strength, ...) {
  base + (base * sin(new)) + abs(sin(strength))
}

#' @export
frac5 <- function(base, new, strength, ...) {
  base + (new / strength)
}

#' @export

frac6 <- function(base, new, strength, ...) {
  base * strength + new
}

#' @export

frac7 <- function(base, new, strength, ...) {
  base + (new + golden_ratio * strength)
}
#' @export

frac8 <- function(base, new, strength, ...) {
  base * golden_ratio + strength + new
}

#' @export
frac9 <- function(base, new, strength, ...) {
  sin(base) + cos(new) + tan(strength)
}

#' @export
frac10 <- function(base, new, strength, ...) {
  base + (2 * sin(new) - 1) * strength
}

#' @export
frac11 <- function(base, new, strength, ...) {
  base + new / strength
}

#' @export
frac12 <- function(base, new, strength, ...) {
  (cos(base) + pi * new) / strength
}
#' @export
frac13 <- function(base, new, strength, ...) {
  tanh(base) + (new * strength)
}
#' @export
frac14 <- function(base, new, strength, ...) {
  (2 + abs(base) - 1) + abs(strength)^new
}


#' @export
frac15 <- function(base, new, strength, ...) {
  base + strength / new
}

#' @export
frac16 <- function(base, new, strength, ...) {
  sin(base) + new
}
attr(frac16, "finalise") <- function(x) abs(x)

#' @export
frac17 <- function(base, new, strength, ...) {
  base + abs(new^exp(abs(strength)))
}



#' @export
frac_fun <- function(all = FALSE) {
  frac_names <- c(
    "frac1",
    "frac2",
    "frac3",
    "frac4",
    "frac5",
    "frac6",
    "frac7",
    "frac8",
    "frac9",
    "frac10",
    "frac11",
    "frac12",
    "frac13",
    "frac14",
    "frac15",
    "frac16",
    "frac17"
  )
  if (all) {
    requireNamespace("ambient", quietly = TRUE)
    frac_names <- append(frac_names, c("fbm", "billow", "ridged", "clamped"))
  }
  return(frac_names)
}

### Read csv with encoding from ssb, london etc..

#' Read csv from european encoding
#'
#' @param file Path to file
#' @param skip Skipping rows
#' @param encoding encoding (CP1252 is defalt)
#' @param ... passed on to vroom::vroom
#'
#' @return
#' @export
#'
#' @examples
#'
read_csv_europe <- function(file,
                            skip = 0,
                            encoding = "CP1252",
                            ...) {
  csv <-
    vroom::vroom(file, skip = skip, locale = vroom::locale(encoding = encoding), ...)

  return(csv)
}

##########
#' Loading packages
#'
#' @param type type startup
#' @param paralell if parralell
#'
#' @return
#' @export
#'
#' @examples
#'
#' pederlib::startup(1)
startup <- function(type = 1, paralell = FALSE) {
  if (!is.numeric(type)) {
    rlang::abort("Type must be numeric")
  }
  if (type == 1) {
    suppressPackageStartupMessages(library(tidyverse))
    suppressPackageStartupMessages(library(lubridate))
    mes <- "Tidyverse and Lubridate has been loaded."
  } else if (type == 3) {
    suppressPackageStartupMessages(library(tidyverse))
    suppressPackageStartupMessages(library(lubridate))
    suppressPackageStartupMessages(library(rvest))
    mes <- "Tidyverse, Lubridate and Rvest has been loaded."
  } else if (type == 4) {
    suppressPackageStartupMessages(library(tidyverse))
    suppressPackageStartupMessages(library(lubridate))
    suppressPackageStartupMessages(library(tidymodels))
    suppressPackageStartupMessages(library(psych))
    suppressPackageStartupMessages(library(blandr))
    suppressPackageStartupMessages(library(gtsummary))
    suppressPackageStartupMessages(library(gt))
    suppressPackageStartupMessages(library(blandaltmanR))
    suppressPackageStartupMessages(library(irrCAC))
    mes <- "Tidymodels, Tidyverse, Lubridate,  psych, blandr, gtsummary, gt, irrCAC, and blandaltmanR has been loaded."
  } else {
    suppressPackageStartupMessages(library(baguette))
    suppressPackageStartupMessages(library(lubridate))
    suppressPackageStartupMessages(library(tidymodels))
    suppressPackageStartupMessages(library(tidyverse))
    suppressPackageStartupMessages(library(finetune))
    suppressPackageStartupMessages(library(textrecipes))
    suppressPackageStartupMessages(library(stacks))
    suppressPackageStartupMessages(library(lubridate))
    suppressPackageStartupMessages(library(themis))
    suppressPackageStartupMessages(library(multilevelmod))

    mes <- "Tidymodels, Tidyverse, Lubridate,  Finetune, Baguette, Themis, Lubridate, Textrecipes, Stacks and Multilevelmod has been loaded."
  }

  if (paralell) {
    mes <- paste0(mes, "\n", "Parallel processing has been initiated.")
    doParallel::registerDoParallel(cores = 8)
  }
  suppressPackageStartupMessages(library(pederlib))
  theme_set(pederlib::theme_center())


  ggplot2::update_geom_defaults("rect", list(fill = "#1d3557", alpha = 0.9))
  ggplot2::update_geom_defaults("point", list(color = "#1d3557", alpha = 0.9))
  mes <- paste0(mes, "\nTheme set to theme_center.\nGeom defaults updated.")
  cat(mes)
  invisible(NULL)
}




###### prosent

#' Prosent
#'
#' @param tbl  a tbble
#' @param na.rm NA-removing of the sum part
#'
#' @return
#' @export
#'
#' @examples
pct <- function(tbl, na.rm = TRUE) {
  tbl %>%
    dplyr::mutate(pct = n / sum(n, na.rm = na.rm))
}


#############################

#' This is a summarize function, and return to into a tibble
#' @description
#' * Mode
#' * Median
#' * Arithmetic mean
#' * Harmonic mean
#' * Geometric mean
#' * SD
#' * SE
#' * TODO: Generalized means, Interquartile mean,Pythagorean mean
#'
#' @param x A numeric vector
#' @param na.rm If NA remove
#'
#' @return
#' @export
#'
#' @examples
#' a <- seq(1, 9, 2)
#'
#' c <- rep(c(a, NA), 10)
#' d <- c(1, 2, 3, 3, 20, 31)
#' e <- c(NA, NA, NA, NA, 3, 3, 2)
#' f <- c(5, 10, 17, 24, 30)
#' e <- rnorm(1e4)
#' sum_fun(a)
#' sum_fun(c, na.rm = TRUE)
#' sum_fun(d)
#' sum_fun(e, na.rm = TRUE)
#' sum_fun(f)
#' sum_fun(e)
sum_fun <- function(x, na.rm = FALSE) {
  if (!is.numeric(x)) {
    stop("Input must be numeric")
  }

  if (any(is.na(x))) {
    if (na.rm == FALSE) {
      stop("Vector contains NA, reconsider")
    } else {
      x <- stats::na.omit(x)
    }
  }

  sum_x <- sum(x)
  length_m <- length(x)
  mean <- sum_x / length_m
  geo <- prod(x)^(1 / length_m)
  ## Mode
  mode_x <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  mode <- mode_x(x)

  ## harmonic
  i <- 0
  harm <- vector(mode = "numeric", length = length_m)
  test <- while (i < length_m + 1) {
    harm[i] <- 1 / x[i]
    i <- i + 1
  }
  h_mean <- length_m / sum(harm)

  ### SDV
  i <- 0
  sdv <- vector(mode = "numeric", length = length_m)
  # Burde vært for-loop
  while (i < length_m + 1) {
    sdv[i] <- (x[i] - (sum(x) / length_m))^2
    i <- i + 1
  }

  sorted <- sort(x)
  med <- ifelse(length_m %% 2 == 1, sorted[(length_m / 2)], mean(sorted[length_m / 2 + 0:1]))

  sd <- sqrt(sum(sdv) / (length_m - 1))
  se <- sd / sqrt(length_m)

  output <- tibble::tibble(mean = mean, geo_mean = geo, harm_mean = h_mean, median = med, standard_deviation = sd, standard_error = se, n = length_m)
  return(output)
}
###### function for finding mode in a numeric vector

#' Mode
#'
#' @param x Vector to find mode
#' @param na.rm na rm
#'
#' @return
#' @export
#'
#' @examples
#'
mode_vec <- function(x, na.rm = FALSE) {
  if (!is.numeric(x)) {
    stop("Imput must be numberic")
  }

  if (any(is.na(x))) {
    if (na.rm == FALSE) {
      rlang::inform("Vector contains NA")
      x <- NA
    } else {
      x <- stats::na.omit(x)
    }
  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

### Read csv with encoding from ssb, london etc..

#' Read csv from european encoding
#'
#' @param file Path to file
#' @param skip Skipping rows
#' @param encoding encoding (CP1252 is defalt)
#' @param ... passed on to vroom::vroom
#'
#' @return tibble
#' @export
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
#' @description type 1 Normal, 2 modeling, 3 webscraping, 4 psychometrics, 5 mixedmodels
#'
#' @param ... 1 Normal, 2 modeling, 3 webscraping, 4 psychometrics, 5 mixedmodels
#' @param parallel if parallel
#' @param  stan_op Include stan
#'
#' @return nothing
#' @export
#'
#' @examples
#'
#' pederlib::startup(1)
startup <-  function(..., parallel = FALSE, stan_op = FALSE) {

  attached_pack <- .packages()

  rlang::check_dots_unnamed()
  pack <- vector()

  tidyverse_pack <- c("ggplot2",
                 "tibble",
                 "tidyr",
                 "readr",
                 "purrr",
                 "dplyr",
                 "stringr",
                 "forcats")

  tidymodels_pack <- c("broom",
                  "dials",
                  "dplyr",
                  "ggplot2",
                  "infer",
                  "modeldata",
                  "parsnip",
                  "purrr",
                  "recipes",
                  "rsample",
                  "tibble",
                  "tidyr",
                  "tune",
                  "workflows",
                  "workflowsets",
                  "yardstick")

  dots <-
    unlist(rlang::dots_values(...))

  if (length(dots)==0) {
    dots = 1
  }
  if (is.character(dots)) {
    dots <-  str_extract(l, "[:digit:]")
    dots <- dots[!is.na(dots)]
    dots <- as.integer(dots)
  }
  if (length(dots)==0) {
    dots = 1
  }

  ###
  if (any(dots ==1)) {
   # pack <- append(pack, tidyverse_pack)
    pack <- append(pack, c("tidyverse","lubridate"))

  }


  tm_addons <- c("baguette",
                 "finetune",
                 "textrecipes",
                 "stacks",
                 "themis",
                 "multilevelmod",
                 "censored",
                 "discrim",
                 "plsmod",
                 "rules",
                 "baguette",
                 "embed",
                 "spatialsample",
                 "tidyposterior",
                 "shinymodels",
                 "extrasteps",
                 "timetk",
                 "bonsai",
                 "rstanarm")

  if (any(dots ==2)) {
   # pack <- append(pack, tidyverse_pack)
   # pack <- append(pack, tidymodels_pack)
    pack <- append(pack, c("tidyverse",
                           "tidymodels",
                           "lubridate"))
    pack <- append(pack, tm_addons)

  }
  if (any(dots ==3)) {
    #pack <- append(pack, tidyverse_pack)
    pack <- append(pack, c("tidyverse",
                           "lubridate",
                           "rvest"))
  }
  if (any(dots == 4)) {
    #pack <- append(pack, tidyverse_pack)
   # pack <- append(pack, tidymodels_pack)
    pack <- append(pack, c("tidyverse",
                           "lubridate",
                           "tidymodels",
                           "psych",
                           "blandr",
                           "gtsummary",
                           "gt",
                           "blandaltmanR",
                           "irrCAC"))
  }
  if (any(dots ==5)) {
   # pack <- append(pack, tidyverse_pack)
   # pack <- append(pack, tidymodels_pack)
    pack <- append(pack, c("tidyverse",
                           "tidymodels",
                           "lubridate",
                           "Matrix",
                           "lmerTest",
                           "lme4",
                           "broom.mixed",
                           "multilevelmod",
                           "nlme",
                           "gamm4",
                           "blme",
                           "cAIC4"))
  }
  if (any(dots >10)) {
    pack <- append(pack, c("MASS",
                           "tidyverse",
                           "tidymodels",
                           "lubridate"))
    pack <- append(pack, tm_addons)
    pack <- append(pack, c("tidyverse",
                           "lubridate",
                           "tidymodels",
                           "psych",
                           "blandr",
                           "gtsummary",
                           "gt",
                           "blandaltmanR",
                           "irrCAC",
                           "Matrix",
                           "lmerTest",
                           "lme4",
                           "broom.mixed",
                           "multilevelmod",
                           "nlme",
                           "gamm4",
                           "blme",
                           "cAIC4",
                           "tidytext",
                           "tidylo",
                           "rstanarm"

    ))
  }
  if (any(dots == 6)) {
    pack <- append(pack,  c("tidyverse",
                            "lubridate",
                            "ggforce",
                            "colorspace",
                            "particles",
                            "ambient",
                            "viridis",
                            "ggtrace",
                            "scico",
                            "ggfx",
                            "patchwork",
                            "transformr",
                            "farver",
                            "scales",
                            "ggdist",
                            "ggpointdensity",
                            "cowplot",
                            "gghalves" )
)
  }


  pack <- append(pack, "pederlib")
  pack <- unique(pack)
  old_pack <- pack[pack %in% attached_pack]
  pack <- pack[!pack %in% attached_pack]

  # dots_logial <- append(all(dots>5),all(dots <1) )
  # print(dots_logial)
  #
  # if (any(dots_logial==TRUE)) {
  #   print("Hei")
  # }
  attach <- vector()
  if (length(pack)>0) {
    cat("Loaded packages: \n")
    for (i in pack) {
      x <- suppressMessages(suppressWarnings(require(i, character.only = TRUE)))
      attach <- append(attach, i)
    #  print(attach)
      Sys.sleep(0.005)
     if (x) {
       if (i == "tidyverse") {
         cat("Tidyverse: \n")
         for (j in tidyverse_pack) {
           cli::cli_alert_success(paste0("\t", j, " (", packageVersion(j),")"))
         }
         cat("\n")
       }
       if (i == "tidymodels") {
         cat("Tidymodels: \n")
         tm <- tidymodels_pack[!tidymodels_pack %in% tidyverse_pack]
      #   print(tm)
         for (k in tm) {
           cli::cli_alert_success(paste0("\t", k," (", packageVersion(i),")"))
         }
         cat("\n")
       }
       # if (i == "tidymodelsaddons") {
       #   cat("Tidymodels - add-on: \n")
       #   for (m in tm_addons) {
       #     cli::cli_alert_success(paste0("\t", m))
       #   }
       #   cat("\n")
       # }

       else {
         cli::cli_alert_success(paste0(i, " (", packageVersion(i),")"))
       }

      }
      else {
        cli::cli_alert_warning(i)
      }
    }
  }

  if (length(old_pack)>0) {
    cat("\nThese packages is already loaded 🙈   \n")
    for (i in old_pack) {
      cli::cli_alert_info(i)
    }
  }

  cat("\n\n")
  cores <- parallel::detectCores()
  if (parallel) {

    doParallel::registerDoParallel(cores = 8)
    cli::cli_alert_success(glue::glue("Parallel processing has been initiated with {cores} cores. 🤖"))
  }

  if (stan_op) {
    mc.cores = parallel::detectCores()
    rstan_options(auto_write = TRUE)
    cli::cli_alert_success(glue::glue("MC-cores has been set to {cores} cores. 🎲\n \t     auto_write is set to TRUE"))
  }

  if (identical(ggplot2:::ggplot_global$theme_current, pederlib::theme_center())) {
    cli::cli_alert_info("Theme is already theme_center  🙈")

  }
  else {
    ggplot2::theme_set(pederlib::theme_center())
    cli::cli_alert_success("Theme set to theme_center 👌")
  }

  if (ggplot2:::GeomPoint$default_aes$colour == "#1d3557") {
    cli::cli_alert_info("Geom defaults is already updated 🙈")
  }
  else {
    ggplot2::update_geom_defaults("rect", list(fill = "#1d3557", alpha = 0.9))
    ggplot2::update_geom_defaults("point", list(color = "#1d3557", alpha = 0.9))
    cli::cli_alert_success("Geom defaults updated ✌️")
  }

  invisible(pack)
}




###### prosent

#' Prosent
#'
#' @param tbl  a tbble
#' @param na.rm NA-removing of the sum part
#'
#' @return tbl
#' @export

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
#' @return a tibble
#' @export
#'
#' @examples
#' a <- seq(1, 9, 2)
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
#' @return mode
#' @export
#'
#' @examples
#'x <- c(1,2,3,1,2,3,3)
#'mode_vec(x)
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



#' Title
#'
#' @param x character vector of JSON like structuse
#'
#' @return a list of character vectors
#' @export
parse_genre_JSON <- function(x) {
  rs <-
    x %>%
    stringr::str_split(",") %>%
    purrr::map(~ stringr::str_remove_all(.x, "\\'")) %>%
    purrr::map(~ stringr::str_remove_all(.x, "[:punct:]")) %>%
    purrr:: map(stringr::str_squish)
  return(rs)
}

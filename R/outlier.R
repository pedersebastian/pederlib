###########


## ** OUTLIER **

###########
#library(tidyverse)
#library(rlang)

filter_outlier <- function(.data, var, ...,  method = c("mean_sd", "MAD", "IQD", "t_test"), threshold = "default", conf_int = NULL) {
  if (missing(.data)) {
    rlang::abort(".data must be supplied")
  }
  UseMethod("filter_outlier")
}

# Mean and Standard Deviation Method
#
# Median and Median Absolute Deviation Method (MAD)
#
# Median and Interquartile Deviation Method (IQD)


#***Noe med z-value eller t-test

filter_outlier.default  <- function(.data, var, ..., method, threshold) {
  mes = paste("filter_outlier does not support data of type", class(.data)[[1]])
  rlang::abort(mes)

}

outlier_threshold <- function(method, threshold) {
  if (threshold == "default") {
    threshold <-  switch (method,
            "mean_sd" = 3,
            "MAD" = 3,
            "IQD" = 2.2,
            "t_test" = 3,
            rlang::abort(paste(method, "is not valid method"))
    )
  }
  else if (!is.numeric(threshold)) {
    rlang::abort("if threshold is not default it must be numeric", )
  }
  else if (length(threshold) != 1) {
    rlang::abort("threshold must be of length 1")
  }
  if (threshold<0) {
    rlang::warn("threshold must be positive - using absolute value")
    threshold <- abs(threshold)
  }

  if (threshold > 5 | threshold < 1) {
    rlang::warn("Extreme value of threshold")
  }
  threshold
}

filter_outlier.data.frame <- function(.data, var, ..., method, threshold = "default", conf_level = NULL) {


  method = match.arg(method, c("mean_sd", "MAD", "IQD", "t_test"), several.ok = FALSE)

  if (!method %in% c("t_test")) {
    threshold <-
      outlier_threshold(method, threshold)
  }
  else if (method == "t_test") {
    if (is.null(conf_level)) {
      conf_level <- 0.95
      print("HEI")
    }
    else if (!is.numeric(conf_level)) {
      cli::cli_abort("conf_int must be numeric")
    }
    else if (conf_level >=1 | conf_level <= 0 | length(conf_level)> 1) {
      cli::cli_abort("'conf_level' must be a single number between 0 and 1")
    }
  }
  print(conf_level)
  var = enquo(var)

  filter_outlier.impl(.data, var, method = method, threshold = threshold, conf_level = conf_level)
}

filter_outlier.impl <- function(.data, var, ..., method, threshold, conf_level) {
  #print(method)
print(threshold)
  print(conf_level)
  switch (method,
          "mean_sd" = outlier_mean_sd(.data, var, threshold),
          "MAD" = outlier_MAD(.data, var, threshold),
          "IQD" = outlier_IQD(.data, var, threshold),
          "t_test" = outlier_t_test(.data, var, conf_level)
  )

  #print(vars)
  #.data[[vars]] |> na.omit()
}

outlier_mean_sd <- function(.data, var, threshold) {
  tbl <-
    dplyr::summarise(.data,
                   mean_var = mean(!!var, na.rm = TRUE),
                   sd_var = sd(!!var, na.rm = TRUE),
                   upper = mean_var + sd_var * threshold,
                   lower = mean_var - sd_var * threshold)

  outlier_return(.data, var, tbl$upper, tbl$lower)

  # sende tilbake *ferdig data, *statement xx> 4 && statement <8 ..  *

}
outlier_MAD <- function(.data, var, threshold) {

  tbl <-
    dplyr::summarise(.data,
                      median_var = median(!!var, na.rm = TRUE),
                      resid_median = median(abs(!!var) - median_var, na.rm = TRUE),
                      upper = median_var + resid_median * threshold,
                      lower = median_var - resid_median * threshold)
  print(tbl)
  outlier_return(.data, var, tbl$upper, tbl$lower)
}
outlier_IQD <- function(.data, var, threshold) {
  tbl <-
    dplyr::summarise(.data,
                     median_var = median(!!var, na.rm = TRUE),
                     q_25 = quantile(!!var, 0.25, na.rm = TRUE, names = FALSE),
                     q_75 = quantile(!!var, 0.75, na.rm = TRUE, names = FALSE),
                     interquartile_deviation = q_75 - q_25,
                     #diff = !!var - median_var,
                     upper = median_var + interquartile_deviation * threshold,
                     lower = median_var - interquartile_deviation * threshold
                     )
  print(tbl)
  outlier_return(.data, var, tbl$upper, tbl$lower)
}

outlier_t_test <- function(.data, var, conf_level) {
  tbl <-
    dplyr::summarise(.data,
                     mean_var = mean(!!var, na.rm = TRUE),
                     upper = purrr::pluck(t.test(!!var, mu = mean_var, conf.level = conf_level), "conf.int", 2),
                     lower = purrr::pluck(t.test(!!var, mu = mean_var, conf.level = conf_level), "conf.int", 1)
    )
  print(tbl)
  outlier_return(.data, var, tbl$upper, tbl$lower)
}





outlier_return <- function(.data, var, upper, lower) {

  rownames(.data)[!mutate(.data, test =  !!var < upper & !!var > lower)$test] |>
    print()

  filtred <-
   # mutate(.data, tet =  !!var < upper & !!var > lower)
    dplyr::filter(.data, !!var < upper, !!var > lower)

  n_removed <- nrow(.data) - nrow(filtred)
  if (n_removed == 0) {
    cli::cli_inform("No outliers were removed")
  }
  else {
    cli::cli_inform(
      glue::glue("removed {n_removed} rows")
    )
  }


  filtred
}


#quo_name(quo(heio))

#mtcars$mpg |>


# mtcars["heii"] = c(rnorm(30), -100,100)
#
# x <-
#   mtcars |> as_tibble() |> filter_outlier(var = heii, method = "IQD")
# x

#tibble::rownames_to_column(x)


#rownames(x)[!x$tet]



#mtcars[mtcars$heii < 76, ]

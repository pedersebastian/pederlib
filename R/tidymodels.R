## Tidymodels:



#' control function
#'
#' @param save_workflow save workflow
#' @param ... passed on to control race
#'
#' @return control_race object
#' @export
#'
#' @examples
#' ctrl(save_workflow = TRUE)
ctrl <- function(save_workflow = FALSE, ...) {
  ctrl <- finetune::control_race(verbose_elim = TRUE, save_pred = T, save_workflow = save_workflow, ...)
  ctrl
}

############################################################


#' Set of for resampling
#'
#' @param data  Data
#' @param strata Strata variable-navn
#' @param resamples Type of resampling method
#' @param number_folds num of resam
#' @param to_clipboard copy to clipboard
#'
#' @return nothing
#' @export
#'
#' @examples
#' use_split(mtcars, mpg, to_clipboard = FALSE)
#' # Returns:
#' # library(tidymodels)
#' #
#' # set.seed(439)
#' # mtcars_split <-
#' # initial_split(mtcars, strata = mpg)
#'
#' # mtcars_train <-
#' #   training(mtcars_split)
#' # mtcars_test <-
#' #   testing(mtcars_split)
#' #
#' #
#' # set.seed(274)
#' # mtcars_folds <-
#' #   bootstraps(mtcars_train, strata = mpg, times = 25)
#'
use_split <- function(data, strata = NULL, resamples = NULL, number_folds = NULL, to_clipboard = TRUE) {
  ok_resamples <- c("vfold", "bootstraps", "bootstrap", "boot", "v_fold", "vfolds", "v_folds")
  vfold_ok <- c("vfold", "vfolds", "v_fold", "v_folds")
  boot <- c("bootstraps", "bootstrap", "boot")




  if (is.null(resamples)) {
    n <- dim(data)
    n <- n[[1]]
  }

  arguments <- match.call()
  arguments <- as.list(arguments)

  df <- as.character(arguments$data)
  strata <- arguments$strata
  number_folds <- arguments$number_folds



  if (is.null(strata)) {
    strata <- "NULL"
  }


  if (!is.null(arguments$resamples)) {
    resamples <- arguments$resamples
  } else if (n <= 1000) {
    resamples <- "bootstraps"
  } else {
    resamples <- "vfold"
  }

  #

  if (!resamples %in% ok_resamples) {
    stop("Currently only support v_fold and bootstraps")
  }

  if (is.null(number_folds)) {
    if (resamples %in% boot) {
      number_folds <- "25"
    } else {
      number_folds <- "10"
    }
  }

  if (stringr::str_detect(as.character(number_folds), "[:alpha:]")) {
    stop("Number of folds must be numeric")
  } else {
    number_folds <- number_folds
  }


  df_name <- stringr::str_remove(df, "\\_.*$")
  df_name <- stringr::str_squish(df_name)
  new_line <- paste0("\n")
  df_split <- paste0(df_name, "_split")




  lib <- paste0("library(tidymodels)\n\n")
  seed_1 <- glue::glue("set.seed({sample.int(1e3, size = 1)})")
  seed_1 <- paste0(seed_1, "\n")
  split <- paste0(df_name, "_split<-\n\tinitial_split(", df, ", strata = ", strata, ")", sep = "")
  train <- paste0(df_name, "_train<-\n\t", "training(", df_split, ")\n", sep = "")
  test <- paste0(df_name, "_test<-\n\t", "testing(", df_split, ")\n\n\n", sep = "")
  seed_2 <- glue::glue("set.seed({sample.int(1e3, size = 1)})")
  seed_2 <- paste0(seed_2, "\n")

  if (resamples %in% boot) {
    folds <-
      paste0(df_name, "_folds<-\n\tbootstraps(", df_name, "_train, strata = ", strata, ", times = ", number_folds, ")\n", sep = "")
  } else {
    folds <-
      paste0(df_name, "_folds<-\n\tvfold_cv(", df_name, "_train, strata = ", strata, ", v = ", number_folds, ")\n", sep = "")
  }

  rs <- paste0(lib,
    seed_1,
    split,
    "\n\n",
    train,
    test,
    seed_2,
    folds,
    sep = "\n"
  )

  if (to_clipboard) {
    clipr::write_clip(rs, object_type = "character")
    cli::cli_alert_success("Code copied to the clipboard!")
  } else {
    cat(rs)
  }

  invisible(rs)
}


#'  prep and bake
#'
#' @param rec unprepped_rec
#' @param new_data  new
#' @param ...  pass
#'
#' @return baked tibble
#' @export
#'
#' @examples
pr_juice <- function(rec, new_data = NULL, ...) {
  recipes::bake(recipes::prep(rec), new_data = NULL, ...)
}


### date

#' Dates
#'
#' @param year add year (default)
#' @param month add month (default)
#' @param week add week (default)
#' @param dow add day of the week (default)
#' @param doy add day of the year (default)
#' @param decimal add decimal
#' @param quarter add quarter
#' @param semester add semester
#'
#' @return vector
#' @export
#'
#' @examples
#' dates()
dates <- function(year = TRUE,
                  month = TRUE,
                  week = TRUE,
                  dow = TRUE,
                  doy = TRUE,
                  decimal = FALSE,
                  quarter = FALSE,
                  semester = FALSE) {
  dates <- vector("character")

  if (year) {
    dates <- append(dates, "year")
  }
  if (month) {
    dates <- append(dates, "month")
  }
  if (week) {
    dates <- append(dates, "week")
  }
  if (dow) {
    dates <- append(dates, "dow")
  }
  if (doy) {
    dates <- append(dates, "doy")
  }
  if (decimal) {
    dates <- append(dates, "decimal")
  }
  if (quarter) {
    dates <- append(dates, "quarter")
  }
  if (semester) {
    dates <- append(dates, "semester")
  }

  return(dates)
}


date_test <- function(...) {
  feat <-
    c(
      "year",
      "doy",
      "week",
      "decimal",
      "semester",
      "quarter",
      "dow",
      "month"
    )
  # dots <-
  # print(dots)
  # return(dots)
  l <- ...length()
  if (!l) {
    ret <-
      c("year", "month", "week", "dow", "doy")
  } else if (purrr::map(rlang::list2(...), is.character) |> purrr::list_c() |> any()) {
    print("HEE")
    return(NULL)
  } else {
    ret <- rlang::quos(...) |>
      purrr::map(rlang::as_label) |>
      purrr::list_c()
  }

  if (!all(ret %in% feat)) {
    rlang::abort(paste0(
      "Possible values of `features` should include: ",
      paste0("'", feat, "'", collapse = ", ")
    ))
  }

  return(ret)
}

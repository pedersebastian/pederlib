##Tidymodels:


########
#Metrics for regression and classification

mset <- function(mode = "C",...) {
  arguments <- match.call()
  arguments <- as.list(arguments)
  if (is.null(arguments$mode)) {
    mode <- "C"
  }
  else{
    mode <- arguments$mode
  }


  mode = stringr::str_to_upper(mode)

  if (!mode %in% c("C", "R")) {
    stop("Mode must be classification or regression")
  }

  if (mode == "R") {
    yardstick::metric_set(yardstick::rmse,
                          yardstick::rsq,
                          yardstick::mae,
                          yardstick::mape,
                          yardstick::mase,
                          ...)
  }
  else{
    yardstick::metric_set(yardstick::roc_auc,
                          yardstick::accuracy,
                          yardstick::sensitivity,
                          yardstick::specificity,
                          yardstick::mn_log_loss,
                          ...)
  }

}
############################################################


# > use_split(mtcars, mpg)

# set.seed(540)
# mtcars_split<-
#   initial_split(mtcars, strata = mpg)
#
# mtcars_train<-
#   training(mtcars_split)
# mtcars_test<-
#   testing(mtcars_split)
#
#
# set.seed(725)
# mtcars_folds<-
#   vfold_cv(mtcars_train, strata = mpg, v = 10)





use_split <- function(data, strata = NULL, resamples = NULL, number_folds =NULL) {

  ok_resamples <- c("vfold", "bootstraps", "bootstrap", "boot", "v_fold", "vfolds", "v_folds")
  vfold_ok <- c("vfold", "vfolds", "v_fold", "v_folds")
  boot <-  c("bootstraps", "bootstrap", "boot")




  if (is.null(resamples)) {
    n = dim(data)
    n= n[[1]]
              }

  arguments <- match.call()
  arguments <- as.list(arguments)

  df <- arguments$data
  strata  <- arguments$strata
  number_folds <- arguments$number_folds



  if (is.null(strata)) {
    strata  <- "NULL"
  }


  if (!is.null(arguments$resamples)) {

    resamples <- arguments$resamples

  }


  else if (n<=1000) {
    resamples <- "bootstraps"
  }

  else{
    resamples <- "vfold"
  }

  #

  if (!resamples %in% ok_resamples) {
    stop("Currently only support v_fold and bootstraps")
  }

  if (is.null(number_folds)) {
    if (resamples %in% boot) {
      number_folds="25"
    }
    else{
      number_folds="10"
    }
  }

if (stringr::str_detect(as.character(number_folds), "[:alpha:]")) {
  stop("Number of folds must be numeric")

}

  else {
    number_folds = number_folds
  }


  df_name <- stringr::str_remove(df, "\\_.*$")
  df_name <- stringr::str_squish(df_name)
  new_line <- paste0("\n")
  df_split <- paste0(df_name, "_split")




  lib <- paste0("library(tidymodels)\n\n")
  seed_1 <- glue::glue("set.seed({sample.int(1e3, size = 1)})")
  seed_1 <- paste0(seed_1,"\n")
  split <- paste0(df_name,"_split<-\n\tinitial_split(", df, ", strata = ", strata, ")", sep = "")
  train <- paste0(df_name,"_train<-\n\t", "training(",df_split,")\n", sep = "")
  test <- paste0(df_name,"_test<-\n\t", "testing(",df_split,")\n\n\n", sep = "")
  seed_2 <- glue::glue("set.seed({sample.int(1e3, size = 1)})")
  seed_2 <- paste0(seed_2,"\n")

  if (resamples %in% boot) {

    folds <-
      paste0(df_name,"_folds<-\n\tbootstraps(", df_name, "_train, strata = ", strata, ", times = ", number_folds,")\n", sep = "")


  }
  else{
    folds <-
      paste0(df_name,"_folds<-\n\tvfold_cv(", df_name, "_train, strata = ", strata, ", v = ", number_folds,")\n", sep = "")

  }

  rs <- paste0(lib,
               seed_1,
               split,
               "\n\n",
               train,
               test,
               seed_2,
               folds,
               sep = "\n")
  cat(rs)
  invisible(NULL)

}





#use_split(ti, strata = y,)

#ti <- tibble(x = 1:10, y = 1:10)


##################################

###  theme_pedr
# *** Based on theme_minimal
# *** Font is `BentonSans Regular`, and must be installed first
# *** title, subtitle are centered

##theme_center
# *** Based on theme_minimal
# *** title, subtitle are centered

##library(ggplot2)
# faithfuld %>%
#    ggplot(aes(density)) + geom_histogram()+ scale_x_continuous(labels = komma()) + theme_pedr()+ labs(title = "This is a title", subtitle = "This is a subtitle")


theme_pedr <- function(base_size = 11,
                       strip_text_size = 12,
                       strip_text_margin = 5,
                       subtitle_size = 13,
                       subtitle_margin = 10,
                       plot_title_size = 16,
                       plot_title_margin = 10,
                       ...) {
  ret <- ggplot2::theme_minimal(base_family = "BentonSans Regular",
                                base_size = base_size, ...)
  ret$strip.text <- ggplot2::element_text(
    hjust = 0, size = strip_text_size,
    margin = ggplot2::margin(b = strip_text_margin),
    family = "BentonSans Regular"
  )
  ret$plot.subtitle <- ggplot2::element_text(
    hjust = 0.5, size = subtitle_size,
    margin = ggplot2::margin(b = subtitle_margin),
    family = "BentonSans Regular",
    color = "gray10"
  )
  ret$plot.title <- ggplot2::element_text(
    hjust = 0.5, size = plot_title_size,
    margin = ggplot2::margin(b = plot_title_margin),
    family = "BentonSans Bold"
  )

  ret
}

#############################################################################
theme_center <- function(base_size = 11,
                         strip_text_size = 12,
                         strip_text_margin = 5,
                         subtitle_size = 13,
                         subtitle_margin = 10,
                         plot_title_size = 16,
                         plot_title_margin = 10,
                         ...) {
  ret <- ggplot2::theme_minimal(base_family = NULL,
                                base_size = base_size, ...)
  ret$strip.text <- ggplot2::element_text(
    hjust = 0, size = strip_text_size,
    margin = ggplot2::margin(b = strip_text_margin),
    family = NULL
  )
  ret$plot.subtitle <- ggplot2::element_text(
    hjust = 0.5, size = subtitle_size,
    margin = ggplot2::margin(b = subtitle_margin),
    family = NULL,
    color = "gray10"
  )
  ret$plot.title <- ggplot2::element_text(
    hjust = 0.5, size = plot_title_size,
    margin = ggplot2::margin(b = plot_title_margin),
    family = NULL
  )

  ret
}

#########################

komma <- function(...) {
  scales::comma_format(decimal.mark = ",", ...)
}

prosent <- function(...) {
  scales::percent_format(decimal.mark = ",", ...)
}



#######Custom metrics

##RMSLE - root mean squared log error
## code from Julia Silge

rmsle_vec <- function(truth, estimate, na_rm = TRUE, ...) {
  rmsle_impl <- function(truth, estimate) {
    sqrt(mean((log(truth + 1) - log(estimate + 1))^2))
  }

  yardstick::metric_vec_template(
    metric_impl = rmsle_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "numeric",
    ...
  )
}

rmsle <- function(data, ...) {
  UseMethod("rmsle")
}
rmsle <- yardstick::new_numeric_metric(rmsle, direction = "minimize")

rmsle.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {
  yardstick::metric_summarizer(
    metric_nm = "rmsle",
    metric_fn = rmsle_vec,
    data = data,
    truth = !!enquo(truth),
    estimate = !!enquo(estimate),
    na_rm = na_rm,
    ...
  )
}
###############

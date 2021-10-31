##Tidymodels:


########
#Metrics for regression and classification

mset <- function(mode = "C", ...) {
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
    metrics <- yardstick::metric_set(...,
                          yardstick::rmse,
                          yardstick::rsq,
                          yardstick::mae,
                          yardstick::mase,
                          )
  }
  else{
    metrics <- yardstick::metric_set(...,
                          yardstick::roc_auc,
                          yardstick::accuracy,
                          yardstick::sensitivity,
                          yardstick::specificity,
                          yardstick::mn_log_loss,
                          )
  }
  metrics

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



#mset("C", first = recall)

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
# Geometric mean of sensitivity and specificity


g_mean_vec <- function(truth,
                       estimate,
                       estimator = NULL,
                       na_rm = TRUE,
                       event_level = "first",
                       ...) {

  estimator <- yardstick::finalize_estimator(truth, estimator, metric_class = "g_mean")

  g_mean_impl <- function(truth, estimate) {
    xtab <- table(estimate, truth)

    g_mean_estimator_impl(xtab, estimator, event_level)
  }

  yardstick::metric_vec_template(
    metric_impl = g_mean_impl,
    truth = truth,
    estimate = estimate,
    na_rm = na_rm,
    cls = "factor",
    estimator = estimator,
    ...
  )
}

# This function switches between binary and multiclass implementations
g_mean_estimator_impl <- function(data, estimator, event_level) {
  if(estimator == "binary") {
    g_mean_binary(data, event_level)
  } else {
    # Encapsulates the macro, macro weighted, and micro cases
    wt <- yardstick::get_weights(data, estimator)
    res <- g_mean_multiclass(data, estimator)
    weighted.mean(res, wt)
  }
}

g_mean_binary <- function(data, event_level) {
  relevant_sens <- yardstick:::pos_val(data, event_level)
  numer_sens <- sum(data[relevant_sens, relevant_sens])
  denom_sens <- sum(data[, relevant_sens])

  sens <- numer_sens/denom_sens

  negative_spec <- yardstick:::neg_val(data, event_level)

  numer_spec <- sum(data[negative_spec, negative_spec])
  denom_spec <- sum(data[, negative_spec])

  spec <- numer_spec/denom_spec

  g_mean  <- sqrt(spec*sens)
  g_mean
}

g_mean_multiclass <- function(data, estimator) {
  numer_sens <- diag(data)
  denom_sens <- colSums(data)
  numer_sens <- sum(numer_sens, na.rm = TRUE)
  denom_sens <- sum(denom_sens, na.rm = TRUE)
  sens <- numer_sens/denom_sens

  n_spec <- sum(data)

  tp_spec   <- diag(data)
  tpfp_spec <- rowSums(data)
  tpfn_spec <- colSums(data)
  tn_spec   <- n_spec - (tpfp_spec + tpfn_spec - tp_spec)
  fp_spec   <- tpfp_spec - tp_spec

  numer_spec <- tn_spec
  denom_spec <- tn_spec + fp_spec

  spec <-   numer_spec/denom_spec


  g_mean  <- sqrt(spec*sens)
  g_mean
}


g_mean <- function(data, ...) {
  UseMethod("g_mean")
}

g_mean <- yardstick::new_class_metric(g_mean, direction = "maximize")

g_mean.data.frame <- function(data,
                              truth,
                              estimate,
                              estimator = NULL,
                              na_rm = TRUE,
                              event_level = "first",
                              ...) {
  yardstick::metric_summarizer(
    metric_nm = "g_mean",
    metric_fn = g_mean_vec,
    data = data,
    truth = !! enquo(truth),
    estimate = !! enquo(estimate),
    estimator = estimator,
    na_rm = na_rm,
    event_level = event_level,
    ...
  )
}


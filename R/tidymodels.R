##Tidymodels:


########
#Metrics for regression and classification

#' Title
#'
#' @param mode Classification of regression (C or R)
#' @param ...  passed on the yardstick::metric_set()
#'
#' @return
#' @export
#'
#' @examples
#'
mset <- function(mode = "C", ...) {
  arguments <- match.call()
  arguments <- as.list(arguments)

  if (is.null(arguments$mode)) {
    mode <- "C"
  }
  else{
    mode <- arguments$mode
  }

  mode  <-  stringr::str_to_upper(mode)


  if (!mode %in% c("C", "R")) {
    rlang::inform(message = "Mode must be classification(C) or regression(R) for standard setup\nYou can still specify your metric using ...\n\n", body = ":)")
    metrics <- yardstick::metric_set(...)
  }

  else if (mode == "R") {
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


#' control function
#'
#' @param save_workflow save workflow
#' @param ... passed on to control race
#'
#' @return
#' @export
#'
#' @examples
#'
ctrl <- function(save_workflow = FALSE,...) {
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
#'
#' @return
#' @export
#'
#' @examples
#'use_split(mtcars, mpg) returns:
#'library(tidymodels)
#'
#'set.seed(439)
#'mtcars_split<-
#'  initial_split(mtcars, strata = mpg)
#'
#'mtcars_train<-
#'  training(mtcars_split)
#'mtcars_test<-
#'  testing(mtcars_split)
#'
#'
#'set.seed(274)
#'mtcars_folds<-
#'  bootstraps(mtcars_train, strata = mpg, times = 25)
#'
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


#' Juice
#'
#' @param prepped_rec a prepped recipe
#' @param new_data new data ?
#' @param ... passed on to bake such as composition
#'
#' @return
#' @export
#'
#' @examples
#'
juice <- function(prepped_rec, new_data = NULL,...) {
  require(recipes)
  recipes::bake(prepped_rec, new_data = new_data,...)
}


## "auto"plot for workflowsets
# TO DO ta med alle, også facets?

#' autoplot for workflowsets
#'
#' @param trained_workflowset  xx
#' @param metric  xx
#' @param n xx
#' @param all_metrics xx
#' @param rank_by_metric xx
#' @param ... xx
#'
#' @return
#' @export
#'
#' @examples
#'
wfs_autoplot <- function(trained_workflowset, metric = "roc_auc", n=1, all_metrics = TRUE, rank_by_metric = FALSE, ...) {

  minimize <- c("mn_log_loss", "rmse", "mae", "mape", "mase")

  p_df  <-
    collect_metrics(trained_workflowset) %>%
    separate(wflow_id, into = c("recipe", "model_type"), sep = "_", remove = F, extra = "merge") %>%
    mutate(metric_name = str_to_upper(.metric),
           metric_name = case_when(metric_name == "ROC_AUC"~"Area Under the ROC-curve (ROC AUC)",
                                   metric_name == "RMSE"~"Root Mean Square Error (RMSE)",
                                   metric_name == "SPEC"~"Specificity",
                                   metric_name == "SENS"~"Sensitivity",
                                   metric_name == "ACCURACY"~"Accuracy",
                                   metric_name == "MN_LOG_LOSS"~"Mean log loss",
                                   metric_name == "RSQ"~"R-Squared",
                                   metric_name == "MAE"~"Mean absolutt error",
                                   metric_name == "MASE"~"Mean absolutt squared error",
                                   TRUE~metric_name),
           metric_name = str_replace_all(metric_name, "\\_", " "))



  p_df_min <- p_df %>%
    filter(.metric %in% minimize)%>%
    group_by(wflow_id,.metric) %>%
    slice_min(order_by = mean, n=n, with_ties = F)%>%
    group_by(.metric) %>%
    arrange(mean)%>%
    group_by(.metric) %>%
    mutate(workflow_rank = row_number(),
           dir = "min")%>%
    ungroup()


  p_df_max <- p_df %>%
    filter(!.metric %in% minimize)%>%
    group_by(wflow_id,.metric) %>%
    slice_max(order_by = mean, n=n, with_ties = F)%>%
    group_by(.metric) %>%
    arrange(-mean)%>%
    mutate(workflow_rank = row_number(),
           dir = "max") %>%
    ungroup()

  if (rank_by_metric==TRUE) {

    p_df_joined <- p_df%>%
      group_by(wflow_id,.metric) %>%
      slice_max(order_by = mean, n=n, with_ties = F)%>%
      group_by(.metric) %>%
      arrange(-mean)%>%
      mutate(workflow_rank = row_number()) %>%
      ungroup()

    x_axis <- glue::glue("Workflowrank based on {metric}")
  }

  else {
    p_df_joined <- bind_rows(p_df_min, p_df_max)

    x_axis <- glue::glue("Workflowrank based on each different metric")
  }




  if (all_metrics) {
    fct_wr <- facet_wrap(vars(metric_name), scales = "free")

    facet <- list(fct_wr)
    y_axis <- list(labs(y = NULL))

  }
  else {
    p_df_joined <-  p_df_joined %>%
      filter(.metric == metric)
    facet <- list()
    y_axis <- list(labs(y = glue::glue("{p_df_joined$metric_name}")))

  }



  p <- p_df_joined %>%
    ggplot(aes(x=workflow_rank, y = mean, shape = recipe)) +
    geom_errorbar(aes(ymin = mean-std_err, ymax = mean+std_err), color = "gray50", lty = 2, width = 0.4) +
    geom_point(size = 3, aes(color = model)) +
    labs(title = "Performance Comparison of Workflow Sets",
         x = x_axis,
         color = "Model Types",
         shape = "Recipes")  +
    scale_x_continuous(breaks  = scales::pretty_breaks(), limits = c(0,NA)) +
    scale_y_continuous(labels = pederlib::komma()) +
    facet +
    y_axis

  return(p)
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
#' @return
#' @export
#'
#' @examples
#'

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

#' root mean square log error
#'
#' @param data data
#' @param ... needs truth and estimate
#'
#' @return
#' @export
#'
#' @examples
#'
rmsle <- function(data, ...) {
  UseMethod("rmsle")
}
rmsle <- yardstick::new_numeric_metric(rmsle, direction = "minimize")

rmsle.data.frame <- function(data, truth, estimate, na_rm = TRUE, ...) {
  yardstick::metric_summarizer(
    metric_nm = "rmsle",
    metric_fn = rmsle_vec,
    data = data,
    truth = !!rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
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


#' Title
#'
#' @param data data
#' @param ... ..
#'
#' @return
#' @export
#'
#' @examples
#'
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
    truth = !! rlang::enquo(truth),
    estimate = !!rlang::enquo(estimate),
    estimator = estimator,
    na_rm = na_rm,
    event_level = event_level,
    ...
  )
}


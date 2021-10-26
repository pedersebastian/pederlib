#******* Contains: ***********

# * theme_pedr
# * theme_center
# * komma  (scales:: object with decimal mark as , )
# * prosent (scales:: object with decimal mark as , )
# * sum_fun (Summary fun into tibble)
# * mode (Finding the mode in a numeric vector)


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

#############################
## This is a summarize function, and return to into a tibble
#
# a <- seq(1,9,2)
# b <- rep(letters)
# c <- rep(c(a,NA),10)
# d <- c(1,2,3,3,20,31)
# e <- c(NA,NA,NA,NA,3,3,2)
# f = c( 5, 10, 17, 24, 30)
# e <- rnorm(1e4)
# sum_fun(a)
# sum_fun(b)
# sum_fun(c, na.rm = TRUE)
# sum_fun(d)
# sum_fun(e, na.rm = TRUE)
# sum_fun(f)
# sum_fun(e)


sum_fun <- function(x, na.rm = FALSE) {
  if(!is.numeric(x)){
    stop("Imput must be numberic")
  }

  if(any(is.na(x))){

    if(na.rm ==FALSE){
      stop("Vector contains NA")

    }
    else {
      x = na.omit(x)
    }
  }

  sum_x = sum(x)
  length_m = length(x)
  mean = sum_x/length_m
  geo = prod(x)^(1/length_m)
  ##Mode
  mode_x <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }

  mode = mode_x(x)

  ##harmonic
  i= 0
  harm = vector(mode ="numeric", length = length_m )
  test = while (i<length_m+1) {
    harm[i] = 1/x[i]
    i = i+1
  }
  h_mean = length_m/sum(harm)

  ###SDV
  i= 0
  sdv = vector(mode ="numeric", length = length_m)
  while (i<length_m+1) {
    sdv[i] = (x[i]- (sum(x)/length_m))^2
    i = i+1
  }

  sorted = sort(x)
  med = ifelse(length_m %% 2==1, sorted[(length_m/2)], mean(sorted[length_m/2+0:1]))

  sd = sqrt(sum(sdv)/(length_m-1))
  se = sd/sqrt(length_m)

  output = dplyr::tibble(mean = mean, geo_mean = geo, harm_mean = h_mean, median = med, standard_deviation= sd, standard_error = se, n=length_m)
  return(output)

}
######function for finding mode in a numeric vector

mode <- function(x, na.rm = FALSE) {
  if(!is.numeric(x)){
    stop("Imput must be numberic")
  }

  if(any(is.na(x))){

    if(na.rm ==FALSE){
      stop("Vector contains NA")

    }
    else {
      x = stats::na.omit(x)
    }



  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

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








use_split <- function(data, strata, resamples = "vfold", n =NULL) {
      ok_resamples <- c("vfold", "bootstraps", "bootstrap", "boot")
      boot <-  c("bootstraps", "bootstrap", "boot")

      data <- NULL
      strata <- NULL
      n <- NULL

  arguments <- match.call()
 arguments <- as.list(arguments)

  df <- arguments$data
  strata  <- arguments$strata
  n <- arguments$n

  if (!is.null(arguments$resamples)) {

    resamples <- arguments$resamples
  }
  else{
    resamples <- "vfold"
  }

  #

  if (!resamples %in% ok_resamples) {
    stop("Does only support v_fold and bootstraps")
  }

  if (is.null(n)) {
    if (resamples %in% boot) {
      n="25"
    }
    else{
      n="10"
    }
  }


  df_name <- stringr::str_remove(df, "df")
  df_name <- stringr::str_squish(df_name)
  new_line <- paste0("\n")




  lib <- paste0("library(tidymodels)\n\n")
  seed_1 <- glue::glue("set.seed({sample.int(1e3, size = 1)})")
  seed_1 <- paste0(seed_1,"\n")
  split <- paste0(df_name,"_split<-\n\tinitial_split(", df, ", strata = ", strata, ")", sep = "")
  train <- paste0(df_name,"_train<-\n\t", "training(",df,"_split)\n", sep = "")
  test <- paste0(df_name,"_test<-\n\t", "testing(",df,"_split)\n\n\n", sep = "")
  seed_2 <- glue::glue("set.seed({sample.int(1e3, size = 1)})")
  seed_2 <- paste0(seed_2,"\n")

  if (resamples %in% boot) {

    folds <-
      paste0(df_name,"_folds<-\n\tbootstraps(", df_name, "_train, strata = ", strata, ", times = ", n,")\n", sep = "")


  }
  else{
    folds <-
      paste0(df_name,"_folds<-\n\tvfold_cv(", df_name, "_train, strata = ", strata, ", v = ", n,")\n", sep = "")

  }

  rs <- paste0(seed_1,split, "\n\n", train,test, seed_2,folds, sep = "\n")
  cat(rs)


}


############################################################
# Some useful keyboard shortcuts for package authoring:    #
#                                                          #
#   Install Package:           'Cmd + Shift + B'           #
#   Check Package:             'Cmd + Shift + E'           #
#   Test Package:              'Cmd + Shift + T'           #
############################################################
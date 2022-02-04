utils::globalVariables(c("%>%" ,".metric" ,"aes", "annotate" ,"arrange", "as_tibble" ,"bind_rows", "case_when",
                         "collect_metrics", "element_text" ,"facet_wrap", "geom_count", "geom_errorbar",
                         "geom_hline" ,"geom_point", "geom_vline", "ggplot", "ggtitle" ,"group_by" ,"labs",
                         "latitude", "longitude", "metric_name", "model", "mutate", "n" ,"new_geo", "pivot_wider",
                         "read_xml" ,"recipe" ,"row_number" ,"scale_x_continuous", "scale_y_continuous",
                         "separate", "separate_rows" ,"slice_max", "slice_min", "std_err", "str_detect",
                         "str_remove_all", "str_replace_all" ,"str_squish", "str_to_upper" ,"theme",
                         "theme_set", "type", "ungroup", "update_geom_defaults", "value", "vars" ,"wflow_id",
                         "workflow_rank" ,"xlab" ,"ylab"))



#S3method(print, myClass)



### extract latitude and longitude for geo_field like GEO3212(31.3112,-88.22)
parse_geo <- function(tbl, geo_variable) {
  suppressPackageStartupMessages(library(tidyverse))
  tbl %>%
    dplyr::mutate(new_geo = stringr::str_remove_all({{geo_variable}},"[:alpha:]" ),
                    new_geo = stringr::str_replace_all(new_geo, "[\r\n]",""),
                    new_geo = stringr::str_replace_all(new_geo,"^.*\\(", ""),
                    longitude = readr::parse_number(new_geo),
                    latitude = stringr::str_remove(new_geo, ".*\\s"),
                    latitude = readr::parse_number(latitude),
                    dplyr::across(c(latitude, longitude),~ifelse(stringr::str_detect({{geo_variable}},"\\(")==FALSE, NA,.)),
                    dplyr::across(c(latitude, longitude), ~ifelse(.==0, NA,.))) %>%

    dplyr::select(-new_geo)
}



###Read csv with encoding from ssb, london etc..

read_csv_europe <- function(file, skip = 0, encoding = "CP1252",...) {
  csv <- vroom::vroom(file, skip = skip, locale = vroom::locale(encoding = encoding), ...)
  csv
}

##########
startup <- function(type = 1, paralell = TRUE) {

  if (type ==1) {
    suppressPackageStartupMessages(library(tidyverse))
    suppressPackageStartupMessages(library(lubridate))
   mes <- "Tidyverse and Lubridate has been loaded."
  }
  else if (type==3) {
    suppressPackageStartupMessages(library(tidyverse))
    suppressPackageStartupMessages(library(lubridate))
    suppressPackageStartupMessages(library(rvest))
    mes <- "Tidyverse, Lubridate and Rvest has been loaded."
  }
  else{
  suppressPackageStartupMessages(library(baguette))
 # suppressPackageStartupMessages(library(discrim))
  suppressPackageStartupMessages(library(tidymodels))
  suppressPackageStartupMessages(library(tidyverse))
  suppressPackageStartupMessages(library(finetune))
  suppressPackageStartupMessages(library(textrecipes))
  suppressPackageStartupMessages(library(stacks))
  suppressPackageStartupMessages(library(lubridate))
  suppressPackageStartupMessages(library(themis))
  mes <- " Tidymodels, Tidyverse, Finetune, Themis, Lubridate, Textrecipes and Stacks has been loaded.\n Baguette, Discrim commented out"
  }

  if (paralell) {
    mes <- paste0(mes, "\n", "Parallel processing has been initiated.")
    doParallel::registerDoParallel(cores = 8)
  }
  suppressPackageStartupMessages(library(pederlib))
  theme_set(theme_center())

  tidyverse::tidyverse_conflicts()
  update_geom_defaults("rect", list(fill="#1d3557", alpha =0.9))
  update_geom_defaults("point", list(color="#1d3557", alpha =0.9))
  mes <- paste0(mes, "\nTheme set to theme_center.\nGeom defaults updated.")
  cat(mes)
  invisible(NULL)
}


########3SITEMAP

sitemap <- function(url) {
  rs <-
    read_xml(url)%>%
    as.character()%>%
    as_tibble()%>%
    separate_rows(value, sep = "\\n")%>%
    filter(str_detect(value, "<loc>|<lastmod>"))%>%
    mutate(value = str_squish(value),
           type = ifelse(str_detect(value, "^<loc>"), "link", "last_mod"))%>%
    group_by(type)%>%
    mutate(id = row_number(),
           value = str_remove_all(value, "<loc>|</loc>|<lastmod>|</lastmod>"))%>%
    ungroup()%>%
    pivot_wider(names_from = type, values_from = value)
  return(rs)
}


###### prosent

pct <- function(tbl) {
  tbl %>%
    mutate(pct = n/sum(n, na.rm = TRUE))
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
      stop("Vector contains NA, reconsider")

    }
    else {
      x = stats::na.omit(x)
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

mode_vec <- function(x, na.rm = FALSE) {
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




############################################################
# Some useful keyboard shortcuts for package authoring:    #
#                                                          #
#   Install Package:           'Cmd + Shift + B'           #
#   Check Package:             'Cmd + Shift + E'           #
#   Test Package:              'Cmd + Shift + T'           #
############################################################

#helpers stolen from yardstick
neg_val2 <- function (xtab, event_level)
{
  if (!all(dim(xtab) == 2)) {
    rlang::abort("Only relevant for 2x2 tables")
  }
  if (is_event_first(event_level)) {
    colnames(xtab)[[2]]
  }
  else {
    colnames(xtab)[[1]]
  }
}

pos_val2 <- function (xtab, event_level)
{
  if (!all(dim(xtab) == 2)) {
    rlang::abort("Only relevant for 2x2 tables")
  }
  if (is_event_first(event_level)) {
    colnames(xtab)[[1]]
  }
  else {
    colnames(xtab)[[2]]
  }
}

is_event_first <- function (event_level)
{
  validate_event_level(event_level)
  identical(event_level, "first")
}

validate_event_level <- function (event_level)
{
  if (identical(event_level, "first")) {
    return(invisible())
  }
  if (identical(event_level, "second")) {
    return(invisible())
  }
  rlang::abort("`event_level` must be 'first' or 'second'.")
}

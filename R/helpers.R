#helpers
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

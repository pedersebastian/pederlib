#' Rep_n
#'
#' @param tbl a tbl or contingency table
#' @param count_col collum where the frequency is
#'
#' @return a tbl
#' @export
count_to_cases <- function(tbl, count_col = n) {
  count_col <- rlang::quo_name(rlang::enquo(count_col))

  # Get the row indices to pull from x
  idx <- rep.int(seq_len(nrow(tbl)), tbl[[count_col]])

  # Drop count column
  tbl[[count_col]] <- NULL

  # Get the rows from x
  tbl[idx, ]
}
#' @rdname count_to_cases
#' @export
rep_n <- count_to_cases

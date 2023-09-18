#' Check t and lambda
#'
#' @param t time interval starting points
#' @param lambda hazards
#'
#' @details
#' check if t and lambda have valid and compatible size, sign and order
#'
#' @return TRUE invisibly
check_t_lambda <- function(t, lambda){
  checkmate::assert_numeric(t, sorted = TRUE, unique=TRUE)
  checkmate::assert_numeric(lambda, lower=0)
  stopifnot(length(lambda) == length(t))
  invisible(TRUE)
}

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
#' @useDynLib miniPCH, .registration = TRUE
## usethis namespace: end
#' @importFrom stats runif uniroot
#' @importFrom graphics abline lines par points
NULL

# declaring varibles to avoid R CMD check notes.
# those are mostly column names that occur in with, within, subset functions,
# dplyr verbs and ggplot calls.
globalVariables(c(
  "jump", "y"
))

# Check t and lambda
#
# @param t time interval starting points
# @param lambda hazards
#
# @details
# check if t and lambda have valid and compatible size, sign and order
#
# @return TRUE invisibly
check_t_lambda <- function(t, lambda){
  checkmate::assert_numeric(t, sorted = TRUE, unique=TRUE)
  checkmate::assert_numeric(lambda, lower=0)
  stopifnot(length(lambda) == length(t))
  invisible(TRUE)
}

# Check parameters for multistate functions
#
# @param t time interval starting points
# @param Q matrix of the continuous time markov chain
# @param pi starting distribution
# @param abs vector indicating which states are absorbing events of interest
#
# @details
# check if all objects have compatible size, sign and order
# check if pi is a distribution, abs is an indicator vector and Q is a Q matrix
# check if abs corresponds to absorbing sets in each timestep
#
# @return TRUE invisibly
check_t_Q_pi_abs <- function(t, Q, pi, abs){
  # t sorted, t and Q of compatible size?
  checkmate::assert_numeric(t, sorted = TRUE, unique=TRUE)
  stopifnot(dim(Q)[3] == length(t))

  # pi, abs, Q of compatible size?
  stopifnot(length(pi) == dim(Q)[1])
  stopifnot(length(pi) == dim(Q)[2])
  stopifnot(length(pi) == length(abs))

  # abs an indicator vector?
  checkmate::assert_integerish(abs)
  checkmate::assert_subset(as.integer(abs), c(0L, 1L))
  # at least one event type of interest?
  checkmate::assert_number(sum(abs), lower = 0.5)

  # pi a distribution?
  checkmate::assert_number(sum(pi), lower=1-.Machine$double.eps, upper=1+.Machine$double.eps)
  checkmate::assert_numeric(pi, lower=-.Machine$double.eps)

  # every slice of Q a Q matrix?
  checkmate::assert_numeric(apply(Q, 3, rowSums), lower=-.Machine$double.eps, upper=.Machine$double.eps)
  checkmate::assert_numeric(apply(Q, 3, diag), upper=.Machine$double.eps)
  checkmate::assert_numeric(apply(Q, 3, \(x){diag(x) <- 0; x}), lower=-.Machine$double.eps)

  # check if abs corresponds to set of absorbing states
  if( any(apply(Q, 3, \(Q_){
    # starting in abs can we reach a state outside of abs?
    (abs %*% Q_ %*% abs) != 0
  })) ){
    stop('"abs" does not correspond to absorbing states of "Q" in each time interval')
  }

  invisible(TRUE)
}

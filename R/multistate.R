#' Survival Distributions with piece-wise constant hazards and multiple states
#'
#' @description
#' Densitiy, distribution function, quantiles, random numbers, hazard function,
#' cumulative hazard function and survival function of multi-state survival
#' functions.
#'
#' @describeIn mstate density of survival distributions for a piece-wise exponential multi-state model
#'
#' @param x vector of quantiles
#' @param t vector of left interval borders
#' @param Q Q-matrices of the process, see details
#' @param pi initial distribution
#' @param abs indicator vector of absorbing states, see details
#'
#' @details
#' `Q` is an array of dimensions N x N x M where M is the number of time intervals
#' and N is the number of states. Every slice of Q along the third dimension is
#' an N x N Q-matrix. Each row of the Q-matrix contains the hazard-rates for
#' transitioning from the respective state to each other state in the
#' off-diagonal elements. The diagonal element is minus the sum of the other
#' elements.
#'
#' `abs` is a vector that is 1 for each absorbing state that corresponds to an
#' event of interest and 0 everywhere else.
#'
#' The densities, distribution functions, etc. now correspond to the event of
#' entering one of the absorbing states when the initial distribution in the
#' states is `pi`.
#'
#' @return `dmstate` gives the density evaluated at `x`.
#' @export
#'
#' @examples
#' Tint <- 0
#' Q <- matrix(
#'   c(
#'     -0.1, 0.1,
#'     0  , 0
#'   ), 2, 2, byrow = TRUE
#' )
#' dim(Q) <- c(2,2,1)
#' pi <- c(1,0)
#' abs <- c(0,1)
#'
#' t <- 0:100
#'
#' par(mfrow=c(3,2))
#' plot.new()
#' text(0.5,0.5,"example 1 proportional hazards")
#' plot(t, pmstate(t, Tint, Q, pi, abs), type="l")
#' plot(t, smstate(t, Tint, Q, pi, abs), type="l")
#' plot(t, dmstate(t, Tint, Q, pi, abs), type="l")
#' plot(t, hmstate(t, Tint, Q, pi, abs), type="l", ylim=c(0,1))
#' plot(t, chmstate(t, Tint, Q, pi, abs), type="l")
#'
#' Tint <- 0
#' Q <- matrix(
#'   c(
#'     -0.3, 0.2, 0.1,
#'     0  ,-0.4, 0.4,
#'     0  ,   0,   0
#'   ), 3, 3, byrow = TRUE
#' )
#' dim(Q) <- c(3,3,1)
#' pi <- c(1,0,0)
#' abs <- c(0,0,1)
#'
#' t <- seq(0,20, by=0.1)
#'
#' par(mfrow=c(3,2))
#' plot.new()
#' text(0.5,0.5,"example 2 disease progression")
#' plot(t, pmstate(t, Tint, Q, pi, abs), type="l")
#' plot(t, smstate(t, Tint, Q, pi, abs), type="l")
#' plot(t, dmstate(t, Tint, Q, pi, abs), type="l")
#' plot(t, hmstate(t, Tint, Q, pi, abs), type="l", ylim=c(0,1))
#' plot(t, chmstate(t, Tint, Q, pi, abs), type="l")
#'
#' Tint <- 0
#' Q <- matrix(
#'   c(
#'     -0.4, 0  , 0.4,
#'     0  ,-0.1, 0.1,
#'     0  ,   0,   0
#'   ), 3, 3, byrow = TRUE
#' )
#' dim(Q) <- c(3,3,1)
#' pi <- c(0.5,0.5,0)
#' abs <- c(0,0,1)
#'
#' t <- seq(0, 40, by=0.1)
#'
#' par(mfrow=c(3,2))
#' plot.new()
#' text(0.5,0.5,"example 3 subgroups")
#' plot(t, pmstate(t, Tint, Q, pi, abs), type="l")
#' plot(t, smstate(t, Tint, Q, pi, abs), type="l")
#' plot(t, dmstate(t, Tint, Q, pi, abs), type="l")
#' plot(t, hmstate(t, Tint, Q, pi, abs), type="l", ylim=c(0,1))
#' plot(t, chmstate(t, Tint, Q, pi, abs), type="l")
#'
#'
#' Tint <- c(0,20)
#' Q <- array(NA_real_, dim=c(3,3,2))
#' Q[,,1] <- matrix(
#'   c(
#'     -0.2, 0   , 0.2 ,
#'     0  ,-0.05, 0.05,
#'     0  ,    0, 0
#'   ), 3, 3, byrow = TRUE
#' )
#' Q[,,2] <- matrix(
#'   c(
#'     -0.05, 0   , 0.05 ,
#'     0  ,-0.05, 0.05,
#'     0  ,    0, 0
#'   ), 3, 3, byrow = TRUE
#' )
#'
#' pi <- c(0.75,0.25,0)
#' abs <- c(0,0,1)
#'
#' t <- seq(0, 100, by=0.1)
#'
#' par(mfrow=c(3,2))
#' plot.new()
#' text(0.5,0.5,"example 4\ndelayed effect in one group\nimmediate effect in the other")
#' plot(t, pmstate(t, Tint, Q, pi, abs), type="l")
#' plot(t, smstate(t, Tint, Q, pi, abs), type="l")
#' plot(t, dmstate(t, Tint, Q, pi, abs), type="l")
#' plot(t, hmstate(t, Tint, Q, pi, abs), type="l", ylim=c(0,0.2))
#' plot(t, chmstate(t, Tint, Q, pi, abs), type="l")
dmstate <- function(x, t, Q, pi, abs){
  check_t_Q_pi_abs(t, Q, pi, abs)
  .Call("_miniPCH_pdfFunCpp_multistate", PACKAGE="miniPCH", t, Q, pi, abs, x)
}

#' @describeIn mstate distribution function of survival distributions for a piece-wise exponential multi-state model
#'
#' @return `pmstate` gives the distribution function evaluated at `q`.
#' @export
pmstate <- function(q, t, Q, pi, abs){
  check_t_Q_pi_abs(t, Q, pi, abs)
  .Call("_miniPCH_cdfFunCpp_multistate", PACKAGE="miniPCH", t, Q, pi, abs, q)
}

#' @describeIn mstate hazard of survival distributions for a piece-wise exponential multi-state model
#'
#' @return `hmstate` gives the hazard function evaluated at `x`.
#' @export
hmstate <- function(x, t, Q, pi, abs){
  check_t_Q_pi_abs(t, Q, pi, abs)
  .Call("_miniPCH_hazFunCpp_multistate", PACKAGE="miniPCH", t, Q, pi, abs, x)
}

#' @describeIn mstate cumulative hazard of survival distributions for a piece-wise exponential multi-state model
#'
#' @return `chmstate` gives the cumulative hazard function evaluated at `x`.
#' @export
chmstate <- function(x, t, Q, pi, abs){
  check_t_Q_pi_abs(t, Q, pi, abs)
  .Call("_miniPCH_cumhazFunCpp_multistate", PACKAGE="miniPCH", t, Q, pi, abs, x)
}

#' @describeIn mstate survival function of survival distributions for a piece-wise exponential multi-state model
#'
#' @return `smstate` gives the survival function evaluated at `q`.
#' @export
smstate <- function(q, t, Q, pi, abs){
  check_t_Q_pi_abs(t, Q, pi, abs)
  .Call("_miniPCH_survFunCpp_multistate", PACKAGE="miniPCH", t, Q, pi, abs, q)
}

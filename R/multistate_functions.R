#' Survival Distributions with piece-wise Constant Hazards and multiple states (function factories)
#'
#' @describeIn multistate_functions density of survival distributions with piece-wise constant hazards and multiple states
#'
#' @description
#' Densitiy, distribution function, hazard function, cumulative hazard function
#' and survival function of multi-state survival functions.
#'
#' Those functions return functions of one parameter that can be evaluated to
#' give the density, distribution function, ... The parameters `t`, `Q`, `pi`
#' and `abs` are checked only once and not at every function evaluation.
#'
#' @seealso [dmstate]
#'
#' @param t vector of left interval borders
#' @param Q Q-matrices of the process, see details
#' @param pi initial distribution
#' @param abs indicator vector of absorbing states, see details
#'
#' @return `dmstate_fun` gives the density.
#' @export
#'
#' @examples
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
#' my_density           <-  dmstate_fun(Tint, Q, pi, abs)
#' my_distribution      <-  pmstate_fun(Tint, Q, pi, abs)
#' my_hazard            <-  hmstate_fun(Tint, Q, pi, abs)
#' my_cumulative_hazard <- chmstate_fun(Tint, Q, pi, abs)
#' my_survival          <-  smstate_fun(Tint, Q, pi, abs)
#'
#' t <- seq(0,20, by=0.1)
#' par(mfrow=c(3,2))
#' plot(t, my_density(t), type="l")
#' plot(t, my_distribution(t), type="l")
#' plot(t, my_hazard(t), type="l", ylim=c(0,1))
#' plot(t, my_cumulative_hazard(t), type="l")
#' plot(t, my_survival(t), type="l")
dmstate_fun <- function(t, Q, pi, abs){
  check_t_Q_pi_abs(t, Q, pi, abs)
  function(x){
    .Call("_miniPCH_pdfFunCpp_multistate", PACKAGE="miniPCH", t, Q, pi, abs, x)
  }
}

#' @describeIn multistate_functions distribution function of survival distributions with piece-wise constant hazards and multiple states
#'
#' @return `pmstate_fun` gives the distribution function
#' @export
pmstate_fun <- function(t, Q, pi, abs){
  check_t_Q_pi_abs(t, Q, pi, abs)
  function(q){
    .Call("_miniPCH_cdfFunCpp_multistate", PACKAGE="miniPCH", t, Q, pi, abs, q)
  }
}

#' @describeIn multistate_functions hazard function of survival distributions with piece-wise constant hazards and multiple states
#'
#' @return `hmstate_fun` gives the hazard function.
#' @export
hmstate_fun <- function(t, Q, pi, abs){
  check_t_Q_pi_abs(t, Q, pi, abs)
  function(x){
    .Call("_miniPCH_hazFunCpp_multistate", PACKAGE="miniPCH", t, Q, pi, abs, x)
  }
}


#' @describeIn multistate_functions cumulative hazard function of survival distributions with piece-wise constant hazards and multiple states
#'
#' @return `chmstate_fun` gives the cumulative hazard function.
#' @export
chmstate_fun <- function(t, Q, pi, abs){
  check_t_Q_pi_abs(t, Q, pi, abs)
  function(x){
    .Call("_miniPCH_cumhazFunCpp_multistate", PACKAGE="miniPCH", t, Q, pi, abs, x)
  }
}


#' @describeIn multistate_functions survival function of survival distributions with piece-wise constant hazards and multiple states
#'
#' @return `smstate_fun` gives the survival function.
#' @export
smstate_fun <- function(t, Q, pi, abs){
  check_t_Q_pi_abs(t, Q, pi, abs)
  function(q){
    .Call("_miniPCH_survFunCpp_multistate", PACKAGE="miniPCH", t, Q, pi, abs, q)
  }
}

#' Object with all functions of a survival distribution with piece-wise constant hazards and multiple states
#'
#' @param t vector of left interval borders
#' @param Q Q-matrices of the process, see details
#' @param pi initial distribution
#' @param abs indicator vector of absorbing states, see details
#'
#' @return `multistate_functions` gives an object of class "miniPCH"
#' @export
#'
#' @examples
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
#' my_obj <- multistate_functions(Tint, Q, pi, abs)
#' t <- seq(0,20, by=0.1)
#' plot(t, my_obj$d(t), type="l")
multistate_functions <- function(t, Q, pi, abs){
  res <- list(
    d  =  dmstate_fun(t, Q, pi, abs),
    p  =  pmstate_fun(t, Q, pi, abs),
    h  =  hmstate_fun(t, Q, pi, abs),
    ch = chmstate_fun(t, Q, pi, abs),
    s  =  smstate_fun(t, Q, pi, abs),
    t = t,
    Q = Q,
    pi = pi,
    abs = abs
  )
  class(res) <- c("miniPCH")
  res
}


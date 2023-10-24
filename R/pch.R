#' Survival Distributions with piece-wise Constant Hazards
#'
#' @description
#' Densitiy, distribution function, quantiles, random numbers, hazard function,
#' cumulative hazard function and survival function of survival distributions
#' with piece-wise constant hazards (picewise exponential distributions).
#'
#' @describeIn pch density of survival distributions with piece-wise constant hazards
#'
#' @param x vector of quantiles
#' @param t vector of left interval borders
#' @param lambda vector of hazards
#'
#' @return `dpch` gives the density evaluated at `x`.
#' @export
#'
#' @examples
#' dpch(1:10, c(0, 3), c(2, 0.1))
dpch <- function(x, t, lambda){
  check_t_lambda(t, lambda)
  .Call("_miniPCH_pdfFunCpp", PACKAGE="miniPCH", t, lambda, x)
}

#' @describeIn pch distribution function of survival distributions with piece-wise constant hazards
#'
#' @param q vector of quantiles
#' @param t vector of left interval borders
#' @param lambda vector of hazards
#'
#' @return `ppch` gives the distribution function evaluated at `q`.
#' @export
#'
#' @examples
#' ppch(1:10, c(0, 3), c(2, 0.1))
ppch <- function(q, t, lambda){
  check_t_lambda(t, lambda)
  .Call("_miniPCH_cdfFunCpp", PACKAGE="miniPCH", t, lambda, q)
}

#' @describeIn pch quantiles of survival distributions with piece-wise constant hazards
#'
#' @param p vector of probabilities
#' @param t vector of left interval borders
#' @param lambda vector of hazards
#'
#' @return `qpch` gives the `p`-quantiles.
#' @export
#'
#' @examples
#' qpch(seq(0,1, by=0.1), c(0, 3), c(2, 0.1))
qpch <- function(p, t, lambda){
  check_t_lambda(t, lambda)
  checkmate::assert_numeric(p, lower=0, upper=1)
  .Call("_miniPCH_quantFunCpp", PACKAGE="miniPCH", t, lambda, p)
}

#' @describeIn pch random samples of survival distributions with piece-wise constant hazards
#'
#' @param n number of random numbers
#' @param t vector of left interval borders
#' @param lambda vector of hazards
#' @param discrete round survival times to whole numbers
#'
#' @return `rpch` gives `n` random numbers.
#' @export
#'
#' @examples
#' rpch(15, c(0, 3), c(2, 0.1))
#' rpch(15, c(0, 3), c(2, 0.1), discrete=TRUE)
rpch <- function(n, t, lambda, discrete=FALSE){
  check_t_lambda(t, lambda)
  if(discrete){
    floor(.Call("_miniPCH_quantFunCpp", PACKAGE="miniPCH", t, lambda, runif(n)) + 1)
  } else {
    .Call("_miniPCH_quantFunCpp", PACKAGE="miniPCH", t, lambda, runif(n))
  }
}
#' @describeIn pch hazard of survival distributions with piece-wise constant hazards
#'
#' @param x vector of quantiles
#' @param t vector of left interval borders
#' @param lambda vector of hazards
#'
#' @return `hpch` gives the hazard function evaluated at `x`.
#' @export
#'
#' @examples
#' hpch(1:10, c(0, 3), c(2, 0.1))
hpch <- function(x, t, lambda){
  check_t_lambda(t, lambda)
  .Call("_miniPCH_hazFunCpp", PACKAGE="miniPCH", t, lambda, x)
}

#' @describeIn pch cumulative hazard of survival distributions with piece-wise constant hazards
#'
#' @param x vector of quantiles
#' @param t vector of left interval borders
#' @param lambda vector of hazards
#'
#' @return `chpch` gives the cumulative hazard function evaluated at `x`.
#' @export
#'
#' @examples
#' chpch(1:10, c(0, 3), c(2, 0.1))
chpch <- function(x, t, lambda){
  check_t_lambda(t, lambda)
  .Call("_miniPCH_cumhazFunCpp", PACKAGE="miniPCH", t, lambda, x)
}

#' @describeIn pch survival function of survival distributions with piece-wise constant hazards
#'
#' @param q vector of quantiles
#' @param t vector of left interval borders
#' @param lambda vector of hazards
#'
#' @return `spch` gives the survival function evaluated at `q`.
#' @export
#'
#' @examples
#' ppch(1:10, c(0, 3), c(2, 0.1))
spch <- function(q, t, lambda){
  check_t_lambda(t, lambda)
  .Call("_miniPCH_survFunCpp", PACKAGE="miniPCH", t, lambda, q)
}

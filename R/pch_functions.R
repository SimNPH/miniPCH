#' Survival Distributions with piece-wise Constant Hazards (function factories)
#'
#' @description
#' Densitiy, distribution function, quantiles, random numbers, hazard function,
#' cumulative hazard function and survival function of survival distributions
#' with piece-wise constant hazards (picewise exponential distributions).
#'
#' Those functions return functions of one parameter that can be evaluated to
#' give the density, distribution function, ... The parameters `t` and `lambda`
#' are checked only once and not at every function evaluation.
#'
#' @describeIn pch_functions density of survival distributions with piece-wise constant hazards
#' @seealso [pch]
#'
#' @param t vector of left interval borders
#' @param lambda vector of hazards
#'
#' @return `dpch_fun` gives the density.
#' @export
#'
#' @examples
#' pch_density <- dpch_fun(c(0, 3), c(2, 0.1))
#' pch_density(1:10)
dpch_fun <- function(t, lambda){
  check_t_lambda(t, lambda)
  function(x){
    .Call("_miniPCH_pdfFunCpp", PACKAGE="miniPCH", t, lambda, x)
  }
}

#' @describeIn pch_functions distribution function of survival distributions with piece-wise constant hazards
#' @seealso [pch]
#'
#' @param t vector of left interval borders
#' @param lambda vector of hazards
#'
#' @return `ppch_fun` gives the distribution function
#' @export
#'
#' @examples
#' pch_distr <- ppch_fun(c(0, 3), c(2, 0.1))
#' pch_distr(1:10)
ppch_fun <- function(t, lambda){
  check_t_lambda(t, lambda)
  function(q){
    .Call("_miniPCH_cdfFunCpp", PACKAGE="miniPCH", t, lambda, q)
  }
}

#' @describeIn pch_functions quantile function of survival distributions with piece-wise constant hazards
#' @seealso [pch]
#'
#' @param t vector of left interval borders
#' @param lambda vector of hazards
#'
#' @return `qpch_fun` gives the quantile function.
#' @export
#'
#' @examples
#' pch_quant <- qpch_fun(c(0, 3), c(2, 0.1))
#' pch_quant(seq(0,1, by=0.1))
qpch_fun <- function(t, lambda){
  check_t_lambda(t, lambda)
  function(p){
    checkmate::assert_numeric(p, lower=0, upper=1)
    .Call("_miniPCH_quantFunCpp", PACKAGE="miniPCH", t, lambda, p)
  }
}

#' @describeIn pch_functions RNG function of survival distributions with piece-wise constant hazards
#' @seealso [pch]
#'
#' @param t vector of left interval borders
#' @param lambda vector of hazards
#' @param discrete round survival times to whole numbers
#'
#' @return `rpch_fun` gives a function to sample from the given distribution.
#' @export
#'
#' @examples
#' rpch_fun_cont  <- rpch_fun(c(0, 3), c(2, 0.1))
#' rpch_fun_discr <- rpch_fun(c(0, 3), c(2, 0.1), discrete=TRUE)
#' rpch_fun_cont(15)
#' rpch_fun_discr(15)
rpch_fun <- function(t, lambda, discrete=FALSE){
  check_t_lambda(t, lambda)
  if(discrete){
    function(n){
      floor(.Call("_miniPCH_quantFunCpp", PACKAGE="miniPCH", t, lambda, runif(n)) + 1)
    }
  } else {
    function(n){
      .Call("_miniPCH_quantFunCpp", PACKAGE="miniPCH", t, lambda, runif(n))
    }
  }
}

#' @describeIn pch_functions hazard function of survival distributions with piece-wise constant hazards
#' @seealso [pch]
#'
#' @param t vector of left interval borders
#' @param lambda vector of hazards
#'
#' @return `hpch_fun` gives the hazard function.
#' @export
#'
#' @examples
#' pch_haz <- hpch_fun(c(0, 3), c(2, 0.1))
#' pch_haz(1:10)
hpch_fun <- function(t, lambda){
  check_t_lambda(t, lambda)
  function(x){
    .Call("_miniPCH_hazFunCpp", PACKAGE="miniPCH", t, lambda, x)
  }
}

#' @describeIn pch_functions cumulative hazard function of survival distributions with piece-wise constant hazards
#' @seealso [pch]
#'
#' @param t vector of left interval borders
#' @param lambda vector of hazards
#'
#' @return `chpch_fun` gives the cumulative hazard function.
#' @export
#'
#' @examples
#' pch_cumhaz <- chpch_fun(c(0, 3), c(2, 0.1))
#' pch_cumhaz(1:10)
chpch_fun <- function(t, lambda){
  check_t_lambda(t, lambda)
  function(x){
    .Call("_miniPCH_cumhazFunCpp", PACKAGE="miniPCH", t, lambda, x)
  }
}

#' @describeIn pch_functions survival function of survival distributions with piece-wise constant hazards
#' @seealso [pch]
#'
#' @param t vector of left interval borders
#' @param lambda vector of hazards
#'
#' @return `spch_fun` gives the survival function.
#' @export
#'
#' @examples
#' pch_surv <- spch_fun(c(0, 3), c(2, 0.1))
#' pch_surv(1:10)
spch_fun <- function(t, lambda){
  check_t_lambda(t, lambda)
  function(q){
    .Call("_miniPCH_survFunCpp", PACKAGE="miniPCH", t, lambda, q)
  }
}

#' Object with all functions of a survival distribution with piece-wise constant hazards
#'
#' @param t vector of left interval borders
#' @param lambda vector of hazards
#' @param discrete round survival times to whole numbers in RNG
#'
#' @return an object of class "miniPCH"
#' @export
#'
#' @examples
#' my_pch <- pch_functions(c(0, 3), c(2, 0.1))#
#' my_pch$t
#' my_pch$r(15)
#' my_pch$ch(1:10)
pch_functions <- function(t, lambda, discrete=FALSE){
  res <- list(
    d  = dpch_fun(t, lambda),
    p  = ppch_fun(t, lambda),
    q  = qpch_fun(t, lambda),
    r  = rpch_fun(t, lambda, discrete),
    h  = hpch_fun(t, lambda),
    ch = chpch_fun(t, lambda),
    s  = spch_fun(t, lambda),
    t = t,
    lambda = lambda,
    discrete = discrete
  )
  class(res) <- "miniPCH"
  res
}

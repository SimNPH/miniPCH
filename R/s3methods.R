#' miniPCH class
#'
#' @param obj miniPCH object
#' @param what what to plot ("d", "p", "q", "h", "ch", "s")
#' @param from lower x-Axis limit
#' @param to upper x-Axis limit
#' @param mfrow plot layout defaults to all plots in one row
#' @param n number of points for interpolation
#' @param ... passed on to base::plot
#'
#' @details
#'
#' @export
#'
#' The layout uses the mfrow argument to par and defaults to all plots in one
#' row. The layout can be overwritten by passing the mfrow argument, that is
#' passed as is to an internal call to par.
#'
#' @return
#' NULL, invisibly
#'
#' @examples
#' my_pch <- pch_functions(c(0, 3), c(2, 0.1))
#' plot(my_pch)
#'
#' Tint <- c(0,3)
#' Q <- array(
#'   c(
#'     -0.3,  0  , 0,
#'     0.2, -0.4, 0,
#'     0.1,  0.4, 0,
#'
#'     -0.3,  0  , 0,
#'     0.2, -0.2, 0,
#'     0.1,  0.2, 0
#'   ), dim=c(3,3,2)
#' )
#'
#' pi <- c(1,0,0)
#' abs <- c(0,0,1)
#'
#' my_obj <- multistate_functions(Tint, Q, pi, abs)
#' plot(my_obj)
plot.miniPCH <- function(obj, what=c("d", "s", "h"), from, to, mfrow=c(1,length(what)), n=1001, ...){
  if(!all(what %in% c("d", "p", "q", "h", "ch", "s"))){
    stop(gettext("Argument what has to be a character vector containing only d, p, q, h, ch, s, to plot density, cdf, quantiles, hazard, cumulative hazard or survival function"))
  }
  if(("q" %in% what) & (!("q" %in% names(obj)))){
    stop(gettext("Plotting quantiles is not supported for this object."))
  }

  # heuristic to set plot interval
  # start at lowest time interval
  if(missing(from)){
    from <- min(obj$t)
  }
  # plot to 90% percentile or 1.5 times the range of given time points,
  # whichever is later
  if(missing(to)){
    if("q" %in% names(obj)){
      q <- obj$q(0.9)
    } else {
      q <- uniroot(\(t){obj$s(t)-0.1}, lower=0, upper=1, extendInt = "downX")$root
    }
    t_max <- max(obj$t) + 0.5*(max(obj$t)-min(obj$t))
    to <- max(q, t_max)
  }

  par(mfrow=mfrow)

  x <- seq(from, to, length.out=n)
  for(i in what){
    if(i %in% c("d", "h")){
      plot_cadlag(x, obj[[i]], obj$t, xlab="t", ylab=i, ...)
    } else {
      plot(x, obj[[i]](x), xlab="t", ylab=i,  type="l", ...)
    }
    abline(v=obj$t, col="grey")
  }

  invisible(NULL)
}


plot_cadlag <- function(x, fun, jumps, ...){
  jumps <- c(jumps, Inf) |>
    sort() |>
    unique()

  plot(x, fun(x), type="n", ...)
  Map(\(i){
    tmp_x <- c(jumps[i], x[(x > jumps[i]) & (x < jumps[i+1])])
    lines(tmp_x, fun(tmp_x))
    points(jumps[i], fun(jumps[i]), pch=18)
    if(i != 1L){
      points(jumps[i], fun(jumps[i]-2*.Machine$double.eps), pch=1)
    }
  }, 1:(length(jumps)-1))

  invisible(NULL)
}

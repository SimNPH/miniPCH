#' miniPCH class
#'
#' @param obj miniPCH object
#' @param what what to plot ("d", "p", "q", "h", "ch", "s")
#' @param from lower x-Axis limit
#' @param to upper x-Axis limit
#' @param ... passed on to base::curve
#'
#' @return
#'
#' @examples
plot.miniPCH <- function(obj, what=c("d", "s", "h"), from, to, ...){
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

  par(mfrow=c(1,length(what)))
  for(i in what){
    curve(obj[[i]](x), from=from, to=to, xlab="t", ylab=i, ...)
    abline(v=obj$t, col="grey")
  }
}


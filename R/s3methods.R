#' miniPCH class
#'
#' @param x miniPCH object
#' @param object miniPCH object
#' @param what what to plot ("d", "p", "q", "h", "ch", "s")
#' @param from lower x-Axis limit
#' @param to upper x-Axis limit
#' @param mfrow plot layout defaults to all plots in one row
#' @param n number of points for interpolation
#' @param ... passed on to base::plot
#'
#' @export
#'
#' @rdname miniPCH.class
#'
#' @details
#' The layout in print uses the mfrow argument to par and defaults to all plots
#' in one row. The layout can be overwritten by passing the mfrow argument, that
#' is passed as is to an internal call to par.
#'
#' @return
#' for plot: NULL, invisibly
#'
#' @examples
#' my_pch <- pch_functions(c(0, 3), c(2, 0.1))
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
#'
#' plot(my_pch)
#' summary(my_pch)
#' print(my_pch)
#'
#' plot(my_obj)
#' summary(my_obj)
#' print(my_obj)
#'
#' library(ggplot2)
#' autoplot(my_pch)
#' autoplot(my_obj)
plot.miniPCH <- function(x, ...,what=c("d", "s", "h"), from, to, mfrow=c(1,length(what)), n=1001){
  if(!all(what %in% c("d", "p", "q", "h", "ch", "s"))){
    stop(gettext("Argument what has to be a character vector containing only d, p, q, h, ch, s, to plot density, cdf, quantiles, hazard, cumulative hazard or survival function"))
  }
  if(("q" %in% what) & (!("q" %in% names(x)))){
    stop(gettext("Plotting quantiles is not supported for this object."))
  }

  # heuristic to set plot interval
  # start at lowest time interval
  if(missing(from)){
    from <- min(x$t)
  }
  # plot to 90% percentile or 1.5 times the range of given time points,
  # whichever is later
  if(missing(to)){
    if("q" %in% names(x)){
      q <- x$q(0.9)
    } else {
      q <- uniroot(\(t){x$s(t)-0.1}, lower=0, upper=1, extendInt = "downX")$root
    }
    t_max <- max(x$t) + 0.5*(max(x$t)-min(x$t))
    to <- max(q, t_max)
  }

  par(mfrow=mfrow)

  t <- seq(from, to, length.out=n)
  for(i in what){
    if(i %in% c("d", "h")){
      plot_cadlag(t, x[[i]], x$t, xlab="t", ylab=i, ...)
    } else {
      plot(t, x[[i]](t), xlab="t", ylab=i,  type="l", ...)
    }
    abline(v=x$t, col="grey")
  }

  invisible(NULL)
}

# internal function to plot right-continuous functions with limits from the left
# (cadlag) functions used in plot.miniPCH
#
# Arguments:
#   * x: points on x-Axis where to evaluate the function
#   * fun: the function
#   * jumps: a vector of jump-discontinuity points of fun
plot_cadlag <- function(x, fun, jumps, ...){
  # eps for comparison of smallest double step left of jump point
  tmp_eps <- ceiling(log2(max(c(abs(jumps), 2))))*.Machine$double.eps

  jumps <- c(jumps, Inf) |>
    sort() |>
    unique()

  plot(x, fun(x), type="n", ...)
  Map(\(i){
    tmp_x <- c(jumps[i], x[(x > jumps[i]) & (x < jumps[i+1])])
    lines(tmp_x, fun(tmp_x))
    points(jumps[i], fun(jumps[i]), pch=18)
    if(i != 1L){
      points(jumps[i], fun(jumps[i]-tmp_eps), pch=1)
    }
  }, 1:(length(jumps)-1))

  invisible(NULL)
}

#' @return for summary: a list
#' @export
#'
#' @describeIn miniPCH.class summary
summary.miniPCH <- function(object, ...){
  object[names(object) %in% c("t", "lambda", "Q", "pi", "abs", "discrete")]
}

#' @return for print: the printed text, invisibly
#' @export
#'
#' @describeIn miniPCH.class printing
print.miniPCH <- function(x, ...){
  if("lambda" %in% names(x)){
    text <- sprintf(
      "A miniPCH object\ndescribing a survival distribution with piecewise constant hazards defined on %i time intervals:\n%s\nand hazards:\n%s",
      length(x$t),
      paste0("[", x$t, ", ", c(x$t[-1], Inf), ")", collapse = ", "),
      paste0(x$lambda, collapse=", ")
    )
  } else {
    text <- sprintf(
      "A miniPCH object\ndescribing a distibution for time to absorption with %i states and %i absoribing states and piecewise constant transition rates on %i time intervals:\n%s",
      length(x$pi),
      sum(x$abs),
      length(x$t),
      paste0("[", x$t, ", ", c(x$t[-1], Inf), ")", collapse = ", ")
    )
  }
  cat(text)
  invisible(text)
}

#' @return
#' for autoplot: a ggplot object
#'
#' @exportS3Method ggplot2::autoplot
#'
#' @describeIn miniPCH.class autoplot with ggplot
autoplot.miniPCH <- function(object, ..., what=c("d", "s", "h"), from, to, n=1001){
  if (!requireNamespace("ggplot2", quietly = TRUE)){
    stop("Missing package 'ggplot2'.")
  }

  if(!all(what %in% c("d", "p", "q", "h", "ch", "s"))){
    stop(gettext("Argument what has to be a character vector containing only d, p, q, h, ch, s, to plot density, cdf, quantiles, hazard, cumulative hazard or survival function"))
  }
  if(("q" %in% what) & (!("q" %in% names(object)))){
    stop(gettext("Plotting quantiles is not supported for this object."))
  }

  # heuristic to set plot interval
  # start at lowest time interval
  if(missing(from)){
    from <- min(object$t)
  }
  # plot to 90% percentile or 1.5 times the range of given time points,
  # whichever is later
  if(missing(to)){
    if("q" %in% names(object)){
      q <- object$q(0.9)
    } else {
      q <- uniroot(\(t){object$s(t)-0.1}, lower=0, upper=1, extendInt = "downX")$root
    }
    t_max <- max(object$t) + 0.5*(max(object$t)-min(object$t))
    to <- max(q, t_max)
  }

  x <- seq(from, to, length.out=n)

  # eps for comparison of smallest double step left of jump point
  tmp_eps <- ceiling(log2(max(c(abs(object$t), 2))))*.Machine$double.eps

  plotdata <- Map(
    \(i, name){
      if(i %in% c("d", "h")){
        tmp_x <- setdiff(x, object$t)

        # data for continuous part
        data_cont <- data.frame(
         x=tmp_x,
         y=object[[i]](tmp_x),
         jump=factor(NA, levels=c("l","r")),
         facet=name
        )

        # data for jumps
        data_jumps <- Map(\(j){
          x_i <- object$t[j]
          data.frame(
            x = rep(x_i, 3),
            y = c(object[[i]](x_i-tmp_eps), NA_real_, object[[i]](x_i)),
            jump = factor(c("r", NA, "l"), levels=c("l","r")),
            facet = name
          )
        }, 1:length(object$t)) |>
          do.call(rbind, args=_)

        # first value only has a left limit
        data_jumps <- data_jumps[-1, ]

        # sort by is stable, so values at jump points are not re-ordered
        rbind(data_cont, data_jumps) |>
          (\(dat){dat[order(dat$x), ]})()
      } else {
        data.frame(
          x=x,
          y=object[[i]](x),
          jump=factor(NA, levels=c("l","r")),
          facet=name
        )
      }
    },
    what, make.unique(what)
  ) |>
    do.call(rbind, args=_)

  gg <- ggplot2::ggplot(plotdata, ggplot2::aes(x=x, y=y)) +
    ggplot2::geom_line(na.rm=TRUE) +
    ggplot2::geom_point(ggplot2::aes(shape=jump), na.rm=TRUE) +
    ggplot2::scale_shape_manual(values=c(l=16, r=1), guide="none") +
    ggplot2::facet_wrap(~facet, scales="free_y") +
    ggplot2::geom_vline(xintercept = object[["t"]], lty=2) +
    ggplot2::labs(
      x="t",
      y=character(0)
    )

  gg
}

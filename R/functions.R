
#' Gompertz function
#'
#' @param alpha Double \eqn{\alpha}
#' @param beta Double \eqn{\beta}
#'
#' @return Function which associes x to \eqn{\alpha exp(\beta x)}
#'
#' @export
#'
gompertz <- function(alpha, beta) {
    fun <- function(x) return(alpha * exp(beta * x))
    class(fun) <- c("gompertz", "function")
    return(fun)
}

#' Weibull function
#'
#' @param shape Double
#' @param scale Double
#'
#' @return Function which associes x to dweibull(x,shape,scale)
#'
#' @export
#'
weibull <- function( shape,scale) {
    fun <- function(x) return(dweibull(x,shape,scale))
    class(fun) <- c("weibull", "function")
    return(fun)
}


#' Piecewise function
#'
#' @param breaks Intervals
#' @param funs List of functions
#'
#' @return Piecewise function built with the given interval and functions
#'
#' @export
#'
piecewise_x <- function(breaks, funs) {
    breaks <- c(-Inf, breaks)
    fun <- function(x) {
        i <- findInterval(x, breaks) #, all.inside = TRUE)
        return((funs[[i]])(x))
    }
    class(fun) <- c("piecewise_x", "function")
    return(fun)
}

#' Piecewise time function
#'
#' @param breaks Intervals
#' @param funs List of functions
#'
#' @return Piecewise time function built with the given interval and functions
#'
#' @export
#'
piecewise_xy <- function(breaks, funs) {
    breaks <- c(-Inf, breaks)
    fun <- function(x, y) {
        i <- findInterval(x, breaks) #, all.inside = TRUE)
        return((funs[[i]])(y))
    }
    class(fun) <- c("piecewise_xy", "function")
    return(fun)
}

#' Max of step function
#'
#' @param ... argument of class 'stepfun'
#' @param na.rm a logical indicating whether missing values should be removed
#'
#' @return The max of the step function
#'
#' @export
#'
max.stepfun <- function(..., na.rm = FALSE) {
    env <- environment(...)
    return(max(env$y))
}


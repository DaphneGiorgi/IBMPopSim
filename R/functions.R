details_cpp <- function() {
   "@details A C++ version of this function is available. See \\code{vignette('IBMPopSim_cpp')} for more details."
}

#' Step Function.
#'
#' @description Given the vectors `(x[1],...,x[n])` and `(y[0],y[1],...,y[n])` (one value more!), `stepfun(x, y)` returns an interpolating step function, say `f_n`. This is the cadlag version (`right = FALSE`) of the `stepfun` function from package `stats`. The step function value `f_n(t)` equals to the constant `y[k-1]` for `t` in `[x[k-1], x[k])` so that
#' \deqn{f_n(t) = \sum_{k=1}^{n+1} y_{k-1} {1}_{[x_{k-1}, x_{k})}(t),}
#' with\eqn{x_0=-\infty} and \eqn{x_{n+1}=+\infty}.
#'
#' @param x Numeric vector giving the knots or jump locations of the step function. Must be sorted with unique values.
#' @param y Numeric vector one longer than x, giving the heights of the function values between the c\code{x} values.
#' @details This function is defined for documentation purposes only. See \code{\link[stats]{stepfun}} and \code{\link[stats]{approxfun}}.
#'
#' @eval details_cpp()
#'
#' @seealso \code{\link[stats]{plot.stepfun}} and \code{\link{max.stepfun}}.
#'
#' @return Objet of class \code{\link[stats]{stepfun}} with option \code{right = FALSE} (cadlag function).
#'
#' @export
#'
stepfun <- function(x, y) {
  assertNumeric(x, finite = TRUE, any.missing = FALSE, sorted = TRUE, unique = TRUE, min.len = 1)
  assertNumeric(y, finite = TRUE, any.missing = FALSE, len = length(x)+1)
  f <- stats::stepfun(x, y, f = 0, ties = "ordered", right = FALSE)
  return(f)
}


#' Linear interpolation function.
#'
#' @description Return a function performing the linear interpolation.
#'
#' @param x,y	 Numeric vectors giving the coordinates of the points to be interpolated.
#' @param yleft	The value to be returned when input \code{x} values are less than \code{min(x)}.
#' @param yright The value to be returned when input \code{x} values are greater than \code{max(x)}.
#'
#' @eval details_cpp()
#'
#' @return Objet of class \code{linfun} and \code{function} which is an \code{\link[stats]{approxfun}} function with \code{method = 'linear'}.
#'
#' @export
#'
linfun <- function(x, y, yleft = y[1], yright = y[length(y)]) {
  assertNumeric(x, finite = TRUE, any.missing = FALSE, sorted = TRUE, min.len = 1)
  assertNumeric(y, finite = TRUE, any.missing = FALSE, len = length(x))
  assertNumber(yleft, finite = TRUE)
  assertNumber(yright, finite = TRUE)
  f <- stats::approxfun(x, y, method = "linear", yleft, yright)
  class(f) <- c("linfun", "function")
  return(f)
}

#' Gompertz–Makeham intensity function.
#'
#' @description The intensity function (or hazard function) for the Gompertz-Makeham law of mortality distribution is defined as
#' \deqn{h(x) = \alpha e^{\beta x} + \lambda}
#' with \eqn{\alpha, \beta, \lambda \in {R}_+}.
#'
#' @param alpha Non-negative real parameter.
#' @param beta Non-negative real parameter.
#' @param lambda Non-negative real parameter.
#'
#' @eval details_cpp()
#'
#' @return Function which associes \code{x} to \eqn{\alpha exp(\beta x) + \lambda}.
#'
#' @seealso <https://en.wikipedia.org/wiki/Gompertz%E2%80%93Makeham_law_of_mortality>
#'
#' @export
#'
gompertz <- function(alpha, beta, lambda = 0) {
  assertNumber(alpha, lower = 0, finite = TRUE)
  assertNumber(beta, lower = 0, finite = TRUE)
  assertNumber(lambda, lower = 0, finite = TRUE)
  fun <- function(x) return(alpha * exp(beta * x) + lambda)
  class(fun) <- c("gompertz", "function")
  return(fun)
}

#' Weibull function.
#'
#' @description The Weibull (density) function is defined as
#' \deqn{h(x) = \bigl(\frac{k}{\lambda}\bigr) {\bigl( \frac{x}{\lambda} \bigr)}^{k-1} e^{-(x/\lambda)^k}}
#' with \eqn{k, \lambda \in (0, +\infty)}.
#'
#' @param k Shape parameter, a positive real number.
#' @param lambda Scale parameter, a positive real number, defaults to 1.
#'
#' @eval details_cpp()
#'
#' @return The Weibull density function \code{dweibull} with shape parameter \code{k} and scale parameter \code{lambda}, see \code{\link[stats]{dweibull}}.
#'
#' @seealso <https://en.wikipedia.org/wiki/Weibull_distribution>
#'
#' @export
#'
weibull <- function(k, lambda = 1) {
  assertNumber(k, lower = 0, finite = TRUE)
  assertNumber(lambda, lower = 0, finite = TRUE)
  fun <- function(x) return(stats::dweibull(x, k, lambda))
  class(fun) <- c("weibull", "function")
  return(fun)
}


#' Piecewise real function.
#'
#' @description Given the vectors `(breaks[1],...,breaks[n])` and the list of `IBMPopSim` compatible
#' functions `funs = (f[0],f[1],...,f[n])` (one value more!), `piecewise_x(breaks, funs)` returns
#' the function
#' \deqn{f(x) = f_0(x){1}_{x\le breaks[1]}+\sum_{k=1}^{n-1} f_k(x) {1}_{[breaks_{k}, breaks_{k+1})}(x) + f_n(x){1}_{x \ge breaks[n]}}
#'
#'
#' @param breaks Numeric vector giving the breaks of functions given in \code{funs}. Must be sorted with unique values.
#' @param funs List of functions.
#'
#' @eval details_cpp()
#'
#' @return Piecewise function built with the given intervals and functions.
#'
#' @examples
#' dr <- with(EW_pop_14$rates,
#'            stepfun(x=death_male[,"age"], y=c(0,death_male[,"value"])))
#' # before age 80 the stepfun and after age 80 the gompertz function
#' f <- piecewise_x(80, list(dr, gompertz(0.00006, 0.085)))
#' x <- seq(40:120)
#' plot(x, sapply(x, f))
#'
#' @export
#'
piecewise_x <- function(breaks, funs) {
  assertNumeric(breaks, any.missing = FALSE, min.len = 1, unique = TRUE, sorted = TRUE)
  assertList(funs, any.missing = FALSE, len = length(breaks)+1)
  breaks <- c(-Inf, breaks)
  fun <- function(x) {
    res <- sapply(x, function(y) {
        i <- findInterval(y, breaks)
        return((funs[[i]])(y))
      })
  }
  class(fun) <- c("piecewise_x", "function")
  return(fun)
}

#' Piecewise real function of two variables.
#'
#' @description Given the vectors `(breaks[1],...,breaks[n])` and the list of `IBMPopSim` compatible
#' functions `funs = (f[0],f[1],...,f[n])` (one value more!), `piecewise_xy(breaks, funs)` returns
#' the function
#' \deqn{f(x,y) = f_0(x) {1}_{y\le breaks[1]}+\sum_{k=1}^{n-1} f_k(x) {1}_{[breaks_{k}, breaks_{k+1})}(y) + f_n(x){1}_{y \ge breaks[n]}}
#'
#' @param breaks Numeric vector giving the breaks of functions given in \code{funs}. Must be sorted with unique values.
#' @param funs List of functions.
#'
#' @eval details_cpp()
#'
#' @return Piecewise bivariate function built with the given intervals and functions.
#'
#' @examples
#' time_dep_function <- piecewise_xy(c(5),
#'                                   list(gompertz(0.1, 0.005), gompertz(0.08, 0.005)))
#' time_dep_function(0, 65)  # death intensity at time 0 and age 65.
#'
#' @export
#'
piecewise_xy <- function(breaks, funs) {
  assertNumeric(breaks, any.missing = FALSE, min.len = 1, unique = TRUE, sorted = TRUE)
  assertList(funs, any.missing = FALSE, len = length(breaks)+1)
  breaks <- c(-Inf, breaks)
  fun <- function(x, y) {
      i <- findInterval(x, breaks)
      return((funs[[i]])(y))
  }
  class(fun) <- c("piecewise_xy", "function")
  return(fun)
}

#' Returns the maximum of a function of class \code{stepfun}.
#'
#' @param ... argument of class \code{stepfun}
#' @param na.rm a logical indicating whether missing values should be removed
#'
#' @return The maximum of the step function.
#'
#' @export
#'
max.stepfun <- function(..., na.rm = FALSE) {
  env <- environment(...)
  return(max(env$y))
}

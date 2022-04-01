#' X11 utility function
#'
#' @param x the input time series
#' @param freq the frequency.
#' @param length the length of the Henderson filter.
#' @param nlags,mul parameters.
#' @param ... unused argument.
#'
#' @name x11_utilities
#' @export
#' @importFrom stats frequency
#' @importFrom rjdfilters henderson
calcICR <- function(x, length = 13){
  sc <- henderson(x, length = length, musgrave = FALSE)
  si <- x - sc
  gc <- calAbsMeanVariations(sc, 1)
  gi <- calAbsMeanVariations(si, 1)
  icr = gi/gc
  freq = frequency(x)
  if(freq == 4){
    icr = icr * 3
  } else if (freq == 2){
    icr = icr * 6
  }
  return(icr)
}
#' @rdname x11_utilities
#' @export
calAbsMeanVariations <- function(x, nlags = 1, mul = FALSE){
  mean = vector(mode = "numeric", length = nlags)
  for (lag in 1:nlags){
    x1 = x
    x0 = lag(x, -lag)
    d = x1 - x0
    if(mul)
      d = d/x0
    mean[lag] = sum(abs(d),na.rm = TRUE)
  }
  mean
}
#' @rdname x11_utilities
#' @export
selectFilter <- function(x, ...){
  UseMethod("selectFilter")
}
#' @rdname x11_utilities
#' @export
selectFilter.default <- function(x, ..., freq) {
  icr = x
  if (freq == 2) {
    return(c(icr = icr, length = 5));
  }
  if (icr >= 1 && icr < 3.5) {
    return(c(icr = icr, length = freq + 1));
  }
  if (icr < 1) {
    if (freq == 12) {
      c(icr = icr, length = 9)
    } else {
      c(icr = icr, length = 5)
    }
  } else {
    if(freq == 12){
      c(icr = icr, length = 23)
    }else{
      c(icr = icr, length = 7)
    }
  }
}
#' @rdname x11_utilities
#' @export
selectFilter.ts <- function(x, ..., length = 13){
  selectFilter(calcICR(x, length = length), freq = frequency(x))
}
#' @rdname x11_utilities
#' @export
#' @importFrom rjdfilters lp_filter
CV <- function(x, horizon = 6){
  sc <- henderson(x, length = horizon*2+1, musgrave = FALSE)
  h_coef <- lp_filter(horizon = horizon)$filters.coef
  h_coef0 <- h_coef["t",horizon+1]
  (x-sc)/(1-h_coef0)
}

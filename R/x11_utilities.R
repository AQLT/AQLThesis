#' X11 utility function
#'
#' @param x the input time series
#' @param freq the frequency.
#' @param nlags,mul parameters
#' @param ... unused argument
#'
#' @name x11_utilities
#' @export
#' @importFrom stats frequency
#' @importFrom rjdfilters henderson
calcICR <- function(x){
  sc <- henderson(x, length = 13,musgrave = FALSE)
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
    return (5);
  }
  if (icr >= 1 && icr < 3.5) {
    return(freq + 1);
  }
  if (icr < 1) {
    if (freq == 12) {
      9
    } else {
      5
    }
  } else {
    if(freq == 12){
      23
    }else{
      7
    }
  }
}
#' @rdname x11_utilities
#' @export
selectFilter.ts <- function(x, ...){
  selectFilter(calcICR(x), freq = frequency(x))
}

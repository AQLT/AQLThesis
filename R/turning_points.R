
#' Detect turning points in a time series
#'
#' @param x the input time series
#' @param start,end the interval where to find turning points
#'
#' @details \loadmathjax
#' Zellner, Hong, et Min (1991) definition is used \mjseqn{k=3}, \mjseqn{m=1}:
#' - we have an upturn at date \mjseqn{t} when
#' \mjsdeqn{
#' y_{t-k}\geq\cdots\geq y_{t-1}<y_t\leq y_{t+1}\leq\cdots y_{t+m}
#' }
#'
#' - we have a downturn at date \mjseqn{t} when
#'
#' \mjsdeqn{
#' y_{t-k}\leq\cdots\leq y_{t-1}>y_t\geq y_{t+1}\geq\cdots y_{t+m}
#' }
#' @export
#' @importFrom stats ts start end time window
#' @importFrom zoo rollapply
turning_points <- function(x, start = NULL, end = NULL,
                           digits = 6){
  if(is.null(x))
    return(NULL)
  list(upturn = upturn(x, start = start, end = end, digits = digits),
       downturn = downturn(x, start = start, end = end, digits = digits))
}
#' @rdname turning_points
#' @export
upturn <- function(x, start = NULL, end = NULL,
                   digits = 6){
  if(is.null(x))
    return(NULL)
  if(!is.null(digits))
    x = round(x, digits = digits)
  res <- zoo::rollapply(x, width=5,
                   function(x){
                     (x[1]>=x[2]) & (x[2]>=x[3]) &
                       (x[3]<x[4]) & (x[4]<=x[5])
                   })
  res <- window(res, start = start, end = end, extend = TRUE)
  res <- time(res)[which(res)]
  res
}
#' @rdname turning_points
#' @export
downturn <- function(x, start = NULL, end = NULL,
                     digits = 6){
  if(is.null(x))
    return(NULL)
  if(!is.null(digits))
    x = round(x, digits = digits)
  res <- rollapply(x, width=5,
                   function(x){
                     (x[1]<=x[2]) & (x[2]<=x[3]) &
                       (x[3]>x[4]) & (x[4]>=x[5])
                   })
  res <- window(res, start = start, end = end, extend = TRUE)
  res <- time(res)[which(res)]
  res
}

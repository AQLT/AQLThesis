#' Simulate Trend-Cycle Component
#' @param t,rho,lambda,sigma_nu,sigma_e,start,n parameters, see details.
#' @details \loadmathjax
#' The following formula is used for the cycle
#' \mjsdeqn{
#' C_t = \rho \[
#' \cos (2 \pi t / \lambda) +
#' \sin (2 \pi t / \lambda)
#' \]
#' }
#' The trend component is assumed to be a random walk with drift:
#' \mjsdeqn{
#' T_t = T_{t-1} + \nu_t\quad \nu_t \sim \mathcal{N}(0, \sigma_\nu^2)
#' }
#' The irregular component is white noise with mean zero and variance \mjseqn{\sigma_e^2}:
#' \mjsdeqn{
#' I_t = e_t\text{ with }
#' e_t \sim \mathcal{N}(0, \sigma_e^2)
#' }
#' @export
#' @importFrom stats rnorm
simulated_tci <- function(t, rho, lambda, sigma_nu, sigma_e, start = 0){
  n = length(t)
  cycle = simulated_cycle(t = t, rho = rho, lambda = lambda)
  trend = simulated_trend(n = n, sigma_nu = sigma_nu, start = start)
  irregular = simulated_irregular(n = n, sigma_e = sigma_e)
  tc = cycle + trend + irregular
  cbind(tc, cycle, trend, irregular)
}
#' @rdname simulated_tci
#' @export
simulated_cycle <- function(t, rho, lambda){
  rho *(cos(2*pi*t/lambda) + sin(2*pi*t/lambda))
}
#' @rdname simulated_tci
#' @export
simulated_trend <- function(n = 1, sigma_nu=1, start = 0){
  data = c(start, rnorm(n - 1, sd = sigma_nu))
  cumsum(data)
}
#' @rdname simulated_tci
#' @export
simulated_irregular <- function(n = 1, sigma_e=1){
  rnorm(n, sd = sigma_e)
}


#' Compute Time Lag in Detection of Turning Points
#'
#' @param data the data.
#' @param peaks,troughs the reference dates of peaks and troughs.
#' @param frequency the annual frequency of the series.
#' @param type method used to devine the time lag.
#' @param tp_limit maximal number of periods between the detected turning-point and the reference dates.
#' @param n_ahead_max maximal number of periods after the final detected turning point used to compute the time lag (when `type ="no_revisions"`).
#' @export
compute_time_lag <- function(data,
         peaks = nber_tp_m[,"Peak"],
         troughs = nber_tp_m[,"Trough"],
         frequency = 12,
         type = c("first_detection", "no_revisions"),
         tp_limit = 3,
         n_ahead_max = NULL){
  type <- match.arg(type)
  if(type == "first_detection"){
    detection_fun <- first_detection
  }else{
    detection_fun <- no_revisions
  }

  peaks <- peaks[!is.na(peaks)]
  troughs <- troughs[!is.na(troughs)]

  troughs_timelag <- compute_tp(data = data, focus_tp = peaks, tp_limit = tp_limit,detection_fun = detection_fun, frequency = frequency, n_ahead_max = n_ahead_max)
  peaks_timelag <- compute_tp(data = data, focus_tp = troughs, tp_limit = tp_limit,detection_fun = detection_fun, frequency = frequency, n_ahead_max = n_ahead_max)
  list(peaks = peaks_timelag,
       troughs = troughs_timelag)
}
utils::globalVariables("nber_tp_m")
compute_tp <- function(data, focus_tp, tp_limit, detection_fun, frequency, n_ahead_max = NULL){

  last_tp_det = do.call(c, data[[length(data)]])
  # vector with phase shift of all the detected tp
  final_phaseshift = sapply(focus_tp, function(y) which_abs_min(last_tp_det - y))*frequency
  final_phaseshift = round(final_phaseshift)
  final_phaseshift[abs(final_phaseshift) > tp_limit] <- NA
  names(final_phaseshift) <- focus_tp
  # vector with the final dates of the TP with correspondance to the focus tp dates
  format_tp <- last_tp_ret <- focus_tp + final_phaseshift/frequency
  # replace NA by current values to format output
  format_tp[is.na(format_tp)] <- focus_tp[is.na(format_tp)]

  timelag <- sapply(data, function(x){
    if(is.null(x))
      return(rep(FALSE, length(format_tp)))
    round(format_tp,3) %in% round(do.call(c, x), 3)
  },simplify = "matrix")
  rownames(timelag) <- focus_tp
  # timelag[names(final_phaseshift)[!is.na(final_phaseshift)],seq(ncol(timelag)-5,length.out = 5)]
# View(timelag)
  first_date = round(as.numeric(colnames(timelag)[1]),3)
  timelag = sapply(names(format_tp), function(n_tp){
    tp = format_tp[n_tp]
    if(round(as.numeric(tp),3) < first_date ||
       !any(timelag[n_tp,]) ||
       timelag[n_tp,1])
      return(NA)
    x = timelag[n_tp,]
    if (!is.null(n_ahead_max))
      x = x[seq_len(min(which(names(x) == n_tp) + n_ahead_max, length(x)))]
    est_tp <- detection_fun(x)
    if(length(est_tp) == 0)
      return(NA)
    (est_tp - as.numeric(tp))*frequency
  })
  list(phaseshift_correcttp = final_phaseshift,
       last_tp = last_tp_ret,
       phaseshift = timelag)
}

no_revisions <- function(x){
  i <- length(x)
  remove_i <- NULL
  while (x[i] && i > 0) {
    remove_i <- c(i, remove_i)
    i <- i - 1
  }
  as.numeric(names(x)[remove_i[1]])
}
first_detection <- function(x){
  valid_dates <- which(x)
  if(length(valid_dates) ==0)
    NA
  as.numeric(as.numeric(names(x[valid_dates[1]])))
}

which_abs_min <- function(x){
  x[which.min(abs(x))]
}

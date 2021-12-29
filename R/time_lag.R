#'@export
compute_time_lag <- function(data,
                             peaks = nber_tp_m[,"Peak"],
                             troughs = nber_tp_m[,"Trough"]){
  peaks <- round(na.omit(peaks),3)
  troughs <- round(na.omit(troughs),3)

  peaks_timelag <- sapply(data, function(x){
    peaks %in% round(do.call(c, x), 3)
  })
  rownames(peaks_timelag) <- peaks
  troughs_timelag <- sapply(data, function(x){
    troughs %in% round(do.call(c, x), 3)
  })
  rownames(troughs_timelag) <- troughs
  peaks_timelag <- sapply(rownames(peaks_timelag), function(tp){
    if(!any(peaks_timelag[tp,]) | peaks_timelag[tp,1])
      return(NA)
    first_est_tp <- colnames(peaks_timelag)[peaks_timelag[tp,]][1]
    (as.numeric(first_est_tp) - as.numeric(tp))*12
  })
  troughs_timelag <- sapply(rownames(troughs_timelag), function(tp){
    if(!any(troughs_timelag[tp,]) | troughs_timelag[tp,1])
      return(NA)
    first_est_tp <- colnames(troughs_timelag)[troughs_timelag[tp,]][1]
    (as.numeric(first_est_tp) - as.numeric(tp))*12
  })
  list(peaks = peaks_timelag,
       troughs = troughs_timelag)
}

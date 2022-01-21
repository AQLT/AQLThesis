
#'@export
compute_time_lag <- function(data,
                             peaks = nber_tp_m[,"Peak"],
                             troughs = nber_tp_m[,"Trough"],
                             frequency = 12){
  peaks <- na.omit(peaks)
  troughs <- na.omit(troughs)

  peaks_timelag <- sapply(data, function(x){
    if(is.null(x))
      return(rep(FALSE, length(peaks)))
    round(peaks,3) %in% round(do.call(c, x), 3)
  })
  rownames(peaks_timelag) <- peaks
  troughs_timelag <- sapply(data, function(x){
    if(is.null(x))
      return(rep(FALSE, length(troughs)))
    round(troughs,3) %in% round(do.call(c, x), 3)
  })
  rownames(troughs_timelag) <- troughs
  first_date = round(as.numeric(colnames(peaks_timelag)[1]),3)
  peaks_timelag <- sapply(rownames(peaks_timelag), function(tp){
    if(round(as.numeric(tp),3) < first_date ||
       !any(peaks_timelag[tp,]) ||
       peaks_timelag[tp,1])
      return(NA)
    first_est_tp <- colnames(peaks_timelag)[peaks_timelag[tp,]][1]
    (as.numeric(first_est_tp) - as.numeric(tp))*frequency
  })

  first_date = round(as.numeric(colnames(troughs_timelag)[1]),3)
  troughs_timelag <- sapply(rownames(troughs_timelag), function(tp){
    if(round(as.numeric(tp),3) < first_date ||
       !any(troughs_timelag[tp,]) ||
       troughs_timelag[tp,1])
      return(NA)
    first_est_tp <- colnames(troughs_timelag)[troughs_timelag[tp,]][1]
    (as.numeric(first_est_tp) - as.numeric(tp))*frequency
  })
  list(peaks = peaks_timelag,
       troughs = troughs_timelag)
}

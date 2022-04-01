
#'@export
compute_time_lag <- function(data,
                             peaks = nber_tp_m[,"Peak"],
                             troughs = nber_tp_m[,"Trough"],
                             frequency = 12,
                             type = c("first_detection", "no_revisions")){
  type <- match.arg(type)
  if(type == "first_detection"){
    detection_fun <- first_detection
  }else{
    detection_fun <- no_revisions
  }

  peaks <- na.omit(peaks)
  troughs <- na.omit(troughs)

  peaks_timelag <- sapply(data, function(x){
    if(is.null(x))
      return(rep(FALSE, length(peaks)))
    round(peaks,3) %in% round(do.call(c, x), 3)
  },simplify = "matrix")
  rownames(peaks_timelag) <- peaks
  troughs_timelag <- sapply(data, function(x){
    if(is.null(x))
      return(rep(FALSE, length(troughs)))
    round(troughs,3) %in% round(do.call(c, x), 3)
  },simplify = "matrix")
  rownames(troughs_timelag) <- troughs
  first_date = round(as.numeric(colnames(peaks_timelag)[1]),3)
  peaks_timelag <- sapply(rownames(peaks_timelag), function(tp){
    if(round(as.numeric(tp),3) < first_date ||
       !any(peaks_timelag[tp,]) ||
       peaks_timelag[tp,1])
      return(NA)
    est_tp <- detection_fun(peaks_timelag[tp,])
    if(length(est_tp) == 0)
      return(NA)
    (est_tp - as.numeric(tp))*frequency
  })

  first_date = round(as.numeric(colnames(troughs_timelag)[1]),3)
  troughs_timelag <- sapply(rownames(troughs_timelag), function(tp){
    if(round(as.numeric(tp),3) < first_date ||
       !any(troughs_timelag[tp,]) ||
       troughs_timelag[tp,1])
      return(NA)
    est_tp <- detection_fun(troughs_timelag[tp,])
    if(length(est_tp) == 0)
      return(NA)
    (est_tp - as.numeric(tp))*frequency
  })
  list(peaks = peaks_timelag,
       troughs = troughs_timelag)
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

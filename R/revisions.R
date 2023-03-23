#'@export
first_est_revisions <- function(x){
  last_est <- x[[length(x)]]
  while(is.null(last_est) & length(x) >1){
    x <- x[-length(x)]
    last_est <- x[[length(x)]]
  }
  if(is.null(last_est))
    return(NULL)
  revisions_firstest <- t(sapply(x, function(est){
    if(is.null(est)){
      return(rep(NA,11))
    }
    tail(abs((est-last_est)/last_est),11)[11:1]
  }))
  colnames(revisions_firstest) <-
    sprintf("rev q%i",0:10)
  revisions_firstest
}

#'@export
consecutive_est_revisions <- function(x){
  revisions_consest <- t(sapply(seq_along(x), function(j){
    if(j == length(x) ||
       is.null(x[[j]]) ||
       is.null(x[[j+1]])){
      return(rep(NA,11))
    }
    tail(abs((x[[j]]-x[[j+1]])/x[[j+1]]),11)[11:1]
  }))
  colnames(revisions_consest) <-
    sprintf("rev q%i",0:10)
  rownames(revisions_consest) <- names(x)

  revisions_consest
}

#' @importFrom stats aggregate na.omit
#'@importFrom utils tail
#' @export
summary_revisions <- function(data,
                              peaks = nber_tp_m[,"Peak"],
                              troughs = nber_tp_m[,"Trough"],
                              annual_frequency_tp = 2){
  tp = c(na.omit(peaks),na.omit(troughs))
  data = data.frame(data)
  data$date = as.numeric(rownames(data))
  data$type = "normal"
  data$total = "total"
  for(date in tp){
    data$type[(data$date <= (date + 1/annual_frequency_tp)) &
                (data$date >= (date - 1/annual_frequency_tp))] <- "turning-point"
  }
  rmse <- function(x, ...){
    sqrt(mean(x^2, na.rm = TRUE, ...))
  }
  mae = function(x, ...){
    mean(x, na.rm=TRUE, ...)
  }

  global_stats = NULL
  for(FUN in c("rmse", "mae")){
    stats = apply(data[,1:11],2, FUN)
    stats_group = aggregate(data[,1:11], by = list(data$type), FUN)
    colnames(stats_group)[1] <- "Group"
    stats = rbind(stats_group,
                  data.frame("Group" = "total", t(stats)))
    stats$stats = toupper(FUN)
    if(is.null(global_stats)){
      global_stats <- stats
    } else {
      global_stats <- rbind(global_stats, stats)
    }
  }
  global_stats
}

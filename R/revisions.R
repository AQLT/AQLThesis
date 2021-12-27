#'@export
first_est_revisions <- function(x){
  last_est <- x[[length(x)]]
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

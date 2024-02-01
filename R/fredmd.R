#' Import fredmd database
#'
#' @param file path to a file.
#' @param date_start,date_end optional date start and end.
#' @param transform boolean indicating if the data should be transformed.
#' @param log boolean indicating if only the log transformation should be used (\code{transform} must be equal to \code{FALSE}).
#'@export
#'@importFrom readr read_csv cols col_date
#'@importFrom utils read.csv
#'@importFrom stats lag
#'@author code largely inspired by Yankang (Bennie) Chen function of the 'fbi' package available at \url{https://github.com/cykbennie/fbi}.
fredmd <- function (file = "", date_start = NULL, date_end = NULL, transform = TRUE,
                    log = FALSE)
{
  if (!is.logical(transform))
    stop("'transform' must be logical.")
  if ((!inherits(date_start, "Date")) && (!is.null(date_start)))
    stop("'date_start' must be Date or NULL.")
  if ((!inherits(date_end, "Date")) && (!is.null(date_end)))
    stop("'date_end' must be Date or NULL.")
  if (inherits(date_start, "Date")) {
    if (as.numeric(format(date_start, "%d")) != 1)
      stop("'date_start' must be Date whose day is 1.")
    if (date_start < as.Date("1959-01-01"))
      stop("'date_start' must be later than 1959-01-01.")
  }
  if (inherits(date_end, "Date")) {
    if (as.numeric(format(date_end, "%d")) != 1)
      stop("'date_end' must be Date whose day is 1.")
  }
  rawdata <- readr::read_csv(file, col_names = FALSE, col_types = readr::cols(X1 = readr::col_date(format = "%m/%d/%Y")),
                             skip = 2)
  rawdata <- rawdata[!is.na(rawdata[,1]), ] #supprimer les eventuelles dernieres lignes vides
  rawdata <- as.data.frame(rawdata)
  nom_col <- colnames(read.csv(file,header = TRUE,nrows =1))
  header <- c("date", nom_col[-1])
  colnames(rawdata) <- header
  tcode <- read.csv(file,header = FALSE,nrows =1,skip = 1)[1,-1]

  data <- rawdata

  if (is.null(date_start))
    date_start <- data[1,1]
  if (is.null(date_end))
    date_end <- data[, 1][nrow(data)]
  index_start <- which.max(data[, 1] == date_start)
  index_end <- which.max(data[, 1] == date_end)
  data <- data[index_start:index_end, ]
  first_date <- as.character(data[1,1])


  ts_data <- ts(data[,-1],
                start = as.numeric(c(substr(first_date,1,4), substr(first_date,6,7))),
                frequency = 12)


  if (transform) {
    data_out <- do.call(cbind, lapply(seq_len(ncol(ts_data)),function(i){
      transxfred(ts_data[,i], tcode[i])
    }))
    colnames(data_out) <- colnames(ts_data)
    ts_data <- data_out
  }else if(log){
    data_out <- do.call(cbind, lapply(seq_len(ncol(ts_data)),function(i){
      transxfredlog(ts_data[,i], tcode[i])
    }))
    colnames(data_out) <- colnames(ts_data)
    ts_data <- data_out
  }

  return(ts_data)
}

transxfred <- function(x, tcode) {
  small <- 1e-06
  y <- NULL
  if (tcode == 1) {
    y <- x
  }else if (tcode == 2) {
    y <- diff(x)
  }else if (tcode == 3) {
    y <- diff(diff(x))
  }else if (tcode == 4) {
    if (min(x, na.rm = TRUE) > small)
      y <- log(x)
  }else if (tcode == 5) {
    if (min(x, na.rm = TRUE) > small) {
      x <- log(x)
      y <- diff(x)
    }
  }else if (tcode == 6) {
    if (min(x, na.rm = TRUE) > small) {
      x <- log(x)
      y <- diff(diff(x))
    }
  }else if (tcode == 7) {
    y1 <- diff(x)/stats::lag(x)
    y <- diff(y1)
  }else{
    stop("wrong tcode")
  }
  return(y)
}


transxfredlog <- function(x, tcode) {
  small <- 1e-06
  y <- NULL
  if (tcode %in% c(1, 2, 3)) {
    y <- x
  }else if (tcode %in% c(4, 5, 6)) {
    if (min(x, na.rm = TRUE) > small)
      y <- log(x)
  }else if (tcode == 7) {
    y1 <- diff(x)/stats::lag(x)
    y <- diff(y1)
  }else{
    stop("wrong tcode")
  }
  return(y)
}

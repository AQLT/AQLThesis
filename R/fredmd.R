#'@export
#'@importFrom readr read_csv cols col_date
#'@importFrom utils read.csv
fredmd <- function (file = "", date_start = NULL, date_end = NULL, transform = TRUE)
{
  if (!is.logical(transform))
    stop("'transform' must be logical.")
  if ((class(date_start) != "Date") && (!is.null(date_start)))
    stop("'date_start' must be Date or NULL.")
  if ((class(date_end) != "Date") && (!is.null(date_end)))
    stop("'date_end' must be Date or NULL.")
  if (class(date_start) == "Date") {
    if (as.numeric(format(date_start, "%d")) != 1)
      stop("'date_start' must be Date whose day is 1.")
    if (date_start < as.Date("1959-01-01"))
      stop("'date_start' must be later than 1959-01-01.")
  }
  if (class(date_end) == "Date") {
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
  tcode <- read.csv(file,header = FALSE,nrows =1,skip = 1,)[1,-1]
  transxf <- function(x, tcode) {
    n <- length(x)
    small <- 1e-06
    y <- rep(NA, n)
    y1 <- rep(NA, n)
    if (tcode == 1) {
      y <- x
    }
    else if (tcode == 2) {
      y[2:n] <- x[2:n] - x[1:(n - 1)]
    }
    else if (tcode == 3) {
      y[3:n] <- x[3:n] - 2 * x[2:(n - 1)] + x[1:(n - 2)]
    }
    else if (tcode == 4) {
      if (min(x, na.rm = TRUE) > small)
        y <- log(x)
    }
    else if (tcode == 5) {
      if (min(x, na.rm = TRUE) > small) {
        x <- log(x)
        y[2:n] <- x[2:n] - x[1:(n - 1)]
      }
    }
    else if (tcode == 6) {
      if (min(x, na.rm = TRUE) > small) {
        x <- log(x)
        y[3:n] <- x[3:n] - 2 * x[2:(n - 1)] + x[1:(n -
                                                     2)]
      }
    }
    else if (tcode == 7) {
      y1[2:n] <- (x[2:n] - x[1:(n - 1)])/x[1:(n - 1)]
      y[3:n] <- y1[3:n] - y1[2:(n - 1)]
    }
    return(y)
  }
  if (transform) {
    N <- ncol(rawdata)
    data <- rawdata
    data[, 2:N] <- NA
    for (i in 2:N) {
      temp <- transxf(rawdata[, i], tcode[i - 1])
      data[, i] <- temp
    }
  }else {
    data <- rawdata
  }
  if (is.null(date_start))
    date_start <- as.Date("1959-01-01")
  if (is.null(date_end))
    date_end <- data[, 1][nrow(data)]
  index_start <- which.max(data[, 1] == date_start)
  index_end <- which.max(data[, 1] == date_end)
  outdata <- data[index_start:index_end, ]
  class(outdata) <- c("data.frame", "fredmd")
  first_date <- as.character(outdata[1,1])

  return(ts(outdata[,-1],
            start = as.numeric(c(substr(first_date,1,4), substr(first_date,6,7))),
            frequency = 12))
}

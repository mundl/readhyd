try_timeformat <- function(x, format = "%d.%m.%Y %H:%M:%S") {
  time <- as.POSIXct(x[1], format = format)
  if(is.na(time)) {
    # position of percent signs, always keep the first one
    pos <- which(strsplit(format, split = "", fixed = T)[[1]] == "%")
    pos <- rev(tail(pos, -1) - 2)

    for(i in pos) {
      format <- substr(format, start = 1, stop = i)
      time <- as.POSIXct(x[1], format = format)

      if(!is.na(time)) return(as.POSIXct(x, format = format))
    }
  }

  return(as.POSIXct(x, format = format))
}



dms2dec <- function(x, split = "[^[:digit:].]+", ...){
  x <- trimws(gsub("[^[:digit:].]+", " ", x))
  res <- rep(NA, length(x))

  if(!all(is.na(x))) {
    y <- strsplit(x[!is.na(x)], split = " ", ...)
    z <- sapply(y, function(x)
      sum(as.numeric(x) * c(1, 1/60^seq_len(length(x) - 1))))
    res[!is.na(x)] <- z
  }

  return(res)
}

.toNum <- function(x) as.numeric(sub(",", ".", x))



regularize <- function(x, interval = "day") {
  # only day is supported
  interval <- match.arg(interval)

  # aggregate to days and truncate xts index
  daily <- apply.daily(x, mean, na.rm = T)
  index(daily) <- as.Date(time(daily))

  fullseq <- seq(from = min(time(daily)), to = max(time(daily)), by = interval)
  missing <- fullseq[!fullseq %in% time(daily)]

  if(length(missing)) {
    gaps <- xts(x = data.frame(discharge = rep_len(NA_real_, length(missing))), order.by = missing)
    daily <- rbind(daily, gaps)
  }

  return(daily)
}



check_colClasses <- function(x, indexCol = guess_index_col(x),
                             valueCol =  guess_value_col(x),
                             strict = FALSE) {

  # there can be more than one valueCol
  assert_that(is.data.frame(x),
              x %has_name% indexCol, all(x %has_name% valueCol),
              length(indexCol) == 1)

  index <- x[, indexCol]
  assert_that(is.date(index) || is.time(index), not_empty(index))

  value <- x[, valueCol]
  assert_that(not_empty(value), all(sapply(value, is.numeric)))


  if(all(is.na(index))) {
    stop("all index values in column ", sQuote(indexCol), " are NA.")
  }
  if(all(is.na(value))) {
    stop("all values in column ", paste(sQuote(valueCol), collapse = ", "),
         " are NA.")
  }

  if(strict){
    # no NA values allowed
    indexNA <- which(is.na(index))
    if(length(indexNA)) {
      stop("NA values in index at position: ", .first(indexNA))
    }

    valueNA <- which(is.na(value))
    if(length(valueNA)) {
      stop("NA values at position: ", .first(valueNA))
    }
  }
}

.first <- function(x, n = 6) {
  x <- as.character(x)
  if(length(x) > n) x <- c(head(x, n), "...")

  return(paste(x, collapse = ", "))
}



guess_index_col <- function(x) {
  # index columns are eihter of class Date or POSIX
  assert_that(is.data.frame(x))

  index <- which(sapply(x, inherits, what = "Date") |
                   sapply(x, inherits, what = "POSIXt"))

  if(length(index) == 1) {
    return(names(index))
  }

  if (length(index) == 0) {
    stop("no time-based column found in '", deparse(substitute(x)), "'.")
  }

  stop("found more than one time-based column in '",
       deparse(substitute(x)), "': ",
       paste(shQuote(names(index)), collapse = ", "))
}

guess_value_col <- function(x) {
  assert_that(is.data.frame(x))

  values <- which(sapply(x, inherits, what = "numeric") |
                    sapply(x, inherits, what = "integer"))

  if (length(values) == 0) {
    stop("no numeric column found in '", deparse(substitute(x)), "'.")
  }

  return(names(values))
}


split_header <- function(file, prefix = "#", regex = NULL, fixed = TRUE,
                         useBytes = TRUE, encoding = "UTF-8", ...) {

  con <- file(file, open = "rt", encoding = encoding)
  header <- list()

  if(!is.null(regex)){
    isHeader <- TRUE
    while (isHeader) {
      # possibly not a good idea to use a while loop
      # readLines() can cause problems when using a wrong encoding
      header <- c(header, readLines(con, n = 1))
      isHeader <- !grepl(regex, tail(header, 1L)[[1]], fixed = fixed,
                         useBytes = useBytes)
    }
  } else {
    isHeader <- TRUE
    while (isHeader) {
      header <- c(header, readLines(con, n = 1, warn = FALSE))
      isHeader <- substr(tail(header, 1L)[[1]], 1L, 1L) == prefix
    }

  }

  # we read one line to much
  pushBack(tail(header, 1)[[1]], con)
  header <- head(header, -1)
  header <- do.call(c, header)

  body <- scan(file = con, ...)
  close(con)

  return(list(header = header,
              body = do.call(data.frame, c(body, stringsAsFactors = FALSE))))

}

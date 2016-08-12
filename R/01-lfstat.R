# this functions are duplicated in lfstat

read.vardat2 <- function(file, check = TRUE, ...) {

  x <- split_header(file = file, sep = " ", quiet = TRUE,
                    what = list(time = character(), discharge = numeric()),
                    na.strings = c("-9999.000000", "-9999"), ...)

  header <- x$header
  body <- x$body
  body$time <- as.POSIXct(body$time, format = "%Y%m%d/%H%M")

  # extract attributes
  dcha <- x$header[grep("^#DCHA", header)]
  dcha <- strsplit(dcha, " ", fixed = TRUE)[[1]][2]
  dcha <- as.numeric(strsplit(dcha, ".", fixed = TRUE)[[1]])
  names(dcha) <- c("regime", "station", "point", "quantity", "version")

  attr(body, "meta") <- as.list(dcha)
  return(body)
}


read.lfu <- function(file, as.zoo = FALSE, ...) {

  x <- split_header(file = file, sep = " ", quiet = TRUE,
                    what = list(time = character(), value = numeric()), ...)

  header <- x$header
  body <- x$body

  # parse header for known keys
  keys <- c("SSNR", "SANR", "SNAME", "SWATER", "CNR", "CMW1",
            "CNAME", "CTYPE", "RINVAL", "RNR1", "RID")

  header <- paste(substring(header, first = 2L), collapse = "")
  header <- strsplit(header, split = "|", fixed = T)[[1]]
  meta <- sapply(keys, .getValueLFU, x = header)

  # this works only if NAstings is a (negative) number
  body$value[body$value == .toNum(meta["RINVAL"])] <- NA

  res <- data.frame(time = as.Date(body$time, format="%Y%m%d%H%M"),
                    flow = body$value)

  if (as.zoo) {
    res <- zoo(res$flow, order.by = res$time)
  }

  meta <- meta[setdiff(names(meta), "RINVAL")]
  attr(res, "meta") <- meta[!is.na(meta)]
  return(res)
}

.getValueLFU <- function(x, key) {
  str <- grep(key, x, fixed = TRUE, value = TRUE)
  skip <- nchar(key)
  value <- substring(str, first = skip+1)
  if (value == "*") value <- NA

  return(value)
}


read.grdc <- function(file, as.zoo = FALSE, ...) {

  # todo: parse header
  x <- split_header(file = file,  sep = ";", quiet = TRUE, skip = 1,
                    what = list(date = character(), time = character(),
                                original = numeric(), corrected = numeric(),
                                flag = numeric()),
                    na.strings = c("--:--", "-999.000"), ...)

  header <- x$header
  body <- x$body

  res <- data.frame(time =  as.Date(body$date),
                    body[, setdiff(colnames(body), c("date", "time"))])

  if (as.zoo) {
    res <- zoo(as.matrix(res[, setdiff(colnames(res), "time")]),
               order.by = res$time)
  }

  return(res)
}

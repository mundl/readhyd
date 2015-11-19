read.hzb <- function(file, parse.header = TRUE, fileEncoding = "ISO8859-1") {
  # separate the header, open the connection with correct enconding
  fh <- file(file, open = "rt", encoding = fileEncoding)
  header <- readLines(fh, n = 50)
  close(fh)

  type <- if (grepl(";", header[1])) "csv2" else "fwf"

  lines.header <- grep("Werte:", header, fixed = T)
  header <- head(header, lines.header - 1)

  na.strings <- iconv("L\u00fccke", from = "UTF-8", to = fileEncoding)

  args <- list(file = file, header = F, skip = lines.header,
               na.strings = na.strings, fileEncoding = fileEncoding,
               strip.white = TRUE, as.is = TRUE)

  infile <- if(type == "fwf") {
    do.call(read.fwf, c(args, list(col.names = c("time", "value"),
                                   colClasses = c("character", "numeric"),
                                   widths = c(20, 20))))
  } else if (type == "csv2"){
    do.call(read.csv2, c(args,
                         list(col.names = c("time", "value", NA),
                              colClasses = c("character", "numeric", "NULL"))))
  }

  infile$time <- as.POSIXct(infile$time, format = "%d.%m.%Y %H:%M:%S")

  if (parse.header) {
    meta <- .parse_hzb_header(header, type = type)
    attr(x = infile, which = "list") <- meta[["list"]]
    attr(x = infile, which = "keyval") <- meta[["keyval"]]
  }

  return(infile)
}
.parse_hzb_header <- function(x, type = c("csv2", "fwf")) {
  type <- match.arg(type)

  # some lines are in key: value format,
  # there are also nested lists idented with spaces, treat them differently
  ident <- nchar(sub("^([ ]*).*$", "\\1", x))

  # lines with the same identation compose a list
  idx <- numeric(length(ident))
  j <- 0
  for (i in seq_along(ident)){
    # only increment counter if there is no ident
    if(i == length(ident) || ident[i] == 0) j <- j + 1
    idx[i] <- j
  }

  # unique indices reflect simple key-value pairs
  is.keyval <- idx %in% names(which(table(idx) == 1))
  keyval <- x[which(is.keyval & grepl(":", x, fixed = T))]
  keyval <- .split_keyval(keyval)

  header.list <-  split(x[which(!is.keyval)], idx[!is.keyval])

  # Abschnitt "Exportzeitreihe" is komisch formatiert...
  header.list <- header.list[!grepl("Exportzeitreihe", header.list)]
  header.list <- do.call(c, lapply(unname(header.list), .split_list_whitespace))

  return(list(keyval = keyval, list = header.list))
}


# .split_keyval_csv <- function(x, only.first = T) {
#   # sometimes, only the first ";" is a field separator, replace it with "\t"
#   x <- if(only.first) sub(";", "\t", x) else gsub(";", "\t", x)
#   keyval <- do.call(rbind, strsplit(x, split = "\t"))
#   keyval <- apply(keyval, 2, trimws)
#   keyval <- apply(keyval, 2, sub, pattern = ":$", replacement = "")
#
#   keyval[1, ] <- .format_title(keyval[1, ])
#
#   return(keyval)
# }

.split_keyval <- function(x, only.first = T) {
  y <- gsub(":[[:blank:]]+;?", "\t ", x)
  keyval <- do.call(rbind, strsplit(y, split = "\t"))
  keyval <- apply(keyval, 2, trimws)

  keyval[, 1] <- .format_title(keyval[, 1])

  return(keyval)
}

.split_list <- function(x) {
  # split list after colons
  txt <- strsplit(x[2], "")[[1]]
  colon <- head(which(txt == ":"), -1)
  offset <- sapply(colon, function(x) grep("[^ ;]", tail(txt, -x))[1])
  pos <- colon + offset
  y <- do.call(rbind, lapply(x[-1], substring,
                             first = c(1, pos),
                             last = c(pos - 1, 10000L)))

  tbl <- trimws(sub(";$", "", y[-1, , drop = F]))
  colnames(tbl) <- .format_title(y[1, ])
  tbl <- list(tbl)
  names(tbl) <- .format_title(x[1])

  return(tbl)
}


.split_list_whitespace <- function(x) {
  x <- trimws(x)
  y <- gsub(":?[[:blank:]]{3,};?", "\t", x)
  y <- do.call(rbind, strsplit(tail(y, -1), split = "\t", fixed = TRUE))

  tbl <- y[-1, , drop = F]
  colnames(tbl) <- .format_title(y[1, ])
  tbl <- list(tbl)
  names(tbl) <- .format_title(x[1])

  return(tbl)
}


.format_list_csv2 <- function(x) {
  title <- .format_title(x[1])

  tbl <- t(.split_keyval(x[-1], only.first = FALSE))

  # table can be empty
  if (is.null(nrow(tbl))) {
    tbl <- NULL
  } else {
    header <- .format_title(tbl[1, ])
    tbl <- data.frame(tbl[-1, , drop = F], stringsAsFactors = FALSE)

    colnames(tbl) <- header
  }


  tbl <- list(tbl)
  names(tbl) <- title

  return(tbl)
}



.format_list_fwf <- function(x) {
  title <- .format_title(x[1])
  widths <- c(27, 20, 25)

  tbl <- lapply(x[-1], substring,
                first = cumsum(c(1, head(widths, -1))),
                last = cumsum(widths))

  tbl <- apply(do.call(rbind, tbl), 2, trimws)
  tbl <- tbl[, apply(tbl != "", 2, any)]

  # table can be empty
  if (is.null(nrow(tbl))) {
    tbl <- NULL
  } else {
    header <- .format_title(tbl[1, ])
    tbl <- data.frame(tbl[-1, , drop = F])

    colnames(tbl) <- header
  }


  tbl <- list(tbl)
  names(tbl) <- title

  return(tbl)
}

.format_title <- function(x) {
  # remove trailing ":"
  y <- sub(":[[:blank:]]*;?$", "", trimws(x))

  # remove everything inside round brackets
  y <- gsub("\\(.*?\\)", "", y)

  # remove everything inside square brackets
  y <- gsub("\\[.*?\\]", "", y)
  return(trimws(y))
}

.value <- function(x, name) x[x[, 1] == name, 2]
.listvalue <- function(x, lname, ename) {
  y <- x[[lname]][, ename, drop = F]
  y <- if(nrow(y) < 1) NA else tail(y, 1)

  return(y)
}


extract_meta <- function(x) {

  att1 <- attr(x, "keyval")
  att2 <- attr(x, "list")

  y <- data.frame(name = .value(att1, "Messstelle"),
                  id.hzb = as.numeric(.value(att1, "HZB-Nummer")),
                  departement = .value(att1, "Dienststelle"),
                  lon = dms2dec(.listvalue(att2, "Geographische Koordinaten", "L\u00e4nge")),
                  lat = dms2dec(.listvalue(att2, "Geographische Koordinaten", "Breite")),
                  z = .toNum(.listvalue(att2, "Pegelnullpunkt", "H\u00f6he")))

  rownames(y) <- NULL
  return(y)
}

extract_meta_xts <- function(x) {
  att <- xtsAttributes(x)
  meta <- att[setdiff(names(att), "coordinates")]
  coord <- att[["coordinates"]]
  meta[names(coord)] <- coord

  names(meta)[names(meta) == "station"] <- "name"

  return(as.data.frame(meta))
}




# append list info
nlast <- function(x, n = 1, col) {
  y <- if (!length(nrow(x))) rep(NA, length(col)) else tail(x[, col], n)
  if(length(dim(y))) apply(y, 2, as.character) else as.character(y)
}


regularize <- function(x, interval = "day") {
  fullseq <- seq(from = min(time(x)), to = max(time(x)), by = interval)
  missing <- fullseq[!fullseq %in% time(x)]

  if(length(missing)) {
    gaps <- xts(x = data.frame(discharge = rep_len(NA_real_, length(missing))), order.by = missing)
    x <- rbind(x, gaps)
  }

  return(x)
}


.toNum <- function(x) as.numeric(sub(",", ".", x))

hzb2xts <- function(x){
  x$time <- as.Date(format(x$time, format = "%Y-%m-%d"))
  x <- x[!duplicated(x$time), ]
  y <- xts(x = data.frame(discharge = x$value), order.by = x$time)
  y <- regularize(y)

  att <- attr(x, "keyval")
  coord <- numeric()
  coord[c("lon", "lat")] <- dms2dec(
    .listvalue(attr(x, "list"), "Geographische Koordinaten",
               c("L\u00e4nge", "Breite")))

  z <- .toNum(.listvalue(attr(x, "list"), "Pegelnullpunkt", "H\u00f6he"))

  dict <- c("Messstelle"="station", "HZB-Nummer"="id.hzb", "HD-Nummer"="id.hd",
            "DBMS-Nummer"="id.dbms", "Gew\u00e4sser"="river",
            "Messstellenbetreiber" = "operator", "Dienststelle" = "departement",
            "orogr.Einzugsgebiet"="catchment", "Einheit"="unit")

  keyval <- list()
  for(i in seq_along(dict)) keyval[[i]] <- .value(att, names(dict)[i])
  names(keyval) <- dict

  keyval[["id.hzb"]] <- as.numeric(keyval[["id.hzb"]])
  keyval[["catchment"]] <- .toNum(keyval[["catchment"]])
  keyval[["unit"]] <- sub("\u00B3", "^3",keyval[["unit"]])

  xtsAttributes(y) <- c(keyval, list(coordinates = coord, z = z))

  return(y)
}

dms2dec <- function(x, split = " ", ...){
  res <- rep(NA, length(x))

  if(!all(is.na(x))) {
    y <- strsplit(x[!is.na(x)], split = split, ...)
    z <- sapply(y, function(x)
      sum(as.numeric(x) * c(1, 1/60^seq_len(length(x) - 1))))
    res[!is.na(x)] <- z
  }

  return(res)
}

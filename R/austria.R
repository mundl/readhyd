read.hzb <- function(file, parse.header = TRUE) {
  # separate the header, open the connection with correct enconding
  fh <- file(file, open = "rt", encoding = "latin1")
  header <- readLines(fh, n = 50)
  close(fh)

  type <- if (grepl(";", header[1])) "csv2" else "fwf"

  lines.header <- grep("Werte:", header, fixed = T)
  header <- head(header, lines.header - 1)

  args <- list(file = file, header = F, skip = lines.header, na.strings = "L\u00fccke",
               strip.white = TRUE, as.is = TRUE, fileEncoding = "latin1")

  infile <- if(type == "fwf") {
    do.call(read.fwf, c(args, list(col.names = c("time", "value"),
                                   colClasses = c("character", "numeric"),
                                   widths = c(20, 20))))
  } else if (type == "csv2"){
    do.call(read.csv2, c(args, list(col.names = c("time", "value", NA),
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
  formatter <- match.fun(paste(".format_list_", type))
  header.list <- do.call(c, lapply(unname(header.list), formatter))

  return(list(keyval = keyval, list = header.list))
}


.split_keyval <- function(x, only.first = T) {
  # sometimes, only the first ";" is a field separator, replace it with "\t"
  x <- if(only.first) sub(";", "\t", x) else gsub(";", "\t", x)
  keyval <- do.call(rbind, strsplit(x, split = "\t"))
  keyval <- apply(keyval, 2, trimws)
  keyval <- apply(keyval, 2, sub, pattern = ":$", replacement = "")

  keyval[1, ] <- .format_title(keyval[1, ])

  return(keyval)
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
  y <- sub(":$", "", trimws(x))

  # remove everything inside round brackets
  y <- gsub("\\(.*?\\)", "", y)

  # remove everything inside square brackets
  y <- gsub("\\[.*?\\]", "", y)
  return(trimws(y))
}


extract_meta <- function(x) {
  .value <- function(x, name) x[x[, 1] == name, 2]

  att1 <- attr(x, "keyval")
  att2 <- attr(x, "list")
  data.frame(name = .value(att1, "Messstelle"),
             id.hzb = as.numeric(.value(att1, "HZB-Nummer")),
             departement = .value(att1, "Dienststelle"),
             lon = dms2deg(.value(att2[["Geographische Koordinaten"]], "Länge")),
             lat = dms2deg(.value(att2[["Geographische Koordinaten"]], "Breite")),
             z = as.numeric(att2[[grep("Höhenangaben", names(att2))]][1, "Geländehöhe"]))
}



# append list info
nlast <- function(x, n = 1, col) {
  y <- if (!length(nrow(x))) rep(NA, length(col)) else tail(x[, col], n)
  if(length(dim(y))) apply(y, 2, as.character) else as.character(y)
}


regularize <- function(x) {
  fullseq <- seq(from = min(time(x)), to = max(time(x)), by = "day")
  missing <- fullseq[!fullseq %in% time(x)]

  if(length(missing)) {
    gaps <- xts(x = data.frame(discharge = rep_len(NA_real_, length(missing))), order.by = missing)
    x <- rbind(x, gaps)
  }

  return(x)
}


hzb2xts <- function(x){
  x$time <- as.Date(format(x$time, format = "%Y-%m-%d"))
  x <- x[!duplicated(x$time), ]
  y <- xts(x = data.frame(discharge = x$value), order.by = x$time)
  y <- regularize(y)

  att <- attr(x, "keyval")
  coord <- dms2dec(nlast(attr(x, "list")$"Geographische Koordinaten")[2:3])
  names(coord) <- c("long", "lat")

  xtsAttributes(y) <- list(id.hzb = as.numeric(att[att[, 1] == "HZB-Nummer", 2]),
                           station = att[att[, 1] == "Messstelle", 2],
                           river = att[att[, 1] == "Gewässer", 2],
                           institution = att[att[, 1] == "Dienststelle", 2],
                           catchment = att[att[, 1] == "orogr.Einzugsgebiet [km²]", 2],
                           unit = sub("\u00B3", "^3", att[att[, 1] == "Einheit", 2]),
                           coordinates = coord,
                           z = as.numeric(nlast(attr(x, "list")$Pegelnullpunkt)[2]))

  return(y)
}

# this functions are duplicated in drought2015

### Belgium / Flanders ----
read.csv.be <- function(file, ...) {

  txt <-  readLines(file, warn = FALSE)

  lines.header <- grep("Timestamp;Value;Quality", head(txt, 50), fixed = T) - 1
  header <- head(txt, lines.header)

  infile <- read.csv2(text = txt, header = T, skip = lines.header,
                      colClasses = c("character", rep("numeric", 2), rep("NULL", 2)),
                      col.names = c("time", "value", "flag", NA, NA),
                      strip.white = TRUE, as.is = TRUE)

  time <- gsub(":00$|00.000", "00", infile$time)
  infile$time <- as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%S%z")

  return(infile[, c("time", "value")])
}


###  Norway ----
read.dat.no <- function(file, ...) {

  infile <- read.table(file, col.names = c("y", "m", "d", "value"),
                       colClasses = "numeric", na.strings = "-9999.000000", ...)

  time <- as.Date(do.call(paste, c(as.list(infile[, c("y", "m", "d")]),
                                   sep = "-")))

  return(data.frame(time = time, value = infile$value))
}


# UK ----
read.nrfa <- function(file, nlines = -1, ...) {
  txt <-  readLines(file)

  lines.header <- grep("data,last,", head(txt, 50), fixed = T)
  header <- head(txt, lines.header)

  infile <- read.csv(text = txt, header = F, skip = lines.header,
                     col.names = c("time", "value", "flag"),
                     colClasses = c("Date", "numeric", "NULL"),
                     strip.white = TRUE, as.is = TRUE, nrow = nlines)


  y <- infile[, c("time", "value")]


  meta <- .parse_header_nrfa(header)

  attr(y, "meta") <- list(
    eid = meta$station$id,
    station = sub(" at .+$", "", meta$station$name),
    river = sub("^.+ at ", "", meta$station$name),
    unit = sub("3", "^3", meta$dataType$units, fixed = TRUE),
    institution = meta$database$name,
    country = "gb",
    time = as.Date(unlist(meta$data[c("first", "last")], use.names = F)))

  return(y)
}

.parse_header_nrfa <- function(x, ...) {

  y <- read.csv(text = x, header = F, strip.white = TRUE,
                col.names = c("key", "component", "value"),
                colClasses = c("character"), ...)

  meta <- list()
  for(i in seq_len(nrow(y))) {
    meta[[y$key[i]]][[y$component[i]]] <- y$value[i]
  }

  return(lapply(meta, as.list))

}


### Spain ----
read.txt.es <- function(file, ...) {

  infile <- read.table(file, colClasses = c("numeric"),
                       col.names = c("eid", "day", "month", "year", "value"),
                       na.strings = c("-100"), ...)

  time <- as.Date(do.call(paste, c(as.list(infile[, c("year", "month", "day")]),
                                   sep = "-")))

  return(data.frame(time = time, value = infile$value))
}


### Germany / Bavaria ----
read.gkd <- function(file, fileEncoding = "ISO8859-1", ...) {

  x <- split_header(file = file,  regex = "Datum;Mittelwert;Maximum;Minimum;",
                    sep = ";", dec = ",", quiet = TRUE, skip = 1,
                    encoding = fileEncoding,
                    what = list(date = character(), mean = numeric(),
                                max = numeric(), min = numeric(),
                                flag = character()), strip.white = TRUE, ...)

  header <- .parse_header_gkd(x$header)
  body <- x$body
  body$date <- as.Date(body$date)

  res  <- data.frame(time = body$date, value = body$mean)

  attr(res, "meta") <- c(header, list(unit = "m^3/s", country = "de"))

  return(res)
}

.parse_header_gkd <- function(x) {

  y <- read.csv2(text = x, header = F, strip.white = TRUE, as.is = TRUE,
                 col.names = c("key", "value"),
                 colClasses = c("character"))

  y$key <- sub("\\.?:$", "", y$key)

  xy <- y$value[y$key == "Koordinaten"]
  xy <- as.numeric(strsplit(xy, "[^[:digit:].]+")[[1]][2:4])
  epsg <- xy[3]
  xy <- xy[1:2]
  names(xy) <- c("x", "y")
  z <- y$value[y$key == "Pegelnullpunktsh\u00f6he"]
  z <- as.numeric(sub(",", ".", gsub("[^[:digit:],]+", "", z)))

  res <- list(institution = y$value[y$key == "Quelle"],
              station = y$value[y$key == "Messstellen-Name"],
              eid = y$value[y$key == "Messstellen-Nr"],
              river = y$value[y$key == "Gew\u00e4sser"],
              region = "Bayern",
              coordinates = c(unlist(xy), z = z),
              epsg = epsg)

  return(res)
}
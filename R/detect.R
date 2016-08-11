library(lfstat)

# mutable state via lexical scooping
track_autodetect <- function(formatsOrdered) {
  list(
    append = function(current) {
      # find index of currently successful file format
      # todo: implement overall highscore
      idx <- which(current == names(formatsAvail))
      if(length(idx)){
        formatsOrdered <<- unique(c(idx, formatsOrdered))
      } else {
        warning("readhyd: type ", shQuote(current),
                " not registered in object 'formatsAvail'.", call. = FALSE)
      }},
    show   = function() formatsOrdered)
}


# track the last sucessfully detected file formats in a hidden object
.lastSuccess <- track_autodetect(seq_along(formatsAvail))

# .lastSuccess$show()
# .lastSuccess$append("grdc")
# .lastSuccess$append("gras")


# todo: there can be "dynamic" file formats: header specifies the data layout
isof_format <- function(format, file, check, nlines = -1, ...) {
  if(!is.function(check)) stop("argumnt 'check' must be a function.")

  txt <- readLines(file, n = nlines, ...)
  success <- check(txt)
  if(!is.logical(success)) stop("argumnt 'check' must return TRUE or FALSE.")

  if(success) {
    .lastSuccess$append(format)
  }

  return(success)
}


# every file format must have a function testing for match ----
is.grdcHeader <- function(lines) {
  any(grepl("GRDC STATION DATA FILE", lines, fixed = TRUE))
}

is.lfuHeader <- function(lines) {
  # header contains '|*|' as field separator
  any(grepl("#.*\\|\\*\\|", lines, fixed = FALSE))
}

is.grdcData <- function(file) {

}


# Registering all available formats ----
formatsAvail <- list("grdc" = list(check  = is.grdcHeader,
                                   import = lfstat:::read.grdc),
                     "lfu"  = list(check  = is.lfuHeader,
                                   import = lfstat:::read.lfu))



# Here comes the magic ----
guess_format <- function(file, formats = formatsAvail,
                         startWith = .lastSuccess$show(), nlines = 100) {
  for(idx in startWith) {
    currentFormat <- names(formats[idx])
    success <- isof_format(format = currentFormat, file = file,
                           check = formats[[idx]][["check"]], nlines = nlines)
    if(success) {
      .lastSuccess$append(currentFormat)
      return(currentFormat)
    }
  }
}


read.hyd <- function(file, format = guess_format(file), ...) {
  readlfdata(file, type = toupper(format), baseflow = FALSE, hyearstart = 1, ...)
}

read_file <- function(file, format = guess_format(file), ...) {
  import <- formats[[format]][["import"]]
  import(file)
}


# Testing
grdc <- system.file("samplesheets/9104020.day", package = "lfstat")
lfu <- system.file("samplesheets/oberammergau.dat", package = "lfstat")
hzb <- system.file("samplesheets/kloesterle.dat", package = "lfstat")

guess_format(grdc, nlines = 1)
guess_format(lfu)
guess_format(hzb)

a <- read.hyd(grdc, n = 100)
b <- read.hyd(lfu)

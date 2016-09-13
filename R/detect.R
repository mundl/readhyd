# mutable state via lexical scooping
track_autodetect <- function(allformats) {
  history <- character()

  list(
    append = function(current) {
      if(current %in% allformats){
        history <<- c(history, current)
      } else {
        warning("readhyd: type ", shQuote(current),
                " not registered in object 'allformatsAvail'.", call. = FALSE)
      }},
    history = function() unique(rev(history)),
    ordered =  function() unique(c(rev(history), allformats)),
    highscore = function() sort(table(history, dnn = NULL), decreasing = TRUE))
}

# track the last sucessfully detected file formats in a hidden object
.success <- track_autodetect(names(formatsAvail))

# todo: there can be "dynamic" file formats: header specifies the data layout
isof_format <- function(format, file, check, nlines = -1, ...) {
  if(!is.function(check)) stop("argumnt 'check' must be a function.")

  txt <- readLines(file, n = nlines, ...)
  success <- check(txt)
  if(!is.logical(success)) stop("argumnt 'check' must return TRUE or FALSE.")

  return(success)
}

# every file format must have a function testing for match ----
is.grdcHeader <- function(lines) {
  any(grepl("GRDC STATION DATA FILE", lines, fixed = TRUE, useBytes = TRUE))
}

is.lfuHeader <- function(lines) {
  # header contains '|*|' as field separator
  any(grepl("#.*\\|\\*\\|", lines, fixed = FALSE, useBytes = TRUE))
}

is.hzbHeader <- function(lines) {
  any(grepl("HZB-Nummer:", lines, fixed = TRUE, useBytes = TRUE))
}

is.nrfaHeader <- function(lines) {
  any(grepl("database,id,nrfa", lines, fixed = TRUE, useBytes = TRUE))
}

is.gkdHeader <- function(lines) {
  # file has an encoding different from utf8
 # any(grepl("Quelle:;\"Bayerisches Landesamt f\u00fcr Umwelt, www.gkd.bayern.de\"",
 #           lines, fixed = TRUE))
  any(grepl("Bayerisches Landesamt f",
                       lines, useBytes = TRUE))
}

is.vardat2Header <- function(lines) {
  any(grepl("####################### 2 ########################",
            lines, fixed = TRUE, useBytes = TRUE))
}

is.grdcData <- function(file) {

}

# Registering all available formats ----
formatsAvail <- list(
  "grdc" = list(check = is.grdcHeader, import = read.grdc),
  "lfu" = list(check = is.lfuHeader, import = read.lfu),
  "hzb" = list(check = is.hzbHeader, import = read.hzb),
  "nrfa" = list(check = is.nrfaHeader, import = read.nrfa),
  "gkd" = list(check = is.gkdHeader, import = read.gkd),
  "vardat2" = list(check = is.vardat2Header, import = read.vardat2))

# Here comes the magic ----
guess_format <- function(file, formats = formatsAvail,
                         startWith = .success$ordered(), nlines = 100) {

  # todo: better open file outside of loop
  for(idx in startWith) {
    currentFormat <- names(formats[idx])
    success <- isof_format(format = currentFormat, file = file,
                           check = formats[[idx]][["check"]], nlines = nlines)
    if(success) {
      # probably safer to determine after import
      .success$append(currentFormat)
      return(currentFormat)
    }
  }
}

# read.hyd <- function(file, format = guess_format(file), ...) {
#   require(lfstat)
#   readlfdata(file, type = toupper(format), baseflow = FALSE, hyearstart = 1,
#              ...)
# }

read.hyd <- function(file, format = guess_format(file), formats = formatsAvail,
                      ...) {
  import <- formats[[format]][["import"]]
  x <- try(import(file, ...))

  if(inherits(x, what = "try-error")) {
    stop("import not successful")
  }

  # append file format to attributes

  check_import(x)
}

check_import <- function(x) {
  # index is time based and doesn't have too many NAs
  # value column is.numeric and doesn't have too many NAs
  # has attributes

  # if successful, .success$append("grdc")

  return(x)
}

# .success$history()
# .success$append("grdc")
# .success$append("gras")
# .success$highscore()
# .success$ordered()


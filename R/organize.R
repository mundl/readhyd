.onAttach <- function(libname, pkgname){
  .success <- track_autodetect(names(formatsAvail))
}

list_formats <- function(x) {
  files <- list.files(system.file("examples", package = "readhyd"),
                      recursive = TRUE, full.names = TRUE)
  hasHeader <- !grepl("/noheader/", files)


  format <- sub("_.*", "", basename(files))
  format[!hasHeader] <- NA

  return(data.frame(file = files, format = format, hasHeader,
                    stringsAsFactors = FALSE))
}

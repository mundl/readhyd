context("Autodetect known formats by header")

# just keep file formats with header
ex <- na.omit(list_formats())

test_that("header are recognized", {
  for(i in seq_len(nrow(ex))) {
    expect_equal(object = guess_format(ex$file[i]),
                 expected = ex$format[i],
                 info = ex$file[i])
  }
})

context("Automatically testing file import")

test_that("read.hyd works for all formats", {
  for(i in seq_len(nrow(ex))) {
    # some sample files just have a few lines
    n <- 200
    filename <- ex$file[i]

    # import functions accept the nlines argument
    expect_silent(infile <- read.hyd(file = filename, nlines = n))

    # number of lines is correct
    expect_equal(object = nrow(infile), expected = n, info = filename)

    # correct classes
    expect_silent(check_colClasses(infile))
  }
})

context("Manually testing file import")

# just keep file formats with header
ex <- na.omit(list_formats())


test_that("read.lfu works", {
  filename <- ex$file[ex$format == "lfu"]
  infile <- read.lfu(filename, nlines = 1000)

  # we catch the first line of data
  expect_equal(infile$flow[1], 2.55)
  expect_equal(infile$time[1], as.Date("1920-11-01"))

  # NAs are detected
  expect_equal(infile$flow[2], NA_real_)

  # has metadata
  meta <- attr(infile, "meta")
  expected <- c("SANR" = "16610709", "SNAME" = "Oberammergau",
                "SWATER" = "Ammer", "CMW1" = "", "RNR1" = "", "RID" = "-1")

  expect_equal(meta, expected)
})


test_that("read.gkd works", {
  filename <- ex$file[ex$format == "gkd"]
  infile <- read.gkd(filename, nlines = 1000)

  # we catch the first line of data
  expect_equal(infile$value[1], 0.32)
  expect_equal(infile$time[1], as.Date("1978-11-01"))

  # has metadata
  meta <- attr(infile, "meta")
  expected <- structure(list(
    institution = "Bayerisches Landesamt f\u00fcr Umwelt, www.gkd.bayern.de",
    station = "K\u00f6fering", eid = "15325004", river = "Pfatter",
    region = "Bayern",
    coordinates = structure(c(4514789, 5422664, 338.03),
                            .Names = c("x", "y", "z")),
    epsg = 31468, unit = "m^3/s",  country = "de"),
    .Names = c("institution", "station", "eid", "river", "region",
               "coordinates", "epsg", "unit", "country"))

  expect_equal(meta, expected)
})


test_that("read.grdc works", {
  filename <- ex$file[ex$format == "grdc"]
  infile <- read.grdc(filename, nlines = 1000)

  # we catch the first line of data
  expect_equal(infile$original[1], 78)
  expect_equal(infile$time[1], as.Date("1887-11-01"))
})


test_that("read.vardat2 works", {
  filename <- ex$file[ex$format == "vardat2"]
  infile <- read.vardat2(filename, nlines = 1000)

  # we catch the first line of data
  expect_equal(infile$discharge[1], NA_real_)
  expect_equal(infile$time[1], as.POSIXct("1973-12-10 12:00"))

  expect_equal(infile$discharge[5], 10.760045)
  expect_equal(infile$time[5], as.POSIXct("1973-12-14 12:00"))
})


test_that("read.nrfa works", {
  filename <- ex$file[ex$format == "nrfa"]

  # file 1
  infile <- read.nrfa(filename[1], nlines = 1000)

  # we catch the first line of data
  expect_equal(infile$value[1], 0.049)
  expect_equal(infile$time[1], as.Date("1970-07-01"))

  # has metadata
  meta <- attr(infile, "meta")
  expected <- structure(
    list(eid = "39065", station = "Ewelme Brook", river = "Ewelme",
         unit = "m^3/s", institution = "National River Flow Archive",
         country = "gb", time = structure(c(181, 16343), class = "Date")),
    .Names = c("eid", "station", "river", "unit", "institution", "country",
               "time"))

  expect_equal(meta, expected)


  # file 2
  infile <- read.nrfa(filename[2], nlines = 1000)

  # we catch the first line of data
  expect_equal(infile$value[1], 1.586)
  expect_equal(infile$time[1], as.Date("1989-05-18"))

  # has metadata
  meta <- attr(infile, "meta")
  expected <- structure(
    list(eid = "39105", station = "Thame", river = "Wheatley",  unit = "m^3/s",
         institution = "National River Flow Archive", country = "gb",
         time = structure(c(7077, 16343), class = "Date")),
    .Names = c("eid", "station", "river", "unit", "institution", "country",
               "time"))

  expect_equal(meta, expected)
})


test_that("read.hzb works", {
  filename <- ex$file[ex$format == "hzb"]

  # file 1
  infile <- read.hzb(filename[1], nlines = 1000)

  # we catch the first line of data
  expect_equal(infile$value[1], 0.574)
  expect_equal(infile$time[1], as.POSIXct("2011-01-01"))

  # has metadata
  meta <- attr(infile, "keyval")
  expected <- structure(
    c("Messstelle", "HZB-Nummer", "HD-Nummer", "DBMS-Nummer",
      "Gew\u00e4sser", "Sachgebiet", "Dienststelle", "Messstellenbetreiber",
      "orogr.Einzugsgebiet", "Ursprungszeitreihe", "Transformation",
      "Mittelungsintervall", "Exportqualit\u00e4t", "Einheit", "Exportzeitraum",
      "Werteformat", "Altschlaining", "210245", "", "1001022", "Tauchenbach",
      "OWF", "HD-Burgenland", "Hydrographischer Dienst", "89,20",
      "Abfluss,K,,,0,F,Z,0,,, - Hauptreihe", "Mittel", "1 Stunde", "MAXQUAL",
      "m\u00B3/s", "01.01.2011 00:00; bis ;01.10.2015 00:00", "3 Nachkommast."),
    .Dim = c(16L, 2L))

  expect_equal(meta, expected)



  # file 2
  infile <- read.hzb(filename[2], nlines = 1000)

  # we catch the first line of data
  expect_equal(infile$value[1], 11.2)
  expect_equal(infile$time[1], as.POSIXct("1991-11-01"))

  # has metadata
  meta <- attr(infile, "keyval")
  expected <- structure(
    c("Messstelle", "HZB-Nummer", "HD-Nummer", "DBMS-Nummer",
      "PorenGW-Gebiet", "Grundwasserk\u00f6rper", "Sachgebiet", "Arbeitsgebiet",
      "Dienststelle", "Messstellenbetreiber", "Exportzeitreihe",
      "Exportqualit\u00e4t", "Einheit", "Exportzeitraum", "Werteformat",
      "Ebental, Blt 223", "338715", "", "2002137", "442 Klagenfurter Becken",
      "Klagenfurter Becken [DRA]", "UWQ", "GW", "HD-K\u00e4rnten", "",
      "GWTEP,I,Mit,M,1,A,Z,0,,,", "MAXQUAL", "\u00B0C",
      "01.01.1966 00:00; bis ;01.01.2016 00:00", "1 Nachkommast."),
    .Dim = c(15L, 2L))

  expect_equal(meta, expected)


  # file 3
  infile <- read.hzb(filename[3], nlines = 1000)

  # we catch the first line of data
  expect_equal(infile$value[1], 0.553)
  expect_equal(infile$time[1], as.POSIXct("2012-01-01"))

  # has metadata
  meta <- attr(infile, "keyval")
  expected <- structure(
    c("Messstelle", "HZB-Nummer", "HD-Nummer", "DBMS-Nummer",
      "Gew\u00e4sser", "Sachgebiet", "Dienststelle", "Messstellenbetreiber",
      "orogr.Einzugsgebiet", "Ursprungszeitreihe", "Transformation",
      "Mittelungsintervall", "Exportqualit\u00e4t", "Einheit", "Exportzeitraum",
      "Werteformat", "Zwettl (Sportplatzbr\u00fccke)", "207936", "no260",
      "3001033", "Zwettl", "OWF", "HD-Nieder\u00f6sterreich",
      "Hydrographischer Dienst", "269.10",
      "Abfluss,K,,,0,F,Z,0,,, - Hauptreihe", "Mittel", "1 Tag", "MAXQUAL",
      "m\u00B3/s", "01.01.2012 00:00 bis 01.01.2016 00:00", "3 Nachkommast."),
    .Dim = c(16L, 2L))

  expect_equal(meta, expected)
})



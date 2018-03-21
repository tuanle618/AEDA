library(testthat)

if (identical(Sys.getenv("TRAVIS"), "true")) {
  test_check("../AEDA", filter = "base_")
} else {
  temp.wd = getwd()
  dir.create("TestFolder")
  setwd("TestFolder/")
  try(test_check("../AEDA", filter = "base_"))
  setwd(temp.wd)
  unlink("TestFolder", recursive = TRUE)
}


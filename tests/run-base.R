library(testthat)
temp.wd = getwd()
dir.create("TestFolder")
setwd("TestFolder/")
if (identical(Sys.getenv("TRAVIS"), "true")) {
  test_check("../AEDA", filter = "base_")
} else {
  try(test_check("../AEDA", filter = "base_"))
}
setwd(temp.wd)
unlink("TestFolder", recursive = TRUE)

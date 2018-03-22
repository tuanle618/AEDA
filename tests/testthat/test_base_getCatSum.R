context("getCatSum")
arthritis = data("Arthritis", package = "vcd")
arthritis = get(arthritis)
arthritis[5, 2] = NA
arthritis[8, 3] = NA
test_that("", {
  cat.sum = getCatSum(data = arthritis, target = "Improved", features = c("Treatment", "Sex"), geombar.args = list())
  expect_length(cat.sum, 6L)
  expect_named(cat.sum, expected = c("freq", "rel.freq", "nas", "contg.list", "rel.contg.list", "plot.list"))
  expect_list(cat.sum$freq)
  expect_equal(as.vector(cat.sum$rel.freq$`1`), c(43, 40) / (43 + 40))
  expect_equal(as.vector(cat.sum$rel.freq$`2`), c(59, 24) / (59 + 24))
  expect_equal(as.numeric(cat.sum$nas), c(1, 1))
  expect_equal(cat.sum$contg.list[[1]][3, 3] + sum(as.numeric(cat.sum$nas)),
    nrow(arthritis))
  expect_equal(cat.sum$rel.contg.list[[1]], cat.sum$contg.list[[1]] / 82)
  expect_length(cat.sum$plot.list, 2L)
})

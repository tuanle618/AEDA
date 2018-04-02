context("getDataType")
library("MASS")

test_that("getDataType with fake Data", {
  # save test Data uniformily
  test.data = data.frame(int = 1L, num = 3.5, string = "Hello",
    fac = factor("A", levels = c("A", "B")), date = Sys.Date(),
    ord = ordered(1), log = TRUE,
    stringsAsFactors = FALSE)

  # Apply getDataType funktion
  data.types = getDataType(test.data, target = NULL)
  # Are all col names in the data.type object
  expectIdentical(data.types$X, names(test.data))
  # Are all columns sorted into the different classes
  expectIdentical(length(unlist(data.types[!names(data.types) %in% c("X", "target")])),
    length(data.types$X))
  # Has the object correct length
  expectIdentical(length(data.types), 9L)
  # Are the list entries all strings
  not.null.data.types = data.types[!names(data.types) %in% "target"]
  for (i in names(not.null.data.types)) expect_character(data.types[[i]])
  # Are the data types correctly sorted
  for (i in data.types$num) expect_numeric(test.data[[i]])
  for (i in data.types$int) expect_integer(test.data[[i]])
  for (i in data.types$ord) expect_class(test.data[[i]], "ordered")
  for (i in data.types$fact) expect_factor(test.data[[i]])
  for (i in data.types$char) expect_character(test.data[[i]])
  for (i in data.types$logic) expect_logical(test.data[[i]])
  for (i in data.types$date) expect_date(test.data[[i]])
})


test_that("getDataType with Aids2 Data", {
  # save test Data uniformily
  test.data = data("Aids2", package = "MASS")
  test.data = get(test.data)
  # Apply getDataType funktion
  data.types = getDataType(test.data, target = NULL)
  # Are all col names in the data.type object
  expectIdentical(data.types$X, names(test.data))
  # Are all columns sorted into the different classes
  expectIdentical(length(unlist(data.types[!names(data.types) %in% c("X", "target")])),
    length(data.types$X))
  # Has the object correct length
  expectIdentical(length(data.types), 9L)
  # Are the list entries all strings
  not.null.data.types = data.types[!names(data.types) %in% "target"]
  for (i in names(not.null.data.types)) expect_character(data.types[[i]])
  # Are the data types correctly sorted
  for (i in data.types$num) expect_numeric(test.data[[i]])
  for (i in data.types$int) expect_integer(test.data[[i]])
  for (i in data.types$ord) expect_class(test.data[[i]], "ordered")
  for (i in data.types$fact) expect_factor(test.data[[i]])
  for (i in data.types$char) expect_character(test.data[[i]])
  for (i in data.types$logic) expect_logical(test.data[[i]])
  for (i in data.types$date) expect_date(test.data[[i]])
})

test_that("getDataType with Boston Data", {
  # save test Data uniformily
  test.data = data("Boston", package = "MASS")
  test.data = get(test.data)
  # Apply getDataType funktion
  data.types = getDataType(test.data, target = NULL)
  # Are all col names in the data.type object
  expectIdentical(data.types$X, names(test.data))
  # Are all columns sorted into the different classes
  expectIdentical(length(unlist(data.types[!names(data.types) %in% c("X", "target")])),
    length(data.types$X))
  # Has the object correct length
  expectIdentical(length(data.types), 9L)
  # Are the list entries all strings
  not.null.data.types = data.types[!names(data.types) %in% "target"]
  for (i in names(not.null.data.types)) expect_character(data.types[[i]])
  # Are the data types correctly sorted
  for (i in data.types$num) expect_numeric(test.data[[i]])
  for (i in data.types$int) expect_integer(test.data[[i]])
  for (i in data.types$ord) expect_class(test.data[[i]], "ordered")
  for (i in data.types$fact) expect_factor(test.data[[i]])
  for (i in data.types$char) expect_character(test.data[[i]])
  for (i in data.types$logic) expect_logical(test.data[[i]])
  for (i in data.types$date) expect_date(test.data[[i]])
})

test_that("getDataType with Insurance Data", {
  # save test Data uniformily
  test.data = data("Insurance", package = "MASS")
  test.data = get(test.data)
  # Apply getDataType funktion
  data.types = getDataType(test.data, target = NULL)
  # Are all col names in the data.type object
  expectIdentical(data.types$X, names(test.data))
  # Are all columns sorted into the different classes
  expectIdentical(length(unlist(data.types[!names(data.types) %in% c("X", "target")])),
    length(data.types$X))
  # Has the object correct length
  expectIdentical(length(data.types), 9L)
  # Are the list entries all strings
  not.null.data.types = data.types[!names(data.types) %in% "target"]
  for (i in names(not.null.data.types)) expect_character(data.types[[i]])
  # Are the data types correctly sorted
  for (i in data.types$num) expect_numeric(test.data[[i]])
  for (i in data.types$int) expect_integer(test.data[[i]])
  for (i in data.types$ord) expect_class(test.data[[i]], "ordered")
  for (i in data.types$fact) expect_factor(test.data[[i]])
  for (i in data.types$char) expect_character(test.data[[i]])
  for (i in data.types$logic) expect_logical(test.data[[i]])
  for (i in data.types$date) expect_date(test.data[[i]])
})

test_that("getDataType with survey Data", {
  # save test Data uniformily
  test.data = data("survey", package = "MASS")
  test.data = get(test.data)
  # Apply getDataType funktion
  data.types = getDataType(test.data, target = NULL)
  # Are all col names in the data.type object
  expectIdentical(data.types$X, names(test.data))
  # Are all columns sorted into the different classes
  expectIdentical(length(unlist(data.types[!names(data.types) %in% c("X", "target")])),
    length(data.types$X))
  # Has the object correct length
  expectIdentical(length(data.types), 9L)
  # Are the list entries all strings
  not.null.data.types = data.types[!names(data.types) %in% "target"]
  for (i in names(not.null.data.types)) expect_character(data.types[[i]])
  # Are the data types correctly sorted
  for (i in data.types$num) expect_numeric(test.data[[i]])
  for (i in data.types$int) expect_integer(test.data[[i]])
  for (i in data.types$ord) expect_class(test.data[[i]], "ordered")
  for (i in data.types$fact) expect_factor(test.data[[i]])
  for (i in data.types$char) expect_character(test.data[[i]])
  for (i in data.types$logic) expect_logical(test.data[[i]])
  for (i in data.types$date) expect_date(test.data[[i]])
})

#' Tests for import_lizard_data function

test_that("import does not work without path", {
  expect_error(import_lizard_data(), regexp = "argument \"path\" is missing, with no default")
})

test_that("importing a data sets results in data frame", {
  expect_output(str(import_lizard_data(path = "../../fedes processed data/Dataset_nolog_bifpr_nosoil.csv")),
                "data.frame")
})

test_that("importing data with default species prints whole data set", {
  data <- read.csv("../../fedes processed data/Dataset_nolog_bifpr_nosoil.csv")
  data1 <- import_lizard_data(path = "../../fedes processed data/Dataset_nolog_bifpr_nosoil.csv")
  expect_identical(data1, data)
})

test_that("when selecting species, extra levels are dropped", {
  path <- "../../fedes processed data/Dataset_nolog_bifpr_nosoil.csv"
  data <- import_lizard_data(path = path)
  species <- levels(data$Species)
  for (spec in species) {
    data <- import_lizard_data(path = path, species = spec)
    expect_length(levels(data$Species), 1)
  }
})

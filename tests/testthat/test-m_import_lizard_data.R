#' Tests for import_lizard_data function

context("m_import_lizard_data")

test_that("import does not work without path", {
  expect_error(m_import_lizard_data(), regexp = "argument \"path\" is missing, with no default")
})

test_that("importing a data set results in data frame", {
  expect_output(str(m_import_lizard_data(path = "example_lizard_data.csv")),
                "data.frame")
})

test_that("importing data with default species prints whole data set", {
  data <- read.csv("example_lizard_data.csv")
  data1 <- m_import_lizard_data(path = "example_lizard_data.csv")
  expect_identical(data1, data)
})

test_that("when selecting species, extra levels are dropped", {
  path <- "example_lizard_data.csv"
  data <- m_import_lizard_data(path = path)
  species <- levels(data$Species)
  for (spec in species) {
    data <- m_import_lizard_data(path = path, species = spec)
    expect_length(levels(data$Species), 1)
  }
})

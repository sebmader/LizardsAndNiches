#' Tests for import_lizard_data function

context("m_import_lizard_data")

test_that("import does not work without path", {
  expect_error(m_import_lizard_data(), regexp = "argument \"path\" is missing, with no default")
})

test_that("importing a data set results in data frame", {
  expect_output(str(m_import_lizard_data(path = "example_lizard_data.csv")),
                "data.frame")
})

test_that("default for species parameter includes all species", {
  data <- read.csv("example_lizard_data.csv")
  specieslvl <- levels(data$Species)
  data1 <- m_import_lizard_data(path = "example_lizard_data.csv")
  specieslvl1 <- levels(data1$Species)
  expect_identical(specieslvl, specieslvl1)
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

test_that("juveniles are dropped from dataset", {
  path <- "example_lizard_data.csv"
  data <- m_import_lizard_data(path = path)
  expect_equal(which(data$SEX == "J"), integer(0))
})

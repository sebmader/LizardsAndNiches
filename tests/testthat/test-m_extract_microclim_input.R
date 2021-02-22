context("Testing extracting microclimate input data")

test_that("Without parameters, it will through error", {
  expect_error(m_extract_microclim_input(), regexp = "argument .* missing")
})

test_that("With default location parameter outputs all locations", {
  loc_data <- read.csv("example_coordinates.csv")
  expect_warning(loc_row <- m_extract_microclim_input(loc_data = loc_data))
  expect_equal(length(loc_data), length(loc_row))
})

test_that("Default 'individual' parameter doesn't give error with location average data", {
  loc_data <- read.csv("example_coordinates.csv")
  loc_data <- loc_data[-which(is.na(loc_data$Spec.Heat) | is.na(loc_data$SREF)),]
  loc_row <- m_extract_microclim_input(loc_data = loc_data)
  expect_equal(loc_row$LID, loc_data$LID)
})

test_that("Default 'individual' parameter does throw error with individual data", {
  loc_data <- read.csv("example_coordinates.csv")
  loc_data <- loc_data[-which(is.na(loc_data$Spec.Heat) | is.na(loc_data$SREF)),]
  expect_error(loc_row <- m_extract_microclim_input(loc_data = loc_data, individual = "121"))
})

test_that("Does give warning for missing Spec Heat and soil refl value", {
  loc_data <-  read.csv("example_coordinates.csv")
  expect_warning(m_extract_microclim_input(location = "CHA", loc_data = loc_data))
})

test_that("Output has numeric values in 'Nature' column", {
  loc_data <- read.csv("example_coordinates.csv")
  expect_warning(loc_row <- m_extract_microclim_input(loc_data = loc_data))
  expect_true(is.numeric(loc_row$Nature))
})

test_that("Specifying individual outputs exactly one row from individual locaiton data", {
  loc_data <- read.csv("example_coordinates_ind.csv")
  loc_row <- m_extract_microclim_input(loc_data = loc_data, individual = "101")
  expect_equal(length(loc_row$ID), 1)
})


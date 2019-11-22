context("Testing extracting microclimate input data")

test_that("Without parameters, it will through error", {
  expect_error(m_extract_microclim_input(), regexp = "argument .* missing")
})

test_that("With default location parameter outputs all locations", {
  loc_data <- read.csv("example_coordinates.csv")
  loc_row <- m_extract_microclim_input(loc_data = loc_data)
  expect_equal(length(loc_data), length(loc_row))
})

test_that("Output is has numeric values in 'Nature' column", {
  loc_data <- read.csv("example_coordinates.csv")
  loc_row <- m_extract_microclim_input(loc_data = loc_data)
  expect_true(is.numeric(loc_row$Nature))
})

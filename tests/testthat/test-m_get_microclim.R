context("Tests for modelling the microclimate")

test_that("Throws error when not supplied with location data", {
  expect_error(m_get_microclim(), regexp = "argument \"loc_row\" is missing")
})

{
  loc_data <- read.csv("example_coordinates.csv")
  loc_row <- m_extract_microclim_input("DAR", loc_data = loc_data)
  micro <- m_get_microclim(loc_row = loc_row)

  test_that("Outputs list", {
    expect_true(is.list(micro))
  })

  test_that("With default nyears, the model is run for 1 year with 365 days each with 24 h", {
    expect_equal(micro$nyears, 1)
    expect_equal(micro$ndays, 12)
    expect_equal(length(micro$metout[,1]), 12 * 24)
  })

  test_that("Changing number of years as input works", {
    nyears <- 2
    micro <- m_get_microclim(loc_row = loc_row, nyears = nyears)
    expect_equal(micro$nyears, nyears)
    expect_equal(micro$ndays, nyears * 12)
    expect_equal(length(micro$metout[,1]), nyears * 12 * 24)
  })

  test_that("Changing number of days as input works", {
    ndays <- 24
    micro <- m_get_microclim(loc_row = loc_row, ndays = ndays)
    expect_equal(micro$ndays, ndays)
    expect_equal(length(micro$metout[,1]), ndays * 24)
  })

  test_that("Combination of 1 year and 48 days (4 per month) causes warning", {
    ndays <- 48
    expect_warning(micro <- m_get_microclim(loc_row = loc_row, ndays = ndays))
  })
}


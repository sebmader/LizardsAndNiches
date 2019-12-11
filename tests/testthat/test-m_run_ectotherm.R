context("Testing functionality of running ectotherm model")

{
  loc_data <- read.csv("example_coordinates.csv")
  loc_row <- m_extract_microclim_input("DAR", loc_data = loc_data)
  micro <- m_get_microclim(loc_row = loc_row)
  params <- read.csv("example_physio.csv")
  ecto <- m_run_ectotherm(param = params[1,], micro = micro)

  test_that("outputs list", {
    expect_true(is.list(ecto))
  })

  test_that("ndays and nyears are correctly implemented", {
    expect_equal(length(ecto$rainfall), micro$ndays * micro$nyears)
    # expect_equal(length(ecto$foodlevels, micro$ndays * micro$nyears))
  })

  test_that("if 'burrow' is not allowed, ecto does not allow shelter underground", {
    expect_equal(sum(ecto$environ[,8]), 0)
    expect_equal(ecto$burrow, 0)
  })
}

test_that("ectotherm functions fails for more than 1 year (bug in NicheMapR)", {
  loc_data <- read.csv("example_coordinates.csv")
  loc_row <- m_extract_microclim_input("DAR", loc_data = loc_data)
  micro <- m_get_microclim(loc_row = loc_row, nyears = 2)
  params <- read.csv("example_physio.csv")
  expect_error(ecto <- m_run_ectotherm(param = params[1,], micro = micro))
})

{
  loc_data <- read.csv("example_coordinates.csv")
  loc_row <- m_extract_microclim_input("DAR", loc_data = loc_data)
  micro <- m_get_microclim(loc_row = loc_row, ndays = 365)
  params <- read.csv("example_physio.csv")
  ecto <- m_run_ectotherm(param = params[1,], micro = micro)

  test_that("ndays and nyears are correctly implemented", {
    expect_equal(length(ecto$rainfall), micro$ndays * micro$nyears)
    # expect_equal(length(ecto$foodlevels, micro$ndays * micro$nyears))
  })
}

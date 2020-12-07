context("Testing functionality of running ectotherm model")

{
  species <- "Karusasaurus_polyzonus"
  loc_data <- read.csv("example_coordinates.csv")
  loc_row <- m_extract_microclim_input("DAR", loc_data = loc_data)
  micro <- m_get_microclim(loc_row = loc_row)
  params <- read.csv("example_physio_spec.csv")
  params$ttl <- 123
  params$ww <- 34
  params$absorp <- 0.9
  ecto <- m_run_ectotherm(param = params[which(params$Species == species),], micro = micro)

  test_that("outputs list", {
    expect_true(is.list(ecto))
  })

  test_that("ndays and nyears are correctly implemented", {
    expect_equal(length(ecto$rainfall), micro$ndays * micro$nyears)
    # expect_equal(length(ecto$foodlevels, micro$ndays * micro$nyears))
  })

  test_that("if 'burrow' is not allowed, ecto does not allow shelter underground", {
    expect_equal(sum(ecto$environ[,8]), 0)
    expect_false(ecto$burrow)
  })


  # test_that("ectotherm functions fails for more than 1 year (bug in NicheMapR)", {
  #   micro <- m_get_microclim(loc_row = loc_row, nyears = 2)
  #   expect_error(ecto <- m_run_ectotherm(params[which(params$Species == species),], micro = micro))
  # }) # they fixed it...!!!

  {
    micro <- m_get_microclim(loc_row = loc_row, ndays = 365)
    ecto <- m_run_ectotherm(params[which(params$Species == species),], micro = micro)

    test_that("ndays and nyears are correctly implemented", {
      expect_equal(length(ecto$rainfall), micro$ndays * micro$nyears)
      # expect_equal(length(ecto$foodlevels, micro$ndays * micro$nyears))
    })
  }
}

context("Testing wrapper for microclimate and biophysical models")

test_that("creates list with one list per location with default", {
  liz_data <- m_import_lizard_data(species = "Karusasaurus_polyzonus")
  n_loc <- length(unique(liz_data$LID))
  bio <- m_run_biophysical()
  expect_equal(length(bio), n_loc)
})

test_that("when run per individual, each list of a location contains the as many lists as
          individuals are at that location (in the dataframe)", {
  liz_data <- m_import_lizard_data(species = "Karusasaurus_polyzonus")
  locations <- unique(liz_data$LID)
  bio <- m_run_biophysical(loc_mean = FALSE)
  for(loc in locations) {
    n_ind <- length(liz_data$ID[which(liz_data$LID == loc)])
    expect_equal(length(bio[[loc]]), n_ind)
  }
})

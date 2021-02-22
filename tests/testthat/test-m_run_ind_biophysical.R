context("Testing wrapper for individual microclimate and biophysical models")

{
  species <- "Karusasaurus_polyzonus"
  liz_file <- "example_lizard_data_ind.csv"
  physio <- "example_physio_spec.csv"
  loc_file <- "example_coordinates_ind.csv"


  test_that("creates list with one list per location with default", {
    liz_data <- m_import_lizard_data(path = liz_file, species = species)
    n_loc <- length(unique(liz_data$LID))
    bio <- m_run_ind_biophysical(liz_file = liz_file,
                             physio_file = physio,
                             loc_file = loc_file,
                             species = species)
    expect_equal(length(bio), n_loc)
  })

  test_that("error message if species is not in dataframe", {
    species <- "sdgfgh"
    expect_error(bio <- m_run_ind_biophysical(liz_file = liz_file,
                                          physio_file = physio,
                                          loc_file = loc_file,
                                          species = species),
                 regexp = "The species you are looking for is not existent in this data frame.")
  })

  test_that("when run per individual, each list of a location contains the as many lists as
          individuals are at that location (in the dataframe)", {
            liz_data <- m_import_lizard_data(liz_file,
                                             species = species)
            locations <- unique(liz_data$LID)
            bio <- m_run_ind_biophysical(liz_file = liz_file,
                                     physio_file = physio,
                                     loc_file = loc_file,
                                     species = species)
            for(loc in locations) {
              n_ind <- length(liz_data$ID[which(liz_data$LID == loc)])
              expect_equal(length(bio[[loc]]), n_ind)
            }
          })

  test_that("does use default value from NicheMapR for spec heat capacity and soil reflectance,
            when no values are provided", {
              species <- "Cordylus_cordylus"
              expect_warning(bio <- m_run_ind_biophysical(liz_file = liz_file,
                                                      physio_file = physio,
                                                      loc_file = loc_file,
                                                      species = species))
            })

  test_that("elevation is added to final list", {
    liz_data <- m_import_lizard_data(liz_file, species = species)
    # individual analysis
    bio <- m_run_ind_biophysical(liz_file = liz_file,
                             physio_file = physio,
                             loc_file = loc_file,
                             species = species)
    expect_true(is.numeric(bio[[1]][[1]]$elev))
  })

  test_that("species in final list", {
    liz_data <- m_import_lizard_data(species = species)
    # individual analysis
    bio <- m_run_ind_biophysical(liz_file = liz_file,
                             physio_file = physio,
                             loc_file = loc_file,
                             species = species)
    expect_true(is.character(bio[[1]][[1]]$species))
  })
}

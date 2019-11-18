#' @title m_run_climate_analysis
#' @description This function runs a full analysis of the climate niche starting with importing
#' our standard dataframe, running microclimate model and ectotherm function subsequently.
#' @name m_run_climate_analysis
#' @param liz_file A file with lizard data (see example_lizard_data.csv) containing at least
#' one individual with location, morphology and reflectance data.
#' @param species The species to be looked at, which will be selected from the data frame and
#' split into sub groups of morphs if present.
#' @param loc_file File with information on locations (see example_coordinates.csv).
#' @return List of lists with output if ectotherm function for each species/population/entity
#' @export


m_run_climate_analysis <- function(liz_file = "example_lizard_data.csv",
                                   species = "",
                                   loc_file = "example_coordinates.csv") {

  # import dataset from file
  data <- m_import_lizard_data(path = liz_file, species = species)

  # extract locations
  locations <- data$LID

  # import location information from file and run microclimate model at these locations
  loc_data <- read.csv(loc_file)
  micro_list <- split(locations, locations)
  ecto_list <- micro_list
  for(loc in micro_list) {
    micro_list[[loc]] <- list()
    loc_row <- m_extract_microclim_input(location = loc, loc_data = loc_data)
    loc <- m_get_microclim(loc_row = loc_row)
  }
  ecto_input <- read.csv("Physio_summary.csv")
  for(loc in ecto_list) {
    ecto_list[[loc]] <- list()
    # m_extract_ectotherm_input()
    ecto_input
  }

}

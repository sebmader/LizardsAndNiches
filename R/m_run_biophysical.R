#' @title m_run_biophysical
#' @description This function runs a full analysis of the climate niche starting with importing
#' our standard dataframe, running microclimate model and ectotherm function subsequently.
#' @name m_run_biophysical
#' @param liz_file A file with lizard data (see example_lizard_data.csv) containing at least
#' one individual with location, morphology and reflectance data.
#' @param species The species to be looked at, which will be selected from the data frame and
#' split into sub groups of morphs if present.
#' @param loc_file File with information on locations (see example_coordinates.csv).
#' @param nyears Number of years the model is run.
#' @param ndays Number of days modeled per year.
#' @param burrow Boolean whether lizard is allowed to seek shelter in burrow.
#' @param DEB Boolean stating wheather the ectotherm should be run with or without
#' the Dynamic Energy Budget model.
#' @param timeper Character string of the time period for the climate data /
#' predictions ("present", "2040_2059", "2080_2099").
#' @param rcp Character string to specify the emission scenario in case of climate
#' predictions ("none", "45", "85").
#' @return List of lists with output if ectotherm function for each species/population/entity
#' @export


m_run_biophysical <- function(liz_file = "example_lizard_data.csv",
                              species = "Karusasaurus_polyzonus",
                              loc_file = "example_coordinates.csv",
                              nyears = 1,
                              ndays = 12,
                              burrow = FALSE,
                              DEB = FALSE,
                              timeper = "present",
                              rcp = "none") {

  # import dataset from file
  data <- m_import_lizard_data(path = liz_file, species = species)

  # extract locations
  locations <- levels(data$LID)

  # import location information from file and run microclimate model at these locations
  loc_data <- read.csv(loc_file)
  micro_list <- split(locations, locations, drop = T)
  ecto_list <- micro_list
  for(loc in micro_list) {
    # micro_list[[loc]] <- list()
    loc_row <- m_extract_microclim_input(location = loc,
                                         loc_data = loc_data)
    micro_list[[loc]] <- m_get_microclim(loc_row = loc_row,
                                         nyears = nyears,
                                         ndays = ndays,
                                         timeper = timeper,
                                         rcp = rcp
                                         )
  }

  ecto_input <- read.csv("Physio_sum_locations.csv")
  for(loc in ecto_list) {
    param <- ecto_input[which(ecto_input$LID == loc),]
    ecto_list[[loc]] <- m_run_ectotherm(param = param,
                                        micro = micro_list[[loc]],
                                        burrow = burrow,
                                        DEB = DEB)

    # plot and save results
      # add 'DEB' to sim_name if applicable
    sim_name <- ecto_list[[loc]]$LID
    if(DEB) {
      sim_name <- paste0(sim_name, "_DEB")
    }

      # actually plot things now
    m_plot_ecto(ecto = ecto_list[[loc]], sim_name = sim_name)

  }
  ecto_list
}

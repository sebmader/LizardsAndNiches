#' @title m_run_biophysical
#' @description This function runs a full analysis of the climate niche starting with importing
#' our standard dataframe, running microclimate model and ectotherm function subsequently.
#' @name m_run_biophysical
#' @param liz_file A file with lizard data (see example_lizard_data.csv) containing at least
#' one individual with location, morphology and reflectance data.
#' @param species The species to be looked at, which will be selected from the data frame and
#' split into sub groups of morphs if present.
#' @param loc_file File with information on locations (see example_coordinates.csv).
#' @param loc_mean Boolean whether the biophysical model should be run with location means
#' to decrease computational demand (loc_mean = TRUE) or if it should be run per individual (FALSE).
#' @param physio_file File path with physiological data of the lizards per location (see
#' 'example_lizard_data.csv' for structure of dataframe).
#' @param nyears Number of years the model is run.
#' @param ndays Number of days modeled per year.
#' @param burrow Boolean whether lizard is allowed to seek shelter in burrow.
#' @param burrowtype Character string defining the burrow type: "sun" (always in the sun),
#' "shade" (always in the shade), "sunshade" (organism decides if burrow is in the sun
#' or in the shade).
#' @param DEB Boolean stating wheather the ectotherm should be run with or without
#' the Dynamic Energy Budget model.
#' @param timeper Character string of the time period for the climate data /
#' predictions ("present", "2040_2059", "2080_2099", and "presentCCKP" to compare
#' the 1961-1990 of CCKP with the one frome NicheMapR).
#' @param rcp Character string to specify the emission scenario in case of climate
#' predictions ("none", "45", "85").
#' @param shade Vector of 2 numerics defining minimum and maximum shade.
#' @param save_plot Boolean whether plots shall be saved or not.
#' @return List of lists with output of ectotherm function for each
#' species/population/entity
#' @export


m_run_biophysical <- function(liz_file = "example_lizard_data.csv",
                              species = "Karusasaurus_polyzonus",
                              loc_file = "example_coordinates.csv",
                              loc_mean = TRUE,
                              physio_file = "example_physio.csv",
                              nyears = 1,
                              ndays = 12,
                              burrow = FALSE,
                              burrowtype = "sun",
                              DEB = FALSE,
                              timeper = "present",
                              rcp = "none",
                              shade = c(0, 90),
                              save_plot = FALSE) {

  # import dataset from file
  data <- m_import_lizard_data(path = liz_file, species = species)

  # extract locations
  locations <- levels(data$LID)

  # import location information from file and run microclimate model at these locations
  loc_data <- utils::read.csv(loc_file)
  micro_list <- split(locations, locations, drop = T)
  ecto_list <- micro_list
  for(loc in locations) {
    # micro_list[[loc]] <- list()
    loc_row <- m_extract_microclim_input(location = loc,
                                         loc_data = loc_data)
    micro_list[[loc]] <- m_get_microclim(loc_row = loc_row,
                                         nyears = nyears,
                                         ndays = ndays,
                                         timeper = timeper,
                                         rcp = rcp,
                                         shade = shade)
  }

  # load physiological data
  ecto_input <- utils::read.csv(physio_file)

  # transform burrow type into numerical value
  burtype <- 0
  if(burrowtype == "sun") {
    burtype <- 0
  } else if(burrowtype == "shade") {
    burtype <- 2
  } else if(burrowtype == "sunshade") {
    burtype <- 1
  } else {
    stop("burrowtype is not specified correctly. Check function description for
         viable parameter values.")
  }

  for(loc in locations) {
    param <- ecto_input[which(ecto_input$LID == loc),]

    if(loc_mean) {
      ecto_list[[loc]] <- m_run_ectotherm(param = param,
                                          micro = micro_list[[loc]],
                                          burrow = burrow,
                                          burtype = burtype,
                                          DEB = DEB)

      # save burrowtype in list
      ecto_list[[loc]]$burrowtype <- burrowtype
    } else {
      # run biophysical per individual
      ecto_list[[loc]] <- list()
      ids <- as.character(data$ID[which(data$LID == loc)])
      for(id in ids) {
        id_data <- data[which(data$ID == id),]
        param_new <- param
        param_new$TTL_mean <- id_data$TTL
        param_new$WW_mean <- id_data$W
        param_new$absorp_mean <- 1 - id_data$REFL_W

        ecto_list[[loc]][[id]] <- m_run_ectotherm(param = param_new,
                                                 micro = micro_list[[loc]],
                                                 burrow = burrow,
                                                 burtype = burtype,
                                                 DEB = DEB)
        ecto_list[[loc]][[id]]$soil <- NULL
        ecto_list[[loc]][[id]]$shadsoil <- NULL
        ecto_list[[loc]][[id]]$shadmet <- NULL
        ecto_list[[loc]][[id]]$soilmoist <- NULL
        ecto_list[[loc]][[id]]$shadmoist <- NULL
        ecto_list[[loc]][[id]]$soilpot <- NULL
        ecto_list[[loc]][[id]]$shadpot <- NULL
        ecto_list[[loc]][[id]]$humid <- NULL
        ecto_list[[loc]][[id]]$shadhumid <- NULL


        # save burrowtype in list
        ecto_list[[loc]][[id]]$burrowtype <- burrowtype
      }
    }

    # # plot and save results
    #   # add 'DEB' to sim_name if applicable
    # sim_name <- ecto_list[[loc]]$LID
    # if(DEB) {
    #   sim_name <- paste0(sim_name, "_DEB")
    # }
    #
    #   # actually plot things now
    # m_plot_ecto(ecto = ecto_list[[loc]], sim_name = sim_name, save_plot = save_plot)

  }
  ecto_list
}

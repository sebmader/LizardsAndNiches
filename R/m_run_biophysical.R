#' @title m_run_biophysical
#' @description This function runs a full analysis of the climate niche starting with importing
#' our standard dataframe, running (location mean) microclimate model and (potentially individual)
#' ectotherm function subsequently.
#' @name m_run_biophysical
#' @param liz_file A file with lizard data (see example_lizard_data.csv) containing at least
#' one individual with location, morphology and reflectance data.
#' @param species The species to be looked at, which will be selected from the data frame and
#' split into sub groups of morphs if present.
#' @param loc_file File with information on locations (see example_coordinates.csv).
#' @param morpho_mean Boolean whether the biophysical model should be run with location means
#' to decrease computational demand (morpho_mean = TRUE) or if it should be run per individual (FALSE).
#' @param physio_file File path with physiological data of the lizards per location (see
#' 'example_lizard_data.csv' for structure of dataframe).
#' @param nyears Number of years the model is run.
#' @param ndays Number of days modeled per year.
#' @param burrow Boolean whether lizard is allowed to seek shelter in burrow.
#' @param burrowtype Character string defining the burrow type: "sun" (always in the sun),
#' "shade" (always in the shade), "sunshade" (organism decides if burrow is in the sun
#' or in the shade).
#' @param burrowdepth The burrows depth in "soil nodes", a numeric ranging from 2 to 10.
#' 2 equals to 2.5 cm, 3 to 5 cm, 4 to 10 cm, 5 to 15 cm, 6 to 20 cm, 7 to 30 cm, 8 to 50 cm,
#' 9 to 100 cm and 10 to 200 cm. These are also the steps the lizard takes while burrowing.
#' @param DEB Boolean stating wheather the ectotherm should be run with or without
#' the Dynamic Energy Budget model.
#' @param timeper Character string of the time period for the climate data /
#' predictions ("present", "2040_2059", "2080_2099", "presentCCKP" to compare
#' the 1961-1990 of CCKP with the one from NicheMapR, and "presentNASA" to use more recent
#' (1983-2017) climate data).
#' @param rcp Character string to specify the emission scenario in case of climate
#' predictions ("none", "45", "85").
#' @param shade Vector of 2 numerics defining minimum and maximum shade.
#' @param save_plot Boolean whether plots shall be saved or not.
#' @return List of lists with output of ectotherm function for each
#' species/population/entity
#' @export


m_run_biophysical <- function(liz_file,
                              species,
                              loc_file,
                              morpho_mean = FALSE,
                              physio_file,
                              nyears = 1,
                              ndays = 12,
                              burrow = FALSE,
                              burrowtype = "sunshade",
                              burrowdepth = 9,
                              DEB = FALSE,
                              timeper = "present",
                              rcp = "none",
                              shade = c(0, 90),
                              save_plot = FALSE) {

  # import dataset from file
  data <- LizardsAndNiches::m_import_lizard_data(path = liz_file, species = species)

  # extract locations
  locations <- levels(data$LID)

  # import location information from file and run microclimate model at these locations
  loc_data <- utils::read.csv(loc_file)
  micro_list <- split(locations, locations, drop = T)
  ecto_list <- micro_list
  for(loc in locations) {
    # micro_list[[loc]] <- list()
    message(paste0("\nExtracting microclimate data at '", loc, "'.\n"))

    loc_row <- LizardsAndNiches::m_extract_microclim_input(location = loc,
                                         loc_data = loc_data)


    # TODO: make microclimate computation individual (terrain data for individual lizards)

    micro_list[[loc]] <- LizardsAndNiches::m_get_microclim(loc_row = loc_row,
                                         nyears = nyears,
                                         ndays = ndays,
                                         timeper = timeper,
                                         rcp = rcp,
                                         shade = shade)
  }

  # load physiological data
  if(!file.exists(physio_file)) {
    stop("The path you specified for the physiological summary data does not
         exist.")
  }
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
    stop("'burrowtype' is not specified correctly. Check function description for
         viable parameter values.")
  }

  for(loc in locations) {
    param <- ecto_input[which(ecto_input$Species == species),]
    # param <- ecto_input[which(ecto_input$LID == loc),]


    if(morpho_mean) {

      message("\nRunning models on average location terrain data and AVERAGED
              population morphology.")

      # calculate means for TTL, WW and absorp
      param$ttl <- mean(data$TTL[which(data$LID == loc)])
      param$ww <- mean(data$W[which(data$LID == loc)])
      param$absorp <- 1 - mean(data$REFL[which(data$LID == loc)])

      ecto_list[[loc]] <- LizardsAndNiches::m_run_ectotherm(param = param,
                                          micro = micro_list[[loc]],
                                          burrow = burrow,
                                          burtype = burtype,
                                          burdepth = burrowdepth,
                                          DEB = DEB)

      # save burrowtype in list
      ecto_list[[loc]]$burrowtype <- burrowtype
      # save elevation
      loc_row <- LizardsAndNiches::m_extract_microclim_input(location = loc,
                                                  loc_data = loc_data)
      ecto_list[[loc]]$elev <- loc_row$Elevation
      # save species
      ecto_list[[loc]]$species <- species

    } else {
      # run biophysical per individual
      message("\nRunning models on average location terrain data and INDIVIDUAL
              morphology.")

      ecto_list[[loc]] <- list()
      ids <- as.character(data$ID[which(data$LID == loc)])
      for(id in ids) {
        id_data <- data[which(data$ID == id),]
        param_new <- param
        param_new$ttl <- as.numeric(id_data$TTL)
        param_new$ww <- as.numeric(id_data$W)
        param_new$absorp <- 1 - id_data$REFL

        ecto_list[[loc]][[id]] <- LizardsAndNiches::m_run_ectotherm(param = param_new,
                                                 micro = micro_list[[loc]],
                                                 burrow = burrow,
                                                 burtype = burtype,
                                                 burdepth = burrowdepth,
                                                 DEB = DEB)
        # save ID in list per ID
        ecto_list[[loc]][[id]]$ID <- id

        # remove unnecessary dataframes to reduce file size
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
        # save elevation
        # loc_row <- LizardsAndNiches::m_extract_microclim_input(location = loc,
        #                                                        loc_data = loc_data)
        ecto_list[[loc]][[id]]$elev <- loc_row$Elevation
        # save species
        ecto_list[[loc]][[id]]$species <- species
      }
    }

    # plot and save results
      # add 'DEB' to sim_name if applicable
    sim_name <- ecto_list[[loc]]$LID
    if(DEB) {
      sim_name <- paste0(sim_name, "_DEB")
    }

      # actually plot things now
      # but only if morpho_mean: don't need the ecophysio plots of every individual
    if(morpho_mean) {
      LizardsAndNiches::m_plot_ecto(ecto = ecto_list[[loc]],
                                    sim_name = sim_name,
                                    save_plot = save_plot)
    }

  }
  ecto_list
}

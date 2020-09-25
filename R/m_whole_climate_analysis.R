#' @title Whole climate analysis
#' @description All-encompassing function to run all different scenarios at once
#' (present & futures), given the input
#' @name m_whole_climate_analysis
#' @param times Vector of strings listing the different time periods to run the model on.
#' Be aware that you need to have downloaded the according climate data and saved it in
#' the appropriate folders. Default is the time period of 1961 to 1990 ("present") used
#' in New et al. (2002), the standard climate dataset used in NicheMapR (Kearney & Porter,
#' 2017). Options: "present", "2040_2059", "2080_2099", "presentCCKP" to compare
#' the 1961-1990 of CCKP with the one from NicheMapR, and "presentNASA" to use more recent
#' (1983-2017) climate data.
#' @param rcps Vector of strings listing the Representative Concentration Pathway (RCP)
#' scenarios of greenhouse gas concentrations. "45" and "85" stand for RCP4.5 and RCP8.5,
#' respectively.
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
#' @param burrowdepth The burrows depth in "soil nodes", a numeric ranging from 2 to 10.
#' 2 equals to 2.5 cm, 3 to 5 cm, 4 to 10 cm, 5 to 15 cm, 6 to 20 cm, 7 to 30 cm, 8 to 50 cm,
#' 9 to 100 cm and 10 to 200 cm. These are also the steps the lizard takes while burrowing.
#' @param DEB Boolean stating wheather the ectotherm should be run with or without
#' the Dynamic Energy Budget model. !!!NOT FULLY IMPLEMENTED AND TESTED YET!!!
#' @param shade Vector of 2 numerics defining minimum and maximum shade. Default is 0 %
#' minimum shade, 50 % maximum shade.
#' @param plot Boolean whether to produce plots of activity pattern of individual
#' lizards or not.
#' @param save_plot Boolean whether plots shall be saved or not.
#' @param monthly Boolean whether the final dataframe should be organised per individual
#' or per month at each location.
#' @return Dataframe with the collective physiological and activity data of all
#' individuals or populations and their respective climatic environments.
#' @export

m_whole_climate_analysis <- function(times = c("present"),
                                     rcps = c("none"),
                                     data_dir = "/.",
                                     liz_file = "Darkness_Morpho_Data_Adjusted.csv",
                                     species,
                                     loc_file = "Coordinates_Clean_notransect_MOK.csv",
                                     loc_mean = FALSE,
                                     physio_file = "Physio_info.csv",
                                     nyears = 1,
                                     ndays = 12,
                                     burrow = TRUE,
                                     burrowtype = "sunshade",
                                     burrowdepth = 9,
                                     DEB = FALSE,
                                     shade = c(0, 50),
                                     plot = FALSE,
                                     save_plot = FALSE,
                                     monthly = FALSE) {

  scenarios <- character()
  for (time in times) {
    for (rcp in rcps) {
      if (stringr::str_detect(time, "present")) {
        scenarios <- c(scenarios, time)
      } else {
        scenarios <- c(scenarios, paste0(time, "_RCP", rcp))
      }
    }
  }
  scenarios <- unique(scenarios)

  # data frame to save climate conditions to plot them in comparison to each other
  multi_all <- split(x = list(), f = scenarios)

  # loop through them and run_biophysical each time
  for (scen in scenarios) {

    # detect time and rcp
    if(stringr::str_detect(string = scen, pattern = "present")) {
      time <- times[which(times == scen)]
    } else {
      time <- times[stringr::str_which(string = scen, pattern = times)]
    }

    # TODO: can't differentiate "present" and "presentNASA" (nor "presentCCKP"??)

    if (stringr::str_detect(time, "present")) {
      rcp <- "none"
    } else {
      rcp <- rcps[stringr::str_detect(string = scen, pattern = rcps)]
    }

    # run biophysical model and save
    ectoall <- LizardsAndNiches::m_run_biophysical(
      liz_file = liz_file,
      species = species,
      loc_file = loc_file,
      loc_mean = loc_mean,
      physio_file = physio_file,
      burrow = burrow,
      burrowtype = burrowtype,
      burrowdepth = burrowdepth,
      DEB = DEB,
      ndays = ndays,
      nyears = nyears,
      timeper = time,
      rcp = rcp,
      save_plot = save_plot,
      shade = shade
    )

    locations <- names(ectoall)

    for (loc in locations) {

      if(loc_mean) { # if mean lizard simulations per location:

        # location data
        multi_all[[scen]][[loc]]$Species <- ectoall[[loc]]$species
        multi_all[[scen]][[loc]]$LID <- ectoall[[loc]]$LID
        # multi_all[[scen]][[loc]]$coor <- ectoall[[loc]]$coor
        multi_all[[scen]][[loc]]$Latitude <- ectoall[[loc]]$Latitude
        multi_all[[scen]][[loc]]$Longitude <- ectoall[[loc]]$Longitude
        multi_all[[scen]][[loc]]$Elevation <- ectoall[[loc]]$elev

        # climate variables: save just metout table in list to decrease memory
        multi_all[[scen]][[loc]]$metout <- ectoall[[loc]]$metout
        # plus precipitation
        multi_all[[scen]][[loc]]$rainfall <- ectoall[[loc]]$rainfall

        # activity: save just environ table in list for calculation of activity times
        multi_all[[scen]][[loc]]$environ <- ectoall[[loc]]$environ

        # individual lizard data
        multi_all[[scen]][[loc]]$absorp <- ectoall[[loc]]$absorp
        multi_all[[scen]][[loc]]$ww <- ectoall[[loc]]$ww
        multi_all[[scen]][[loc]]$ttl <- ectoall[[loc]]$ttl

        # additional scenario parameters
        multi_all[[scen]][[loc]]$timeper <- ectoall[[loc]]$timeper
        multi_all[[scen]][[loc]]$rcp <- ectoall[[loc]]$rcp

      } else { # if individual lizard simulations:

        # save model output per individual
        multi_all[[scen]][[loc]] <- list()
        # n_ind <- length(ectoall[[loc]])
        ids <- names(ectoall[[loc]])
        # ids <- as.character(data$ID[which(data$LID == loc)])
        for(id in ids) {
          # save location data
          multi_all[[scen]][[loc]][[id]]$Species <- ectoall[[loc]][[id]]$species
          multi_all[[scen]][[loc]][[id]]$ID <- ectoall[[loc]][[id]]$ID
          multi_all[[scen]][[loc]][[id]]$LID <- ectoall[[loc]][[id]]$LID
          # multi_all[[scen]][[loc]][[id]]$coor <- ectoall[[loc]][[id]]$coor
          multi_all[[scen]][[loc]][[id]]$Latitude <- ectoall[[loc]][[id]]$Latitude
          multi_all[[scen]][[loc]][[id]]$Longitude <- ectoall[[loc]][[id]]$Longitude
          multi_all[[scen]][[loc]][[id]]$Elevation <- ectoall[[loc]][[id]]$elev

          # climate variables: save just metout table in list to decrease memory
          multi_all[[scen]][[loc]][[id]]$metout <- ectoall[[loc]][[id]]$metout
          # plus precipitation
          multi_all[[scen]][[loc]][[id]]$rainfall <- ectoall[[loc]][[id]]$rainfall

          # activity: save just environ table in list for calculation of activity times
          multi_all[[scen]][[loc]][[id]]$environ <- ectoall[[loc]][[id]]$environ

          # individual lizard data
          multi_all[[scen]][[loc]][[id]]$absorp <- ectoall[[loc]][[id]]$absorp
          multi_all[[scen]][[loc]][[id]]$ww <- ectoall[[loc]][[id]]$ww
          multi_all[[scen]][[loc]][[id]]$ttl <- ectoall[[loc]][[id]]$ttl

          # additional scenario parameters
          multi_all[[scen]][[loc]][[id]]$timeper <- ectoall[[loc]][[id]]$timeper
          multi_all[[scen]][[loc]][[id]]$rcp <- ectoall[[loc]][[id]]$rcp

        }
      }
    }
  }


  # tidy up the data and output the requested dataframe
  tidy_multi <- data.frame()
  if(!monthly) {
    # individual lizards and yearly averaged climate data
    tidy_multi <- LizardsAndNiches::m_tidy_output(multi_all, monthly_clim = FALSE,
                                                  avg_loc_out = FALSE)
  } else {
    # population level averaged and monthly climate data
    # TODO: make activity also monthly
    tidy_multi <- LizardsAndNiches::m_tidy_output(multi_all, monthly_clim = TRUE,
                                                        avg_loc_out = TRUE)
  }
  tidy_multi

  # TODO fix activity plotting for individual analysis
  # (maybe take means of activity output of individual analysis and plot with SD)

  # TODO: wrong plots are produced -> take the ones from final_stats_realfinal.R
  # if(plot) {
  #   # plot activity times
  #   print(LizardsAndNiches::m_plot_activity(multi_ecto = tidy_multi, save_plot = save_plot))
  #
  #   # plot microclimates
  #   print(LizardsAndNiches::m_plot_all_micros(multi_micro = tidy_multi_month, save_plot = save_plot))
  # }

  # # close all open devices
  # if(save_plot) {
  #   for (id in dev.list()[2]:dev.list()[length(dev.list())]) {
  #     dev.off()
  #   }
  # }

}

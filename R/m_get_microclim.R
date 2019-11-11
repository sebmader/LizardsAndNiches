#' @title m_get_microclim
#' @description This function models the microclimate of locations in the given
#' input list (= output of m_extract_microclim_input)
#' @param loc_list A list containing information on location (location ID, latitude & longitude,
#' nature of soil, soil reflectance, soilgrids (downloading: yes or no))
# @param year The year for which the microclimate shall be modelled (default = present)
#' @return list of locations (as sublists) with corresponding microclimate data
#' @export

library(NicheMapR)

m_get_microclim <- function(loc_list) {
  assertthat::assert_that(is.list(loc_list))

  micro_list <- loc_list

  # TODO: implementation of model of future micro climate
  # run micro_global for each location and add output to list
  for(i in 1:length(loc_list)) {
    loc <- c(loc_list[[i]]$Longitude, loc_list[[i]]$Latitude)
    soiltype <- loc_list[[i]]$Nature
    soilgrids <- loc_list[[i]]$soilgrids
    soilrefl <- loc_list[[i]]$SREF
    soilrefl <- if(is.na(soilrefl)) {FALSE} else {soilrefl}
    micro <- NicheMapR::micro_global(loc = loc, timeinterval = 365, nyears = 3, soiltype = soiltype,
                          REFL = soilrefl, runshade = 1, run.gads = 1, Refhyt= 0.01)
    # sometimes: "no climate data for this site, using dummy data so solar is still produced "
    # ... fuck?

    # add micro climate data to loc_list
    micro_list[[i]] <- micro
  }
  micro_list
}

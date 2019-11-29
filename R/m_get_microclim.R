#' @title m_get_microclim
#' @description This function models the microclimate of locations in the given
#' input list (= output of m_extract_microclim_input)
#' @name m_get_microclim
#' @param loc_row A data frame row containing information on location
#' (location ID, latitude & longitude, nature of soil, soil reflectance,
#' soilgrids (downloading: yes or no))
# @param year The year for which the microclimate shall be modelled (default = present)
#' @param nyears Number of years the model runs.
#' @param ndays Number of days modeled per year (12 - 365)
#' @return list of locations (as sublists) with corresponding microclimate data
#' @export

m_get_microclim <- function(loc_row, nyears = 1, ndays = 365) {
  # load NicheMapR; otherwise an error is thrown because there is an object in the package
  # that is used by micro_global() ...
  require(NicheMapR)

  assertthat::assert_that(is.data.frame(loc_row))

  # TODO: implementation of model of future micro climate

  # List of multiple locations
  # micro_list <- loc_row

  # run micro_global for each location and add output to list
  # for(i in 1:length(loc_row)) {
  #   loc <- c(loc_row[[i]]$Longitude, loc_row[[i]]$Latitude)
  #   soiltype <- loc_row[[i]]$Nature
  #   soilgrids <- loc_row[[i]]$soilgrids
  #   soilrefl <- loc_row[[i]]$SREF
  #   soilrefl <- if(is.na(soilrefl)) {FALSE} else {soilrefl}
  #   micro <- NicheMapR::micro_global(loc = loc, timeinterval = 365, nyears = 3, soiltype = soiltype,
  #                         REFL = soilrefl, runshade = 1, run.gads = 1, Refhyt= 0.01)
  #   # sometimes: "no climate data for this site, using dummy data so solar is still produced"
  #   # ... fuck?
  #
  #   # add micro climate data to loc_list
  #   micro_list[[i]] <- micro
  # }

  # One location
  loc <- c(loc_row$Longitude, loc_row$Latitude)
  soiltype <- loc_row$Nature
  soilgrids <- loc_row$soilgrids
  soilrefl <- loc_row$SREF
  soilrefl <- if(is.na(soilrefl)) {FALSE} else {soilrefl}

  # function micro_global uses variables as defaults that are defined in the defaults..
  minshade <- 0
  maxshade <- 90
  # Density <- 2.56
  # BulkDensity <- 1.3
  # CampNormTbl9_1 <- NicheMapR::CampNormTbl9_1

  micro <- NicheMapR::micro_global(loc = loc, timeinterval = ndays, nyears = nyears,
                                   soiltype = soiltype, REFL = soilrefl, runshade = 1,
                                   run.gads = 1, Usrhyt = 0.01
                                   )
  # sometimes: "no climate data for this site, using dummy data so solar is still produced "
  # ... fuck?

  micro$LID <- loc_row$LID

  micro
}

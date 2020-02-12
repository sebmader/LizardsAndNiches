#' @title m_get_microclim
#' @description This function models the microclimate of locations in the given
#' input list (= output of m_extract_microclim_input)
#' @name m_get_microclim
#' @param loc_row A data frame row containing information on location
#' (location ID, latitude & longitude, nature of soil, soil reflectance,
#' soilgrids (downloading: yes or no))
#' @param nyears Number of years the model runs.
#' @param ndays Number of days modeled per year (12 - 365)
#' @param timeper Time period for climate data: either present or future ("present",
#' "2040_2059", "2080_2099")
#' @param rcp Pick a representative concentration pathway (rcp) for climate predictions
#' ("none" (for present), "45" (medium low emissions), "85" (high emissions))
#' @param shade Vector of 2 numeric values: the first for minimum, the second for
#' maximum shade.
#' @return list of locations (as sublists) with corresponding microclimate data
#' @export

m_get_microclim <- function(loc_row, nyears = 1,
                            ndays = 12,
                            timeper = "present",
                            rcp = "none",
                            shade = c(0, 90)) {
  # load NicheMapR; otherwise an error is thrown because there is an object in the package
  # that is used by micro_global() ...
  requireNamespace("NicheMapR")

  assertthat::assert_that(is.data.frame(loc_row))
  assertthat::assert_that(is.vector(shade) & length(shade) == 2)

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
  soilrefl <- ifelse(is.na(loc_row$SREF), FALSE, loc_row$SREF)
  elev <- loc_row$Elevation
  slope <- ifelse(is.na(loc_row$Slope), 0, loc_row$Slope)
  asp <- ifelse(is.na(loc_row$Aspect), 0, loc_row$Aspect)
  spec_heat <- ifelse(is.na(loc_row$Spec.Heat), 870, loc_row$Spec.Heat) # 870 is default in NicheMapR


  # function micro_global uses variables as defaults that are defined in the defaults..
  # minshade <- 0
  # maxshade <- 90
  # Density <- 2.56
  # BulkDensity <- 1.3
  # CampNormTbl9_1 <- NicheMapR::CampNormTbl9_1

  micro <- NicheMapR::micro_global(time = timeper, rcp = rcp, loc = loc,
                                   timeinterval = ndays, nyears = nyears,
                                   soiltype = soiltype, REFL = soilrefl, runshade = 1,
                                   run.gads = 1, Usrhyt = 0.01, elev = elev,
                                   slope = slope, aspect = asp, minshade = shade[1],
                                   maxshade = shade[2], SpecHeat = spec_heat
                                   )
  # sometimes: "no climate data for this site, using dummy data so solar is still produced "
  # ... fuck?

  micro$LID <- loc_row$LID
  micro$timeper <- timeper
  micro$rcp <- rcp
  micro$shade <- shade

  micro
}

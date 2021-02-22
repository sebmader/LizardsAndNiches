#' @title Extract microclimate input
#' @description This function extracts the input data of the micro_global() function of NicheMapR
#' based on our data structure (see example_coordinates.csv in the testthat folder in tests)
#' @name m_extract_microclim_input
#' @param location An R factor containing the location ID of interest
#' @param loc_data A data frame with location IDs, coordinates, elevation, slope,
#' aspect of slope and soil data (type, reflectance, if soilgrids should be computed)
#' @param individual A character string of the individuals ID that is of interest.
#' If == NULL, it is assumed that no individual data is present, so there is only one set of
#' input parameters per location.
#' @return Data frame of input data for given location for the micro_global function
#' @export

m_extract_microclim_input <- function(location = "", loc_data, individual = NULL) {

  assertthat::assert_that(is.data.frame(loc_data))

  location <- as.character(location)
  # extract data for location of interest
  if(location != "") {
    loc_data <- loc_data[which(loc_data$LID == location),]
    if(length(loc_data$LID) == 0) {
      stop(paste0("Location ", location, " can't be found in location dataset."))
    }
  }
  if(!is.null(individual)) {
    if(is.null(loc_data$ID)) {
      stop(paste0("There is no individual data supplied in ", loc_file, "."))
    }
    loc_data <- loc_data[which(loc_data$ID == individual),]
    if(length(loc_data$ID) == 0) {
      stop(paste0("Individual ", individual, " can't be found in location dataset."))
    }
  }

  # delete levels of factors not present in the selected data frame
  loc_data <- droplevels(loc_data)

  # change substrate code into the one used by NicheMapR (copied from Fedes script)
  loc_data$Nature <- gsub("R", "0", loc_data$Nature) #Rock is coded as 0 in NicheMapR
  loc_data$Nature <- gsub("S", "6", loc_data$Nature) #The only soil instance is Windheuwel
      # where the soil was sandy clay loam (Info from https://data.isric.org/geonetwork/srv/ita/catalog.search#/metadata/10aa9a99-1433-11e9-a8fa-a0481ca9e724)
  loc_data$Nature <- gsub("P", "1", loc_data$Nature) #Macropholis in labertbaai lives on
      # plants but the soild underneath is sand
  loc_data$Nature <- as.numeric(loc_data$Nature)

  # specific heat capacity needs multiplication by 1000
  # our data is in J(g^-1)(K^-1), NicheMapR wants J(kg^-1)(K^-1)



  if(sum(is.na(loc_data$Spec.Heat)) > 0) {
    warning(paste0("Specific heat capacity at ", location, " constains ",
                   sum(is.na(loc_data$Spec.Heat)) > 0, " NAs.
            The default of NicheMapR will be used for them."))
  }
  if(sum(is.na(loc_data$SREF)) > 0) {
    warning(paste0("Soil reflectance at ", location, " contains ",
                   sum(is.na(loc_data$SREF)), " NAs.
            The default of NicheMapR will be used for them."))
  }

  loc_data$Spec.Heat <- loc_data$Spec.Heat * 1000

  # output
  loc_data

  # # make list to add the micro_global output to each location
  # loc_list <- split(loc_data, loc_data$LID)
  #
  # # write as csv file for direct use in the future
  # # write.csv(loc_list, "Microclimate_Southafrica.csv")
  #
  # loc_list
}

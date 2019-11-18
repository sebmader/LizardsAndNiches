#' @title Extract microclimate input
#' @description This function extracts the input data of the micro_global() function of NicheMapR
#' based on our data structure (see example_coordinates.csv in the testthat folder in tests)
#' @name m_extract_microclim_input
#' @param location An R factor containing the location ID of interest
#' @param loc_data A data frame with location IDs, coordinates and soil data
#' @return Data frame of input data for given location for the micro_global function
#' @export

m_extract_microclim_input <- function(location, loc_data) {
  assertthat::assert_that(is.factor(location))
  assertthat::assert_that(is.data.frame(loc_data))

  # load data set
  # data <- m_import_lizard_data(path = file, species = species)
  # extract location IDs
  # locations <- levels(data$LID)
  # load location data
  # loc_data_all <- read.csv(file = "Coordinates_Clean.csv")
  # loc_data <- data.frame()
  # for(loc in location) {
  #   loc_data <- rbind(loc_data,
  #                     loc_data_all[which(loc_data_all$LID == loc),])
  # }

  location <- as.character(location)
  # extract data for location of interest
  loc_data <- loc_data[which(loc_data$LID == location),]
  # delete levels of factors not present in the selected data frame
  loc_data <- droplevels(loc_data)
  # change substrate code into the one used by NicheMapR (copied from Fedes script)
  loc_data$Nature <- gsub("R", "0", loc_data$Nature) #Rock is coded as 0 in NicheMapR
  loc_data$Nature <- gsub("S", "6", loc_data$Nature) #The only soil instance si Windheuwel where the soil was sandy clay loam (Info from https://data.isric.org/geonetwork/srv/ita/catalog.search#/metadata/10aa9a99-1433-11e9-a8fa-a0481ca9e724)
  loc_data$Nature <- gsub("P", "1", loc_data$Nature) #Macropholis in labertbaai lives on plants but the soild underneath is sand
  loc_data$Nature <- as.numeric(loc_data$Nature)
  loc_data
  # # make list to add the micro_global output to each location
  # loc_list <- split(loc_data, loc_data$LID)
  #
  # # write as csv file for direct use in the future
  # # write.csv(loc_list, "Microclimate_Southafrica.csv")
  #
  # loc_list
}

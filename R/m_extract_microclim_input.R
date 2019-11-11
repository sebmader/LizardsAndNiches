#' @title Extract microclimate input
#' @description This function extracts the input data of the micro_global() function of NicheMapR
#' based on our data structure (see example_lizard_data.csv in the testthat folder in tests)
#' @name m_extract_microclim_input
#' @param file Data file in csv format (set working directory to folder with all data files)
#' @param species Species of interest in format "Genus_species"
#' @return List of vectors of input data for each location for the micro_global function
#' @importFrom utils read.csv
#' @export

m_extract_microclim_input <- function(file, species = "") {

  # load data set
  data <- m_import_lizard_data(path = file, species = species)
  # extract location IDs
  locations <- levels(data$LID)
  # load location data
  loc_data_all <- read.csv(file = "Coordinates_Clean.csv")
  loc_data <- data.frame()
  for(loc in locations) {
    loc_data <- rbind(loc_data,
                      loc_data_all[which(loc_data_all$LID == loc),])
  }
  # delete levels of factors not present in the selected data frame
  loc_data <- droplevels(loc_data)
  # change substrate code into the one used by NicheMapR (copied from Fedes script)
  loc_data$Nature <- gsub("R", "0", loc_data$Nature) #Rock is coded as 0 in NicheMapR
  loc_data$Nature <- gsub("S", "6", loc_data$Nature) #The only soil instance si Windheuwel where the soil was sandy clay loam (Info from https://data.isric.org/geonetwork/srv/ita/catalog.search#/metadata/10aa9a99-1433-11e9-a8fa-a0481ca9e724)
  loc_data$Nature <- gsub("P", "1", loc_data$Nature) #Macropholis in labertbaai lives on plants but the soild underneath is sand
  loc_data$Nature <- as.numeric(loc_data$Nature)
  # make list to add the micro_global output to each location
  loc_list <- split(loc_data, loc_data$LID)

  loc_list
}

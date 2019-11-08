#' @title Extract microclimate
#' This function extracts the output data of the micro_global() function of NicheMapR
#' based on our data structure (see example_lizard_data.csv in the testthat folder in tests)
#' @name m_extract_microclim
#' @param file Data file in csv format (set working directory to folder with all data files)
#' @param species Species of interest in format "Genus_species"
#' @return List of vectors of micro climate data for each location
#' @importFrom utils read.csv
#' @export

m_extract_microclim <- function(file, species = "") {

  # load data set
  data <- m_import_lizard_data(path = file, species = species)
  # extract location IDs
  locations <- names(data$LID)
  # load location data
  loc_data <- read.csv(file ="Coordinates_Clean.csv")
  # change substrate code into the one used by NicheMapR (copied from Fedes script)
  loc_data$Nature <- gsub("R", "0", loc_data$Nature) #Rock is coded as 0 in NicheMapR
  loc_data$Nature <- gsub("S", "6", loc_data$Nature) #The only soil instance si Windheuwel where the soil was sandy clay loam (Info from https://data.isric.org/geonetwork/srv/ita/catalog.search#/metadata/10aa9a99-1433-11e9-a8fa-a0481ca9e724)
  loc_data$Nature <- gsub("P", "1", loc_data$Nature) #Macropholis in labertbaai lives on plants but the soild underneath is sand
  loc_data$Nature <- as.numeric(loc_data$Nature)
  # make list to add the micro_global output to each location
  loc_list <- split(loc_data, loc_data$LID)

  micro_list <- loc_list
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

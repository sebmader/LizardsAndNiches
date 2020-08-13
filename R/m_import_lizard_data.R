#' @title Import lizard data
#' This function imports a CSV file of processed field data of lizard individuals
#' of different species including sites, their location (coordinates), morphology data
#' and reflectance means
#' @name m_import_lizard_data
#' @param path The path to the csv data file
#' @param species The name of the species starting with a capital letter and an underscore as
#' seperation between genus and species name; by default empty to load the whole data set
#' @return R data frame
# @importFrom utils read.csv
#' @export

# require(assertthat)

m_import_lizard_data <- function(path = "example_lizard_data.csv",
                                 species = "") {
  assertthat::is.string(path)
  if(!file.exists(path)) {
    stop("The path you specified for the individual data does not
         exist.")
  }
  assertthat::is.string(species)

  data <- utils::read.csv(file = path, header = T, stringsAsFactors = T)

  # drop non-adults
  data <- data[which(data$SEX == "M" | data$SEX == "F"),]

  if(species == "") {
    data
  } else {
    if(!(species %in% data$Species)) {
      stop("The species you are looking for is not existent in this data frame.
          Try another species and make sure the genus and species names are separated
          by an underscore.")
    }
    sel_species_data <- data[which(data$Species == species),]

    sel_species_data <- droplevels(sel_species_data)

    sel_species_data
  }
}

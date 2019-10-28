#' General analysis of lizard data set
#' This is a script to check the data of Karusasaurus polyzonus populations for various
#' correlations. First of all, I want to check if I there are differences in morphology
#' between the population, so that I would need to use distinct values for the different
#' populations


# define path and species (in this case Karusasaurus polyzonus)
path <- "../../fedes processed data/Dataset_nolog_bifpr_nosoil.csv"
species <- "Karusasaurus_polyzonus"

# import our field data and already selecting given species
data <- import_lizard_data(path = path,
                           species = species)

# checking that the extra factor levels have been dropped
# str(data)

# splitting the species data into population data
populations <- levels(data$LID)
pop_data <- list()

for(pop in populations) {
  sel_data <- data[which(data$LID == pop),]
  sel_data <- droplevels(sel_data)
  pop_data[[pop]] <- sel_data
}

# import Julia's data

# compare different summary statistics of the populations

# compare summary statistcs of colour morphs



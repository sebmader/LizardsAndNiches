% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/m_extract_microclim_input.R
\name{m_extract_microclim_input}
\alias{m_extract_microclim_input}
\title{Extract microclimate input}
\usage{
m_extract_microclim_input(location = "", loc_data, individual = NULL)
}
\arguments{
\item{location}{An R factor containing the location ID of interest}

\item{loc_data}{A data frame with location IDs, coordinates, elevation, slope,
aspect of slope and soil data (type, reflectance, if soilgrids should be computed)}

\item{individual}{A character string of the individuals ID that is of interest.
If == NULL, it is assumed that no individual data is present, so there is only one set of
input parameters per location.}
}
\value{
Data frame of input data for given location for the micro_global function
}
\description{
This function extracts the input data of the micro_global() function of NicheMapR
based on our data structure (see example_coordinates.csv in the testthat folder in tests)
}

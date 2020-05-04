#' @title Tidy(verse) up NMR results
#' @description This function tidys up the output of the biophysical model of ectotherms
#' from the NicheMapR package and returns it as one dataframe.
#' @name m_tidy_output
#' @param multi_all A list of different climate scenarios which are lists
#' of locations which are lists containing the microclimate and biophysical
#' model outputs plus some parameter settings (see vignette of 'ectotherm'
#' and 'micro_global' in NicheMapR package).
#' @param monthly_clim Boolean to define whether the microclimate variables are
#' reported per month or as yearly average (default = FALSE).
#' @param avg_loc_out When the input data is per individual, this parameter decides
#' if the output should be averaged (TRUE) or not (FALSE).
#' @return data.frame
#' @export

m_tidy_output <- function(multi_all, monthly_clim = FALSE, avg_loc_out = FALSE) {

  # make tidyverse data frame with activity times summed over the whole year,
  # microclimate values and some extra (input) parameters

  # check if multi_all is per individual or the average of each location
  loc_mean <- TRUE
  if(is.list(multi_all[[1]][[1]][[1]])) {
    loc_mean <- FALSE
  }

  # throw error message if input is per location and it shall be averaged over location
  if(loc_mean == T & avg_loc_out == T) {
    warning("You cannot average the individual data over location if the input
         data is already averaged over location. The averaged input data will be
         reported.")
  }

  multi_all_tab <- c()

  if(loc_mean) {
    multi_all_tab <- LizardsAndNiches:::m_tidy_output_loc(multi_all, monthly_clim)
  } else {
    multi_all_tab <- LizardsAndNiches:::m_tidy_output_ind(multi_all, monthly_clim, avg_loc_out)
  }

  # # calculate the activity-basking ratio and add it to dataframe
  # act_bask_ratio <- multi_all_tab$h_active/multi_all_tab$h_bask
  # multi_all_tab <- cbind(multi_all_tab, act_bask_ratio)

  # # make dataframe with 'present' being both rcp 4.5 and 8.5 instead of none
  # present45 <- multi_all_tab[which(stringr::str_detect(multi_all_tab$timeper,
  #                                                       "present")),]
  # present85 <- present45
  # present45$rcp <- "45"
  # present85$rcp <- "85"
  #
  # multi_all_tab <- rbind(multi_all_tab[which(
  #   !stringr::str_detect(multi_all_tab$timeper,
  #                        "present")),],
  #   present45, present85)

  # change 'timeper' and 'rcp' to nicely displayable strings
  multi_all_tab$timeper <- gsub(pattern = "present.*",
                                      replacement = "pres",
                                      x = multi_all_tab$timeper)
  multi_all_tab$timeper <- gsub(pattern = "2040_2059",
                                      replacement = "40-59",
                                      x = multi_all_tab$timeper)
  multi_all_tab$timeper <- gsub(pattern = "2080_2099",
                                      replacement = "80-99",
                                      x = multi_all_tab$timeper)
  multi_all_tab$rcp <- gsub(pattern = "85",
                                replacement = "8.5",
                                x = multi_all_tab$rcp)
  multi_all_tab$rcp <- gsub(pattern = "45",
                            replacement = "4.5",
                            x = multi_all_tab$rcp)

  multi_all_tab$scenario <- as.factor(multi_all_tab$scenario)
  multi_all_tab$timeper <- as.factor(multi_all_tab$timeper)
  multi_all_tab$rcp <- as.factor(multi_all_tab$rcp)

  multi_all_tab

}

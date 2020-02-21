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
#' @return data.frame
#' @export

m_tidy_output <- function(multi_all, monthly_clim = FALSE) {

  # make tidyverse data frame with activity times summed over the whole year,
  # microclimate values and some extra (input) parameters

  # vector of scenario names
  scenarios <- names(multi_all)

  # vector of location names
  locations <- names(multi_all[[1]])

  # loop through multi_all and save total activity time per year
  for(scen in scenarios) {
    for(loc in locations) {

      #### ectotherm output ####

      ndays <- length(unique(multi_all[[scen]][[loc]]$environ[,1]))
      nyears <- length(unique(multi_all[[scen]][[loc]]$environ[,2]))

      # count hours with activity == 2 for activity times
      h_active <- length(which(multi_all[[scen]][[loc]]$environ[,9] == 2))
      perc_active <- h_active/(nyears*ndays*24)

      # count hours with activity == 1 for basking times
      h_bask <- length(which(multi_all[[scen]][[loc]]$environ[,9] == 1))
      perc_bask <- h_bask/(nyears*ndays*24)

      # save in multi_all
      multi_all[[scen]][[loc]]$h_active <- h_active
      multi_all[[scen]][[loc]]$perc_active <- perc_active
      multi_all[[scen]][[loc]]$h_bask <- h_bask
      multi_all[[scen]][[loc]]$perc_bask <- perc_bask
      multi_all[[scen]][[loc]]$act_bask_ratio <- h_active/h_bask
      multi_all[[scen]][[loc]]$ndays <- ndays
      multi_all[[scen]][[loc]]$nyears <- nyears


      #### microclimate output ####

      list_days <- split.data.frame(x = multi_all[[scen]][[loc]]$metout[,3:6],
                                    f = multi_all[[scen]][[loc]]$metout[,1])

      months <- seq(1, length(list_days), 1)

      # calculate average microclimate conditions per day and save as vector
      T_loc <- vector(mode = "numeric", length = length(months))
      T_ref <- vector(mode = "numeric", length = length(months))
      RH_loc <- vector(mode = "numeric", length = length(months))
      RH_ref <- vector(mode = "numeric", length = length(months))


      for(month in months) {
        T_loc[month] <- mean(list_days[[month]][,1])
        T_ref[month] <- mean(list_days[[month]][,2])
        RH_loc[month] <- mean(list_days[[month]][,3])
        RH_ref[month] <- mean(list_days[[month]][,4])
      }

      if(monthly_clim) {
        multi_all[[scen]][[loc]]$months <- months
        multi_all[[scen]][[loc]]$T_loc <- T_loc
        multi_all[[scen]][[loc]]$T_ref <- T_ref
        multi_all[[scen]][[loc]]$RH_loc <- RH_loc
        multi_all[[scen]][[loc]]$RH_ref <- RH_ref
      } else {
        multi_all[[scen]][[loc]]$T_loc <- mean(T_loc)
        multi_all[[scen]][[loc]]$T_ref <- mean(T_ref)
        multi_all[[scen]][[loc]]$RH_loc <- mean(RH_loc)
        multi_all[[scen]][[loc]]$RH_ref <- mean(RH_ref)
      }

    }
  }

  # no need for the environ table anymore (and it makes unlisting very tricky)
  for(scen in scenarios) {
    for(loc in locations) {

      #### ectotherm output ####

      # calculate activity change to present
      multi_all[[scen]][[loc]]$change_act <- multi_all[[scen]][[loc]]$h_active - multi_all[["present"]][[loc]]$h_active
      multi_all[[scen]][[loc]]$change_bask <- multi_all[[scen]][[loc]]$h_bask - multi_all[["present"]][[loc]]$h_bask
      # calculate percentage of change
      multi_all[[scen]][[loc]]$perc_change_act <- multi_all[[scen]][[loc]]$h_active / multi_all[["present"]][[loc]]$h_active
      multi_all[[scen]][[loc]]$perc_change_bask <- multi_all[[scen]][[loc]]$h_bask / multi_all[["present"]][[loc]]$h_bask

      multi_all[[scen]][[loc]]$environ <- NULL

      #### microclimate output ####

      # calculate and save total change
      multi_all[[scen]][[loc]]$change_T_loc <- multi_all[[scen]][[loc]]$T_loc - multi_all[["present"]][[loc]]$T_loc
      multi_all[[scen]][[loc]]$change_RH_loc <- multi_all[[scen]][[loc]]$RH_loc - multi_all[["present"]][[loc]]$RH_loc
      # calculate and save percentage change
      multi_all[[scen]][[loc]]$perc_T_loc <- multi_all[[scen]][[loc]]$T_loc / multi_all[["present"]][[loc]]$T_loc
      multi_all[[scen]][[loc]]$perc_RH_loc <- multi_all[[scen]][[loc]]$RH_loc / multi_all[["present"]][[loc]]$RH_loc

      multi_all[[scen]][[loc]]$metout <- NULL
    }
  }

  # unlist multi_all into a dataframe
  multi_all_tab <- data.table::rbindlist(lapply(multi_all,
                                                 function(x) data.table::rbindlist(x)),
                                          idcol = "id")

  # # calculate the activity-basking ratio and add it to dataframe
  # act_bask_ratio <- multi_all_tab$h_active/multi_all_tab$h_bask
  # multi_all_tab <- cbind(multi_all_tab, act_bask_ratio)

  # make dataframe with 'present' being both rcp 4.5 and 8.5 instead of none
  present45 <- multi_all_tab[which(stringr::str_detect(multi_all_tab$timeper,
                                                        "present")),]
  present85 <- present45
  present45$rcp <- "45"
  present85$rcp <- "85"

  multi_all_tab <- rbind(multi_all_tab[which(
    !stringr::str_detect(multi_all_tab$timeper,
                         "present")
  ),], present45, present85)

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

  multi_all_tab$id <- as.factor(multi_all_tab$id)
  multi_all_tab$timeper <- as.factor(multi_all_tab$timeper)
  multi_all_tab$rcp <- as.factor(multi_all_tab$rcp)

  multi_all_tab

}

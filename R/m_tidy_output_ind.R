#' @title Tidy(verse) up NMR results - individual input data
#' @description This function tidys up the output of the biophysical model of ectotherms
#' from the NicheMapR package and returns it as one dataframe. This function takes over
#' the individual input data tidying.
#' @name m_tidy_output_ind
#' @param multi_all A list of different climate scenarios which are lists
#' of locations which are lists containing the microclimate and biophysical
#' model outputs plus some parameter settings (see vignette of 'ectotherm'
#' and 'micro_global' in NicheMapR package).
#' @param monthly_clim Boolean to define whether the microclimate variables are
#' reported per month or as yearly average (default = FALSE).
#' @param avg_loc_out When the input data is per individual, this parameter decides
#' if the output should be averaged (TRUE) or not (FALSE).
#' @return data.frame

m_tidy_output_ind <- function(multi_all, monthly_clim = FALSE, avg_loc_out = FALSE) {

  # vector of scenario names
  scenarios <- names(multi_all)

  # vector of location names
  locations <- names(multi_all[[1]])

  # loop through multi_all and save total activity time per year
  for(scen in scenarios) {
    for(loc in locations) {

      # build data frame per *individual*

      ids <- names(multi_all[[scen]][[loc]])

      for(id in ids) {
        ### ectotherm output ###

        ndays <- length(unique(multi_all[[scen]][[loc]][[id]]$environ[,1]))
        nyears <- length(unique(multi_all[[scen]][[loc]][[id]]$environ[,2]))

        # count hours with activity == 2 for activity times
        h_active <- length(which(multi_all[[scen]][[loc]][[id]]$environ[,9] == 2))
        perc_active <- h_active/(nyears*ndays*24)

        # count hours with activity == 1 for basking times
        h_bask <- length(which(multi_all[[scen]][[loc]][[id]]$environ[,9] == 1))
        perc_bask <- h_bask/(nyears*ndays*24)

        # save in multi_all
        multi_all[[scen]][[loc]][[id]]$h_active <- h_active
        multi_all[[scen]][[loc]][[id]]$perc_active <- perc_active
        multi_all[[scen]][[loc]][[id]]$h_bask <- h_bask
        multi_all[[scen]][[loc]][[id]]$perc_bask <- perc_bask
        multi_all[[scen]][[loc]][[id]]$act_bask_ratio <- h_active/h_bask
        multi_all[[scen]][[loc]][[id]]$ndays <- ndays
        multi_all[[scen]][[loc]][[id]]$nyears <- nyears


        ### microclimate output ###

        list_days <- split.data.frame(x = multi_all[[scen]][[loc]][[id]]$metout[,3:6],
                                      f = multi_all[[scen]][[loc]][[id]]$metout[,1])

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
          multi_all[[scen]][[loc]][[id]]$months <- months
          multi_all[[scen]][[loc]][[id]]$T_loc <- T_loc
          multi_all[[scen]][[loc]][[id]]$T_ref <- T_ref
          multi_all[[scen]][[loc]][[id]]$RH_loc <- RH_loc
          multi_all[[scen]][[loc]][[id]]$RH_ref <- RH_ref
        } else {
          multi_all[[scen]][[loc]][[id]]$T_loc <- mean(T_loc)
          multi_all[[scen]][[loc]][[id]]$T_ref <- mean(T_ref)
          multi_all[[scen]][[loc]][[id]]$RH_loc <- mean(RH_loc)
          multi_all[[scen]][[loc]][[id]]$RH_ref <- mean(RH_ref)
        }

      }
    }
  }

  # no need for the 'environ' and 'metout' tables anymore (and it makes unlisting very tricky)
  for(scen in scenarios) {
    for(loc in locations) {

      ids <- names(multi_all[[scen]][[loc]])

      for(id in ids) {

        #### ectotherm output ####

        # calculate activity change to present
        multi_all[[scen]][[loc]][[id]]$change_act <- multi_all[[scen]][[loc]][[id]]$h_active - multi_all[["present"]][[loc]][[id]]$h_active
        multi_all[[scen]][[loc]][[id]]$change_bask <- multi_all[[scen]][[loc]][[id]]$h_bask - multi_all[["present"]][[loc]][[id]]$h_bask
        # calculate percentage of change
        multi_all[[scen]][[loc]][[id]]$perc_change_act <- multi_all[[scen]][[loc]][[id]]$h_active / multi_all[["present"]][[loc]][[id]]$h_active
        multi_all[[scen]][[loc]][[id]]$perc_change_bask <- multi_all[[scen]][[loc]][[id]]$h_bask / multi_all[["present"]][[loc]][[id]]$h_bask

        multi_all[[scen]][[loc]][[id]]$environ <- NULL

        #### microclimate output ####

        # calculate and save total change
        multi_all[[scen]][[loc]][[id]]$change_T_loc <- multi_all[[scen]][[loc]][[id]]$T_loc - multi_all[["present"]][[loc]][[id]]$T_loc
        multi_all[[scen]][[loc]][[id]]$change_RH_loc <- multi_all[[scen]][[loc]][[id]]$RH_loc - multi_all[["present"]][[loc]][[id]]$RH_loc
        # calculate and save percentage change
        multi_all[[scen]][[loc]][[id]]$perc_T_loc <- multi_all[[scen]][[loc]][[id]]$T_loc / multi_all[["present"]][[loc]][[id]]$T_loc
        multi_all[[scen]][[loc]][[id]]$perc_RH_loc <- multi_all[[scen]][[loc]][[id]]$RH_loc / multi_all[["present"]][[loc]][[id]]$RH_loc

        multi_all[[scen]][[loc]][[id]]$metout <- NULL
      }
    }
  }

  # take means of individuals at each location if applicable

  if(avg_loc_out) {
    for(scen in scenarios) {
      for(loc in locations) {

        loc_summary <- list()  # empty list to calculate the means

        # save shared descriptive parameters
        loc_summary$LID <- multi_all[[scen]][[loc]][[1]]$LID
        loc_summary$coor <- multi_all[[scen]][[loc]][[1]]$coor
        loc_summary$timeper <- multi_all[[scen]][[loc]][[1]]$timeper
        loc_summary$rcp <- multi_all[[scen]][[loc]][[1]]$rcp
        loc_summary$ndays <- multi_all[[scen]][[loc]][[1]]$ndays
        loc_summary$nyears <- multi_all[[scen]][[loc]][[1]]$nyears

        ids <- names(multi_all[[scen]][[loc]])
        n_ind <- length(ids)

        absorp <- vector(mode = "numeric", length = n_ind)
        ww <- vector(mode = "numeric", length = n_ind)
        ttl <- vector(mode = "numeric", length = n_ind)
        h_active <- vector(mode = "numeric", length = n_ind)
        perc_active <- vector(mode = "numeric", length = n_ind)
        h_bask <- vector(mode = "numeric", length = n_ind)
        perc_bask <- vector(mode = "numeric", length = n_ind)
        act_bask_ratio <- vector(mode = "numeric", length = n_ind)
        change_act <- vector(mode = "numeric", length = n_ind)
        change_bask <- vector(mode = "numeric", length = n_ind)
        perc_change_act <- vector(mode = "numeric", length = n_ind)
        perc_change_bask <- vector(mode = "numeric", length = n_ind)

        if(monthly_clim) {
          # matrix: for each individuals number of months(/days)
          n_months <- length(months)
          T_loc <- matrix(nrow = n_ind, ncol = n_months)
          T_ref <- matrix(nrow = n_ind, ncol = n_months)
          RH_loc <- matrix(nrow = n_ind, ncol = n_months)
          RH_ref <- matrix(nrow = n_ind, ncol = n_months)
          change_T_loc <- matrix(nrow = n_ind, ncol = n_months)
          change_RH_loc <- matrix(nrow = n_ind, ncol = n_months)
          perc_T_loc <- matrix(nrow = n_ind, ncol = n_months)
          perc_RH_loc <- matrix(nrow = n_ind, ncol = n_months)

        } else {
          # otherwise (yearly average climate) just a vector of length
          # of number of individuals
          T_loc <- vector(mode = "numeric", length = n_ind)
          T_ref <- vector(mode = "numeric", length = n_ind)
          RH_loc <- vector(mode = "numeric", length = n_ind)
          RH_ref <- vector(mode = "numeric", length = n_ind)
          change_T_loc <- vector(mode = "numeric", length = n_ind)
          change_RH_loc <- vector(mode = "numeric", length = n_ind)
          perc_T_loc <- vector(mode = "numeric", length = n_ind)
          perc_RH_loc <- vector(mode = "numeric", length = n_ind)
        }

        for(i in 1:n_ind) {

          absorp[i] <- multi_all[[scen]][[loc]][[i]]$absorp
          ww[i] <- multi_all[[scen]][[loc]][[i]]$ww
          ttl[i] <- multi_all[[scen]][[loc]][[i]]$ttl
          h_active[i] <- multi_all[[scen]][[loc]][[i]]$h_active
          perc_active[i] <- multi_all[[scen]][[loc]][[i]]$perc_active
          h_bask[i] <- multi_all[[scen]][[loc]][[i]]$h_bask
          perc_bask[i] <- multi_all[[scen]][[loc]][[i]]$perc_bask
          act_bask_ratio[i] <- multi_all[[scen]][[loc]][[i]]$act_bask_ratio
          change_act[i] <- multi_all[[scen]][[loc]][[i]]$change_act
          change_bask[i] <- multi_all[[scen]][[loc]][[i]]$change_bask
          perc_change_act[i] <- multi_all[[scen]][[loc]][[i]]$perc_change_act
          perc_change_bask[i] <- multi_all[[scen]][[loc]][[i]]$perc_change_bask

          if(monthly_clim) {

            T_loc[i,] <- multi_all[[scen]][[loc]][[i]]$T_loc
            T_ref[i,] <- multi_all[[scen]][[loc]][[i]]$T_ref
            RH_loc[i,] <- multi_all[[scen]][[loc]][[i]]$RH_loc
            RH_ref[i,] <- multi_all[[scen]][[loc]][[i]]$RH_ref
            change_T_loc[i,] <- multi_all[[scen]][[loc]][[i]]$change_T_loc
            change_RH_loc[i,] <- multi_all[[scen]][[loc]][[i]]$change_RH_loc
            perc_T_loc[i,] <- multi_all[[scen]][[loc]][[i]]$perc_T_loc
            perc_RH_loc[i,] <- multi_all[[scen]][[loc]][[i]]$perc_RH_loc


          } else {
            T_loc[i] <- multi_all[[scen]][[loc]][[i]]$T_loc
            T_ref[i] <- multi_all[[scen]][[loc]][[i]]$T_ref
            RH_loc[i] <- multi_all[[scen]][[loc]][[i]]$RH_loc
            RH_ref[i] <- multi_all[[scen]][[loc]][[i]]$RH_ref
            change_T_loc[i] <- multi_all[[scen]][[loc]][[i]]$change_T_loc
            change_RH_loc[i] <- multi_all[[scen]][[loc]][[i]]$change_RH_loc
            perc_T_loc[i] <- multi_all[[scen]][[loc]][[i]]$perc_T_loc
            perc_RH_loc[i] <- multi_all[[scen]][[loc]][[i]]$perc_RH_loc
          }
        }

        # save means and standard deviations
          # physio
        loc_summary$absorp <- mean(absorp)
        loc_summary$absorp_sd <- sd(absorp)
        loc_summary$ww <- mean(ww)
        loc_summary$ww_sd <- sd(ww)
        loc_summary$ttl <- mean(ttl)
        loc_summary$ttl_sd <- sd(ttl)
        loc_summary$h_active <- mean(h_active)
        loc_summary$h_active_sd <- sd(h_active)
        loc_summary$perc_active <- mean(perc_active)
        loc_summary$perc_active_sd <- sd(perc_active)
        loc_summary$h_bask <- mean(h_bask)
        loc_summary$h_bask_sd <- sd(h_bask)
        loc_summary$perc_bask <- mean(perc_bask)
        loc_summary$perc_bask_sd <- sd(perc_bask)
        loc_summary$act_bask_ratio <- mean(act_bask_ratio)
        loc_summary$act_bask_ratio_sd <- sd(act_bask_ratio)
        loc_summary$change_act <- mean(change_act)
        loc_summary$change_act_sd <- sd(change_act)
        loc_summary$change_bask <- mean(change_bask)
        loc_summary$change_bask_sd <- sd(change_bask)
        loc_summary$perc_change_act <- mean(perc_change_act)
        loc_summary$perc_change_act_sd <- sd(perc_change_act)
        loc_summary$perc_change_bask <- mean(perc_change_bask)
        loc_summary$perc_change_bask_sd <- sd(perc_change_bask)

          # micro climate (should be the same for every individual)
        if(monthly_clim) {

          loc_summary$month <- months

          assertthat::are_equal(sd(T_loc[,1]), 0)
          loc_summary$T_loc <- colMeans(T_loc)
          assertthat::are_equal(sd(T_ref[,1]), 0)
          loc_summary$T_ref <- colMeans(T_ref)
          assertthat::are_equal(sd(RH_loc[,1]), 0)
          loc_summary$RH_loc <- colMeans(RH_loc)
          assertthat::are_equal(sd(RH_ref[,1]), 0)
          loc_summary$RH_ref <- colMeans(RH_ref)
          assertthat::are_equal(sd(change_T_loc[,1]), 0)
          loc_summary$change_T_loc <- colMeans(change_T_loc)
          assertthat::are_equal(sd(change_RH_loc[,1]), 0)
          loc_summary$change_RH_loc <- colMeans(change_RH_loc)
          assertthat::are_equal(sd(perc_T_loc[,1]), 0)
          loc_summary$perc_T_loc <- colMeans(perc_T_loc)
          assertthat::are_equal(sd(perc_RH_loc[,1]), 0)
          loc_summary$perc_RH_loc <- colMeans(perc_RH_loc)
        } else {
          assertthat::are_equal(sd(T_loc), 0)
          loc_summary$T_loc <- mean(T_loc)
          assertthat::are_equal(sd(T_ref), 0)
          loc_summary$T_ref <- mean(T_ref)
          assertthat::are_equal(sd(RH_loc), 0)
          loc_summary$RH_loc <- mean(RH_loc)
          assertthat::are_equal(sd(RH_ref), 0)
          loc_summary$RH_ref <- mean(RH_ref)
          assertthat::are_equal(sd(change_T_loc), 0)
          loc_summary$change_T_loc <- mean(change_T_loc)
          assertthat::are_equal(sd(change_RH_loc), 0)
          loc_summary$change_RH_loc <- mean(change_RH_loc)
          assertthat::are_equal(sd(perc_T_loc), 0)
          loc_summary$perc_T_loc <- mean(perc_T_loc)
          assertthat::are_equal(sd(perc_RH_loc), 0)
          loc_summary$perc_RH_loc <- mean(perc_RH_loc)
        }
        # save in full data list
        multi_all[[scen]][[loc]] <- loc_summary

      }
    }
  }

  # unlist multi_all into a dataframe

  if(avg_loc_out) {
    multi_all_tab <- data.table::rbindlist(lapply(multi_all,
                                                  function(x) data.table::rbindlist(x)),
                                           idcol = "id")
  } else {
  multi_all_tab <- data.table::rbindlist(
    lapply(multi_all,
           function(y) data.table::rbindlist(
             lapply(y,
                    function(x) data.table::rbindlist(x)))),
    idcol = "id")
  }

  # rename 'id' (list names) column
  names(multi_all_tab)[which(names(multi_all_tab) == "id")] <- "scenario"

  # output
  multi_all_tab

}

#' @title Plot all microclimates
#' @description This function plots the microclimate conditions from the output of
#' multiple micro_global models per location and scenario in one plot
#' @name m_plot_all_micros
#' @param multi_micro A tidy data frame of summarised output results of the ecotherm function
#' containing monthly (!) averaged microclimate variables per scenario and location
#' (for details see ?m_tidy_output).
#' @param save_plot Boolean whether the microclimate plot should be saved or not
#' (default = FALSE)
#' @return Plot
# @importFrom ggplot2 ggplot aes geom_line facet_grid vars scale_x_discrete labs
#' @importFrom grDevices png
#' @export


m_plot_all_micros <- function(multi_micro, save_plot = FALSE) {

  # create directory of save path if applicable
  save_path <- "./Plots/microclim_plots/"

  if(save_plot) {

    if(!dir.exists(save_path)) {
      dir.create(save_path, recursive = T)
      cat(paste0("Created folder ", save_path, "\n"))
    }
  }


  # # vector of scenario names
  # scenarios <- names(multi_micro)
  #
  # # vector of location names
  # locations <- names(multi_micro[[1]])
  #
  # for(scen in scenarios) {
  #   for(loc in locations) {
  #
  #     list_days <- split.data.frame(x = multi_micro[[scen]][[loc]]$metout[,3:6],
  #                                   f = multi_micro[[scen]][[loc]]$metout[,1])
  #
  #     months <- seq(1, length(list_days), 1)
  #
  #     # calculate average microclimate conditions per day and save as vector
  #     T_loc <- vector(mode = "numeric", length = length(months))
  #     T_ref <- vector(mode = "numeric", length = length(months))
  #     RH_loc <- vector(mode = "numeric", length = length(months))
  #     RH_ref <- vector(mode = "numeric", length = length(months))
  #
  #
  #     for(month in months) {
  #       T_loc[month] <- mean(list_days[[month]][,1])
  #       T_ref[month] <- mean(list_days[[month]][,2])
  #       RH_loc[month] <- mean(list_days[[month]][,3])
  #       RH_ref[month] <- mean(list_days[[month]][,4])
  #     }
  #
  #     multi_micro[[scen]][[loc]]$months <- months
  #     multi_micro[[scen]][[loc]]$T_loc <- T_loc
  #     multi_micro[[scen]][[loc]]$T_ref <- T_ref
  #     multi_micro[[scen]][[loc]]$RH_loc <- RH_loc
  #     multi_micro[[scen]][[loc]]$RH_ref <- RH_ref
  #   }
  # }
  #
  #
  # for(scen in scenarios) {
  #   for(loc in locations) {
  #     # calculate and save total change
  #     multi_micro[[scen]][[loc]]$change_T_loc <- multi_micro[[scen]][[loc]]$T_loc - multi_micro[["present"]][[loc]]$T_loc
  #     multi_micro[[scen]][[loc]]$change_RH_loc <- multi_micro[[scen]][[loc]]$RH_loc - multi_micro[["present"]][[loc]]$RH_loc
  #     # calculate and save percentage change
  #     multi_micro[[scen]][[loc]]$perc_T_loc <- multi_micro[[scen]][[loc]]$T_loc / multi_micro[["present"]][[loc]]$T_loc
  #     multi_micro[[scen]][[loc]]$perc_RH_loc <- multi_micro[[scen]][[loc]]$RH_loc / multi_micro[["present"]][[loc]]$RH_loc
  #
  #     multi_micro[[scen]][[loc]]$metout <- NULL
  #     multi_micro[[scen]][[loc]]$environ <- NULL # never needed this in the first place
  #   }
  # }
  #
  # # unlist multi_ecto into a dataframe
  # multi_micro_tab <- data.table::rbindlist(lapply(multi_micro,
  #                                                function(x) data.table::rbindlist(x)),
  #                                         idcol = "id")
  #
  # # make dataframe with 'present' being both rcp 4.5 and 8.5 instead of none
  # present45 <- multi_micro_tab[which(stringr::str_detect(multi_micro_tab$timeper,
  #                                                       "present")),]
  # present85 <- present45
  # present45$rcp <- "45"
  # present85$rcp <- "85"
  #
  # multi_micro_tab_rcps <- rbind(multi_micro_tab[which(
  #   !stringr::str_detect(multi_micro_tab$timeper,
  #                        "present")
  # ),], present45, present85)
  #
  #
  # # rename times and rcps
  # multi_micro_tab_rcps$timeper <- gsub(pattern = "present.*",
  #                                     replacement = "pres",
  #                                     x = multi_micro_tab_rcps$timeper)
  # multi_micro_tab_rcps$timeper <- gsub(pattern = "2040_2059",
  #                                     replacement = "40-59",
  #                                     x = multi_micro_tab_rcps$timeper)
  # multi_micro_tab_rcps$timeper <- gsub(pattern = "2080_2099",
  #                                     replacement = "80-99",
  #                                     x = multi_micro_tab_rcps$timeper)
  # multi_micro_tab_rcps$rcp <- gsub(pattern = "45",
  #                                 replacement = "4.5",
  #                                 x = multi_micro_tab_rcps$rcp)
  # multi_micro_tab_rcps$rcp <- gsub(pattern = "85",
  #                             replacement = "8.5",
  #                             x = multi_micro_tab_rcps$rcp)
  # # make id, time, day and rcp factors
  # multi_micro_tab_rcps$id <- as.factor(multi_micro_tab_rcps$id)
  # multi_micro_tab_rcps$timeper <- as.factor(multi_micro_tab_rcps$timeper)
  # multi_micro_tab_rcps$rcp <- as.factor(multi_micro_tab_rcps$rcp)

  assertthat::assert_that(is.data.frame(multi_micro))
  multi_micro_tab_rcps <- multi_micro



  #### plot data ####

  # plot size
  unit <- "cm"
  width <- 22
  height <- 13.7


  ### Temperatures ###

  # local temperature over the year; facet grid locations and RCPs
  p <- ggplot2::ggplot(data = multi_micro_tab_rcps)+
    ggplot2::geom_point(size = 2,
                        mapping = ggplot2::aes_string(x = 'months',
                                                      y = 'T_loc',
                                                      colour = 'timeper',
                                                      shape = 'timeper'))+
    ggplot2::geom_line(size = 1,
                       mapping = ggplot2::aes_string(x = 'months',
                                                     y = 'T_loc',
                                                     colour = 'timeper',
                                                     group = 'timeper'))+
    ggplot2::scale_x_continuous(breaks = seq(2, 12, 2), limits = c(1,12))+
    # ggplot2::facet_wrap(~LID)+
    ggplot2::facet_grid(cols = ggplot2::vars(LID), rows = ggplot2::vars(rcp))+
    ggplot2::theme_bw()

  # save plot
  if(save_plot) {
    file_name <- "microclimates_localTemp.png"
    ggplot2::ggsave(filename = file_name, plot = p, device = png(),
                    path = save_path, units = unit,
                    width = width, height = height, dpi = 500)

    message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
    # unlink(file_name)
  } else { print(p) }


  # data frame without 'present' --> don't want it for displaying change in temp or RH
  multi_micro_tab_nopres <- multi_micro_tab_rcps[which(multi_micro_tab_rcps$timeper != "pres"),]

  # change in local temperature over the year; facet grid locations and RCPs
  p <- ggplot2::ggplot(data = multi_micro_tab_nopres)+
    ggplot2::geom_point(size = 2,
                        mapping = ggplot2::aes_string(x = 'months',
                                                      y = 'change_T_loc',
                                                      colour = 'timeper',
                                                      shape = 'timeper'))+
    ggplot2::geom_line(size = 1,
                       mapping = ggplot2::aes_string(x = 'months',
                                                     y = 'change_T_loc',
                                                     colour = 'timeper',
                                                     group = 'timeper'))+
    ggplot2::scale_x_continuous(breaks = seq(2, 12, 2), limits = c(1,12))+
    # ggplot2::facet_wrap(~LID)+
    ggplot2::facet_grid(cols = ggplot2::vars(LID), rows = ggplot2::vars(rcp))+
    ggplot2::theme_bw()

  # save plot
  if(save_plot) {
    file_name <- "microclimates_change_localTemp.png"
    ggplot2::ggsave(filename = file_name, plot = p, device = png(),
                    path = save_path, units = unit,
                    width = width, height = height, dpi = 500)

    message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
    # unlink(file_name)
  } else { print(p) }


  # % of change in local temperature over the year; facet grid locations and RCPs
  p <- ggplot2::ggplot(data = multi_micro_tab_nopres)+
    ggplot2::geom_point(size = 2,
                        mapping = ggplot2::aes_string(x = 'months',
                                                      y = 'perc_T_loc',
                                                      colour = 'timeper',
                                                      shape = 'timeper'))+
    ggplot2::geom_line(size = 1,
                       mapping = ggplot2::aes_string(x = 'months',
                                                     y = 'perc_T_loc',
                                                     colour = 'timeper',
                                                     group = 'timeper'))+
    ggplot2::geom_hline(ggplot2::aes(yintercept = 1, linetype = "present"),
                        colour = "black")+
    ggplot2::scale_linetype_manual(name = "Reference", values = 2,
                                   guide = ggplot2::guide_legend(override.aes = list(color = "black")))+
    ggplot2::scale_x_continuous(breaks = seq(2, 12, 2), limits = c(1,12))+
    # ggplot2::facet_wrap(~LID)+
    ggplot2::facet_grid(cols = ggplot2::vars(LID), rows = ggplot2::vars(rcp))+
    ggplot2::theme_bw()

  # save plot
  if(save_plot) {
    file_name <- "microclimates_percchange_localTemp.png"
    ggplot2::ggsave(filename = file_name, plot = p, device = png(),
                    path = save_path, units = unit,
                    width = width, height = height, dpi = 500)

    message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
    # unlink(file_name)
  } else { print(p) }


  ### just RCP 8.5 ###

  multi_micro_tab_85 <- multi_micro_tab_nopres[which(multi_micro_tab_nopres$rcp != "4.5"),]
  # local temperature over the year; facet grid locations and RCPs
  p <- ggplot2::ggplot(data = multi_micro_tab_85)+
    ggplot2::geom_point(size = 2,
                        mapping = ggplot2::aes_string(x = 'months',
                                                      y = 'T_loc',
                                                      colour = 'timeper',
                                                      shape = 'timeper'))+
    ggplot2::geom_line(size = 1,
                       mapping = ggplot2::aes_string(x = 'months',
                                                     y = 'T_loc',
                                                     colour = 'timeper',
                                                     group = 'timeper'))+
    ggplot2::scale_x_continuous(breaks = seq(2, 12, 2), limits = c(1,12))+
    # ggplot2::facet_wrap(~LID)+
    ggplot2::facet_grid(cols = ggplot2::vars(LID), rows = ggplot2::vars(rcp))+
    ggplot2::theme_bw()

  # save plot
  if(save_plot) {
    file_name <- "microclimates_localTemp_85.png"
    ggplot2::ggsave(filename = file_name, plot = p, device = png(),
                    path = save_path, units = unit,
                    width = width, height = height, dpi = 500)

    message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
    # unlink(file_name)
  } else { print(p) }


  # change in local temperature over the year; facet grid locations and RCPs
  p <- ggplot2::ggplot(data = multi_micro_tab_85)+
    ggplot2::geom_point(size = 2,
                        mapping = ggplot2::aes_string(x = 'months',
                                                      y = 'change_T_loc',
                                                      colour = 'timeper',
                                                      shape = 'timeper'))+
    ggplot2::geom_line(size = 1,
                       mapping = ggplot2::aes_string(x = 'months',
                                                     y = 'change_T_loc',
                                                     colour = 'timeper',
                                                     group = 'timeper'))+
    ggplot2::scale_x_continuous(breaks = seq(2, 12, 2), limits = c(1,12))+
    # ggplot2::facet_wrap(~LID)+
    ggplot2::facet_grid(cols = ggplot2::vars(LID), rows = ggplot2::vars(rcp))+
    ggplot2::theme_bw()

  # save plot
  if(save_plot) {
    file_name <- "microclimates_change_localTemp_85.png"
    ggplot2::ggsave(filename = file_name, plot = p, device = png(),
                    path = save_path, units = unit,
                    width = width, height = height, dpi = 500)

    message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
    # unlink(file_name)
  } else { print(p) }



  # % of change in local temperature over the year; facet grid locations and RCPs
  p <- ggplot2::ggplot(data = multi_micro_tab_85)+
    ggplot2::geom_point(size = 2,
                        mapping = ggplot2::aes_string(x = 'months',
                                                      y = 'perc_T_loc',
                                                      colour = 'timeper',
                                                      shape = 'timeper'))+
    ggplot2::geom_line(size = 1,
                       mapping = ggplot2::aes_string(x = 'months',
                                                     y = 'perc_T_loc',
                                                     colour = 'timeper',
                                                     group = 'timeper'))+
    ggplot2::geom_hline(ggplot2::aes(yintercept = 1, linetype = "present"),
                        colour = "black")+
    ggplot2::scale_linetype_manual(name = "Reference", values = 2,
                                   guide = ggplot2::guide_legend(override.aes = list(color = "black")))+
    ggplot2::scale_x_continuous(breaks = seq(2, 12, 2), limits = c(1,12))+
    # ggplot2::facet_wrap(~LID)+
    ggplot2::facet_grid(cols = ggplot2::vars(LID), rows = ggplot2::vars(rcp))+
    ggplot2::theme_bw()

  # save plot
  if(save_plot) {
    file_name <- "microclimates_percchange_localTemp_85.png"
    ggplot2::ggsave(filename = file_name, plot = p, device = png(),
                    path = save_path, units = unit,
                    width = width, height = height, dpi = 500)

    message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
    # unlink(file_name)
  } else { print(p) }


  # # reference temperature over the year; facet grid locations and RCPs
  # p <- ggplot2::ggplot(data = multi_micro_tab_rcps)+
  #   ggplot2::geom_point(size = 2,
  #                       mapping = ggplot2::aes_string(x = 'months',
  #                                                     y = 'T_ref',
  #                                                     colour = 'timeper',
  #                                                     shape = 'timeper'))+
  #   ggplot2::geom_line(size = 1,
  #                      mapping = ggplot2::aes_string(x = 'months',
  #                                                    y = 'T_ref',
  #                                                    colour = 'timeper',
  #                                                    group = 'timeper'))+
  #   ggplot2::scale_x_continuous(breaks = seq(2, 12, 2), limits = c(1,12))+
  #   # ggplot2::facet_wrap(~LID)+
  #   ggplot2::facet_grid(cols = ggplot2::vars(LID), rows = ggplot2::vars(rcp))+
  #   ggplot2::theme_bw()
  #
  # # save plot
  # if(save_plot) {
  #   file_name <- "microclimates_refTemp.png"
  #   ggplot2::ggsave(filename = file_name, plot = p, device = png(),
  #                   path = save_path, units = unit,
  #                   width = width, height = height, dpi = 500)
  #
  #   message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
  #   # unlink(file_name)
  # } else { print(p) }




  ### Rel. humidity ###

  # local relative humidity over the year; facet grid locations and RCPs
  p <- ggplot2::ggplot(data = multi_micro_tab_rcps)+
    ggplot2::geom_point(size = 2,
                        mapping = ggplot2::aes_string(x = 'months',
                                                      y = 'RH_loc',
                                                      colour = 'timeper',
                                                      shape = 'timeper'))+
    ggplot2::geom_line(size = 1,
                       mapping = ggplot2::aes_string(x = 'months',
                                                     y = 'RH_loc',
                                                     colour = 'timeper',
                                                     group = 'timeper'))+
    ggplot2::scale_x_continuous(breaks = seq(2, 12, 2), limits = c(1,12))+
    # ggplot2::facet_wrap(~LID)+
    ggplot2::facet_grid(cols = ggplot2::vars(LID), rows = ggplot2::vars(rcp))+
    ggplot2::theme_bw()

  # save plot
  if(save_plot) {
    file_name <- "microclimates_localRH.png"
    ggplot2::ggsave(filename = file_name, plot = p, device = png(),
                    path = save_path, units = unit,
                    width = width, height = height, dpi = 500)

    message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
    # unlink(file_name)
  } else { print(p) }


  # # total change in local RH over the year; facet grid locations and RCPs
  # p <- ggplot2::ggplot(data = multi_micro_tab_rcps[which(multi_micro_tab_rcps$timeper != "pres"),])+
  #   ggplot2::geom_point(size = 2,
  #                       mapping = ggplot2::aes_string(x = 'months',
  #                                                     y = 'change_RH_loc',
  #                                                     colour = 'timeper',
  #                                                     shape = 'timeper'))+
  #   ggplot2::geom_line(size = 1,
  #                      mapping = ggplot2::aes_string(x = 'months',
  #                                                    y = 'change_RH_loc',
  #                                                    colour = 'timeper',
  #                                                    group = 'timeper'))+
  #   # ggplot2::facet_wrap(~LID)+
  #   ggplot2::facet_grid(cols = ggplot2::vars(LID), rows = ggplot2::vars(rcp))+
  #   ggplot2::theme_bw()
  #
  # # save plot
  # if(save_plot) {
  #   file_name <- "microclimates_change_localRH.png"
  #   ggplot2::ggsave(filename = file_name, plot = p, device = png(),
  #                   path = save_path, units = unit,
  #                   width = width, height = height, dpi = 500)
  #
  #   message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
  #   # unlink(file_name)
  # } else { print(p) }


  # % of change in local RH over the year; facet grid locations and RCPs
  p <- ggplot2::ggplot(data = multi_micro_tab_rcps[which(multi_micro_tab_rcps$timeper != "pres"),])+
    ggplot2::geom_point(size = 2,
                        mapping = ggplot2::aes_string(x = 'months',
                                                      y = 'perc_RH_loc',
                                                      colour = 'timeper',
                                                      shape = 'timeper'))+
    ggplot2::geom_line(size = 1,
                       mapping = ggplot2::aes_string(x = 'months',
                                                     y = 'perc_RH_loc',
                                                     colour = 'timeper',
                                                     group = 'timeper'))+
    ggplot2::geom_hline(ggplot2::aes(yintercept = 1, linetype = "present"),
                        colour = "black")+
    ggplot2::scale_linetype_manual(name = "Reference", values = 2,
                                   guide = ggplot2::guide_legend(override.aes = list(color = "black")))+
    ggplot2::scale_x_continuous(breaks = seq(2, 12, 2), limits = c(1,12))+
    # ggplot2::facet_wrap(~LID)+
    ggplot2::facet_grid(cols = ggplot2::vars(LID), rows = ggplot2::vars(rcp))+
    ggplot2::theme_bw()

  # save plot
  if(save_plot) {
    file_name <- "microclimates_percchange_localRH.png"
    ggplot2::ggsave(filename = file_name, plot = p, device = png(),
                    path = save_path, units = unit,
                    width = width, height = height, dpi = 500)

    message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
    # unlink(file_name)
  } else { print(p) }


  # just RCP 8.5
  # % of change in local RH over the year; facet grid locations and RCPs
  p <- ggplot2::ggplot(data = multi_micro_tab_85)+
    ggplot2::geom_point(size = 2,
                        mapping = ggplot2::aes_string(x = 'months',
                                                      y = 'perc_RH_loc',
                                                      colour = 'timeper',
                                                      shape = 'timeper'))+
    ggplot2::geom_line(size = 1,
                       mapping = ggplot2::aes_string(x = 'months',
                                                     y = 'perc_RH_loc',
                                                     colour = 'timeper',
                                                     group = 'timeper'))+
    ggplot2::geom_hline(ggplot2::aes(yintercept = 1, linetype = "present"),
                        colour = "black")+
    ggplot2::scale_linetype_manual(name = "Reference", values = 2,
                                   guide = ggplot2::guide_legend(override.aes = list(color = "black")))+
    ggplot2::scale_x_continuous(breaks = seq(2, 12, 2), limits = c(1,12))+
    # ggplot2::facet_wrap(~LID)+
    ggplot2::facet_grid(cols = ggplot2::vars(LID), rows = ggplot2::vars(rcp))+
    ggplot2::theme_bw()

  # save plot
  if(save_plot) {
    file_name <- "microclimates_percchange_localRH_85.png"
    ggplot2::ggsave(filename = file_name, plot = p, device = png(),
                    path = save_path, units = unit,
                    width = width, height = height, dpi = 500)

    message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
    # unlink(file_name)
  } else { print(p) }


  # # reference relative humidity over the year; facet grid locations and RCPs
  # p <- ggplot2::ggplot(data = multi_micro_tab_rcps)+
  #   ggplot2::geom_point(size = 2,
  #                       mapping = ggplot2::aes_string(x = 'months',
  #                                                     y = 'RH_ref',
  #                                                     colour = 'timeper',
  #                                                     shape = 'timeper'))+
  #   ggplot2::geom_line(size = 1,
  #                      mapping = ggplot2::aes_string(x = 'months',
  #                                                    y = 'RH_ref',
  #                                                    colour = 'timeper',
  #                                                    group = 'timeper'))+
  #   ggplot2::scale_x_continuous(breaks = seq(2, 12, 2), limits = c(1,12))+
  #   # ggplot2::facet_wrap(~LID)+
  #   ggplot2::facet_grid(cols = ggplot2::vars(LID), rows = ggplot2::vars(rcp))+
  #   ggplot2::theme_bw()
  #
  # # save plot
  # if(save_plot) {
  #   file_name <- "microclimates_refRH.png"
  #   ggplot2::ggsave(filename = file_name, plot = p, device = png(),
  #                   path = save_path, units = unit,
  #                   width = width, height = height, dpi = 500)
  #
  #   message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
  #   # unlink(file_name)
  # } else { print(p) }

}

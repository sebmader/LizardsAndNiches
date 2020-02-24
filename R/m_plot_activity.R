#' @title Plot activity times over the organisms reflectance
#' @description This function plots the output of the biophysical model of ectotherms
#' from the NicheMapR package.
#' @name m_plot_activity
#' @param multi_ecto A tidy data frame of summarised output results of the ecotherm function
#' containing the activity times (total, percentage, ratios, etc.), absorptivity, weight,
#' yearly averaged microclimate variables per scenario and location (for details see ?m_tidy_output).
#' @param save_plot Boolean whether the plot should be saved or not (default = FALSE).
#' @return Plot
# @importFrom graphics abline legend text
#' @importFrom grDevices png
#' @export

m_plot_activity <- function(multi_ecto, save_plot = FALSE) {

  # create directory of save path if applicable
  save_path <- "./Plots/activity_plots/"

  if(save_plot) {

    if(!dir.exists(save_path)) {
      dir.create(save_path, recursive = T)
      cat(paste0("Created folder ", save_path, "\n"))
    }
  }
  # make tidyverse data frame with activity times summed over the whole year
  # and absorptivity per location per climate scenario and time period

    # vector of scenario names
  # scenarios <- names(multi_ecto)
  #
  #   # vector of location names
  # locations <- names(multi_ecto[[1]])
  #
  # # loop through multi_ecto and save total activity time per year
  # for(scen in scenarios) {
  #   for(loc in locations) {
  #     ndays <- length(unique(multi_ecto[[scen]][[loc]]$environ[,1]))
  #     nyears <- length(unique(multi_ecto[[scen]][[loc]]$environ[,2]))
  #
  #     # count hours with activity == 2 for activity times
  #     h_active <- length(which(multi_ecto[[scen]][[loc]]$environ[,9] == 2))
  #     perc_active <- h_active/(nyears*ndays*24)
  #
  #     # count hours with activity == 1 for basking times
  #     h_bask <- length(which(multi_ecto[[scen]][[loc]]$environ[,9] == 1))
  #     perc_bask <- h_bask/(nyears*ndays*24)
  #
  #     # save in multi_ecto
  #     multi_ecto[[scen]][[loc]]$h_active <- h_active
  #     multi_ecto[[scen]][[loc]]$perc_active <- perc_active
  #     multi_ecto[[scen]][[loc]]$h_bask <- h_bask
  #     multi_ecto[[scen]][[loc]]$perc_bask <- perc_bask
  #     multi_ecto[[scen]][[loc]]$act_bask_ratio <- h_active/h_bask
  #     multi_ecto[[scen]][[loc]]$ndays <- ndays
  #     multi_ecto[[scen]][[loc]]$nyears <- nyears
  #   }
  # }
  #
  # # no need for the environ table anymore (and it makes unlisting very tricky)
  # for(scen in scenarios) {
  #   for(loc in locations) {
  #     multi_ecto[[scen]][[loc]]$environ <- NULL
  #     multi_ecto[[scen]][[loc]]$metout <- NULL # never needed this in the first place...
  #     # calculate activity change to present
  #     multi_ecto[[scen]][[loc]]$change_act <- multi_ecto[[scen]][[loc]]$h_active - multi_ecto[["present"]][[loc]]$h_active
  #     multi_ecto[[scen]][[loc]]$change_bask <- multi_ecto[[scen]][[loc]]$h_bask - multi_ecto[["present"]][[loc]]$h_bask
  #     # calculate percentage of change
  #     multi_ecto[[scen]][[loc]]$perc_change_act <- multi_ecto[[scen]][[loc]]$h_active / multi_ecto[["present"]][[loc]]$h_active
  #     multi_ecto[[scen]][[loc]]$perc_change_bask <- multi_ecto[[scen]][[loc]]$h_bask / multi_ecto[["present"]][[loc]]$h_bask
  #
  #   }
  # }
  #
  # # unlist multi_ecto into a dataframe
  # multi_ecto_tab <- data.table::rbindlist(lapply(multi_ecto,
  #                                            function(x) data.table::rbindlist(x)),
  #                                     idcol = "id")
  #
  # # # calculate the activity-basking ratio and add it to dataframe
  # # act_bask_ratio <- multi_ecto_tab$h_active/multi_ecto_tab$h_bask
  # # multi_ecto_tab <- cbind(multi_ecto_tab, act_bask_ratio)
  #
  #   # make dataframe with 'present' being both rcp 4.5 and 8.5 instead of none
  # present45 <- multi_ecto_tab[which(stringr::str_detect(multi_ecto_tab$timeper,
  #                                                       "present")),]
  # present85 <- present45
  # present45$rcp <- "45"
  # present85$rcp <- "85"
  #
  # multi_ecto_tab_rcps <- rbind(multi_ecto_tab[which(
  #                                     !stringr::str_detect(multi_ecto_tab$timeper,
  #                                                          "present")
  #                                     ),], present45, present85)
  #
  #   # change 'timeper' to nicely displayable strings
  # multi_ecto_tab_rcps$timeper <- gsub(pattern = "present.*",
  #                                     replacement = "pres",
  #                                     x = multi_ecto_tab_rcps$timeper)
  # multi_ecto_tab_rcps$timeper <- gsub(pattern = "2040_2059",
  #                                     replacement = "40-59",
  #                                     x = multi_ecto_tab_rcps$timeper)
  # multi_ecto_tab_rcps$timeper <- gsub(pattern = "2080_2099",
  #                                     replacement = "80-99",
  #                                     x = multi_ecto_tab_rcps$timeper)
  #
  # multi_ecto_tab_rcps$id <- as.factor(multi_ecto_tab_rcps$id)
  # multi_ecto_tab_rcps$timeper <- as.factor(multi_ecto_tab_rcps$timeper)
  # multi_ecto_tab_rcps$rcp <- as.factor(multi_ecto_tab_rcps$rcp)

  assertthat::assert_that(is.data.frame(multi_ecto))
  multi_ecto_tab_rcps <- multi_ecto

  #### plot the data ####

  # plot size
  unit <- "cm"
  width <- 22
  height <- 13.7

  ### split into locations ###

  #   # act-bask ratio vs. time point; facet grid locations
  # p <- ggplot2::ggplot(data = multi_ecto_tab_rcps)+
  #   ggplot2::geom_point(size = 2,
  #                       mapping = ggplot2::aes_string(x = 'timeper',
  #                                                     y = 'act_bask_ratio',
  #                                                     colour = 'rcp',
  #                                                     shape = 'rcp'))+
  #   ggplot2::geom_line(size = 1,
  #                      mapping = ggplot2::aes_string(x = 'timeper',
  #                                                    y = 'act_bask_ratio',
  #                                                    colour = 'rcp',
  #                                                    group = 'rcp'))+
  #   ggplot2::geom_hline(ggplot2::aes(yintercept = 1), linetype = "dashed",
  #                       colour = "black")+
  #   ggplot2::scale_x_discrete(limits = c("pres", "40-59", "80-99"))+
  #   ggplot2::facet_wrap(~LID)+
  #   ggplot2::theme_bw()
  #
  #   # save plot
  # if(save_plot) {
  #   file_name <- "act-bask_ratio_scenario.png"
  #   ggplot2::ggsave(filename = file_name, plot = p, device = png(),
  #                   path = save_path, units = unit,
  #                   width = width, height = height, dpi = 500)
  #
  #   message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
  #   # unlink(file_name)
  # } else { print(p) }
  #
  #   # total active hours vs. time point; facet grid locations
  # p <- ggplot2::ggplot(data = multi_ecto_tab_rcps)+
  #   ggplot2::geom_point(size = 2,
  #                       mapping = ggplot2::aes_string(x = 'timeper',
  #                                                     y = 'h_active',
  #                                                     colour = 'rcp',
  #                                                     shape = 'rcp'))+
  #   ggplot2::geom_line(size = 1,
  #                      mapping = ggplot2::aes_string(x = 'timeper',
  #                                                    y = 'h_active',
  #                                                    colour = 'rcp',
  #                                                    group = 'rcp'))+
  #   ggplot2::scale_x_discrete(limits = c("pres", "40-59", "80-99"))+
  #   ggplot2::facet_wrap(~LID)+
  #   ggplot2::theme_bw()
  #
  # # print or save plot
  # if(save_plot) {
  #   file_name <- "total_act_scenario.png"
  #   ggplot2::ggsave(filename = file_name, plot = p, device = png(),
  #                   path = save_path, units = unit,
  #                   width = width, height = height, dpi = 500)
  #
  #   message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
  #   # unlink(file_name)
  # } else { print(p) }
  #
  # # change in active hours vs. time point; facet grid locations
  # p <- ggplot2::ggplot(data = multi_ecto_tab_rcps)+
  #   ggplot2::geom_point(size = 2,
  #                       mapping = ggplot2::aes_string(x = 'timeper',
  #                                                     y = 'change_act',
  #                                                     colour = 'rcp',
  #                                                     shape = 'rcp'))+
  #   ggplot2::geom_line(size = 1,
  #                      mapping = ggplot2::aes_string(x = 'timeper',
  #                                                    y = 'change_act',
  #                                                    colour = 'rcp',
  #                                                    group = 'rcp'))+
  #   ggplot2::scale_x_discrete(limits = c("pres", "40-59", "80-99"))+
  #   ggplot2::facet_wrap(~LID)+
  #   ggplot2::theme_bw()
  #
  # # print or save plot
  # if(save_plot) {
  #   file_name <- "change_act_scenario.png"
  #   ggplot2::ggsave(filename = file_name, plot = p, device = png(),
  #                   path = save_path, units = unit,
  #                   width = width, height = height, dpi = 500)
  #
  #   message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
  #   # unlink(file_name)
  # } else { print(p) }


  # percentage of change in active hours vs. time point; facet grid locations
  p <- ggplot2::ggplot(data = multi_ecto_tab_rcps)+
    ggplot2::geom_point(size = 2,
                        mapping = ggplot2::aes_(x = quote(timeper),
                                                y = quote(perc_change_act),
                                                colour = quote(rcp),
                                                shape = quote(rcp)))+
    ggplot2::geom_line(size = 1,
                       mapping = ggplot2::aes_(x = quote(timeper),
                                               y = quote(perc_change_act),
                                               colour = quote(rcp),
                                               group = quote(ID)))+
    ggplot2::geom_hline(ggplot2::aes(yintercept = 1, linetype = "present"),
                        colour = "black")+
    ggplot2::scale_linetype_manual(name = "Reference", values = 2,
                                   guide = ggplot2::guide_legend(override.aes = list(color = "black")))+
    ggplot2::scale_x_discrete(limits = c("pres", "40-59", "80-99"))+
    ggplot2::facet_wrap(~LID)+
    ggplot2::theme_bw()

  # print or save plot
  if(save_plot) {
    file_name <- "perc_change_act_scenario.png"
    ggplot2::ggsave(filename = file_name, plot = p, device = png(),
                    path = save_path, units = unit,
                    width = width, height = height, dpi = 500)

    message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
    # unlink(file_name)
  } else { print(p) }



  ### activity vs. physio data ###

  multi_ecto_tab_nopres <- multi_ecto_tab_rcps[which(multi_ecto_tab_rcps$timeper != "pres"),]
  # absorptivity #

  #   # activity-basking hours ratio vs. absorptivity (all locations & scenarios)
  # p <- ggplot2::ggplot(data = multi_ecto_tab)+
  #   ggplot2::geom_point(size = 2,
  #                       mapping = ggplot2::aes_string(x = 'absorp',
  #                                                     y = 'act_bask_ratio',
  #                                                     colour = 'id',
  #                                                     shape = 'id'))+
  #   ggplot2::geom_line(size = 1,
  #                      mapping = ggplot2::aes_string(x = 'absorp',
  #                                                    y = 'act_bask_ratio',
  #                                                    colour = 'id'))+
  #   ggplot2::annotate(geom = "text", x = unique(multi_ecto_tab$absorp),
  #                     y = 0.1 * (max(multi_ecto_tab_nopres$act_bask_ratio) -
  #                                  min(multi_ecto_tab_nopres$act_bask_ratio)) +
  #                       unlist(lapply(split(multi_ecto_tab,
  #                                      f = multi_ecto_tab$LID),
  #                                function(x) max(x$act_bask_ratio))),
  #                     label = unique(multi_ecto_tab$LID))+
  #   ggplot2::labs(title = "Activity-basking ratio (per year) vs. absorptivity")+
  #   ggplot2::theme_bw()
  #
  # # save plot
  # if(save_plot) {
  #   file_name <- "act-bask_ratio_absorp.png"
  #   ggplot2::ggsave(filename = file_name, plot = p, device = png(),
  #                   path = save_path, units = unit,
  #                   width = width, height = height, dpi = 500)
  #
  #   message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
  #   # unlink(file_name)
  # } else { print(p) }
  #
  #
  #   # total hours active vs. absorptivity (all locations & scenarios)
  # p <- ggplot2::ggplot(data = multi_ecto_tab)+
  #   ggplot2::geom_point(size = 2,
  #                       mapping = ggplot2::aes_string(x = 'absorp',
  #                                                     y = 'h_active',
  #                                                     colour = 'id',
  #                                                     shape = 'id'))+
  #   ggplot2::geom_line(size = 1,
  #                      mapping = ggplot2::aes_string(x = 'absorp',
  #                                                    y = 'h_active',
  #                                                    colour = 'id'))+
  #   ggplot2::annotate(geom = "text", x = unique(multi_ecto_tab$absorp),
  #                     y = 0.1 * (max(multi_ecto_tab_nopres$h_active) -
  #                                  min(multi_ecto_tab_nopres$h_active)) +
  #                       unlist(lapply(split(multi_ecto_tab,
  #                                           f = multi_ecto_tab$LID),
  #                                     function(x) max(x$h_active))),
  #                     label = unique(multi_ecto_tab$LID))+
  #   ggplot2::labs(title = "Hours of activity (per year) vs. absorptivity")+
  #   ggplot2::theme_bw()
  #
  # # save plot
  # if(save_plot) {
  #   file_name <- "total_act_absorp.png"
  #   ggplot2::ggsave(filename = file_name, plot = p, device = png(),
  #                   path = save_path, units = unit,
  #                   width = width, height = height, dpi = 500)
  #
  #   message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
  #   # unlink(file_name)
  # } else { print(p) }
  #
  #
  #
  #
  # # change in hours active vs. absorptivity (all locations & scenarios)
  # p <- ggplot2::ggplot(data = multi_ecto_tab_nopres)+
  #   ggplot2::geom_point(size = 2,
  #                       mapping = ggplot2::aes_string(x = 'absorp',
  #                                                     y = 'change_act',
  #                                                     colour = 'id',
  #                                                     shape = 'id'))+
  #   ggplot2::geom_line(size = 1,
  #                      mapping = ggplot2::aes_string(x = 'absorp',
  #                                                    y = 'change_act',
  #                                                    colour = 'id'))+
  #   ggplot2::annotate(geom = "text", x = unique(multi_ecto_tab_nopres$absorp),
  #                     y = 0.1 * (max(multi_ecto_tab_nopres$change_act) -
  #                                  min(multi_ecto_tab_nopres$change_act)) +
  #                       unlist(lapply(split(multi_ecto_tab_nopres,
  #                                           f = multi_ecto_tab_nopres$LID),
  #                                     function(x) max(x$change_act))),
  #                     label = unique(multi_ecto_tab_nopres$LID))+
  #   ggplot2::labs(title = "Change in activity hours (per year) vs. absorptivity")+
  #   ggplot2::theme_bw()
  #
  # # save plot
  # if(save_plot) {
  #   file_name <- "change_act_absorp.png"
  #   ggplot2::ggsave(filename = file_name, plot = p, device = png(),
  #                   path = save_path, units = unit,
  #                   width = width, height = height, dpi = 500)
  #
  #   message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
  #   # unlink(file_name)
  # } else { print(p) }


  # percentage of change in hours active vs. absorptivity (all locations & scenarios)
  p <- ggplot2::ggplot(data = multi_ecto_tab_nopres)+
    ggplot2::geom_point(size = 2,
                        mapping = ggplot2::aes_string(x = 'absorp',
                                                      y = 'perc_change_act',
                                                      colour = 'scenario',
                                                      shape = 'scenario'))+
    ggplot2::geom_line(size = 1,
                       mapping = ggplot2::aes_string(x = 'absorp',
                                                     y = 'perc_change_act',
                                                     colour = 'scenario'))+
    ggplot2::geom_hline(ggplot2::aes(yintercept = 1, linetype = "present"),
                        colour = "black")+
    ggplot2::scale_linetype_manual(name = "Reference", values = 2,
                                   guide = ggplot2::guide_legend(override.aes = list(color = "black")))+
    # ggplot2::annotate(geom = "text", x = unique(multi_ecto_tab_nopres$absorp),
    #                   y = 0.1 * (max(multi_ecto_tab_nopres$perc_change_act) -
    #                                min(multi_ecto_tab_nopres$perc_change_act)) +
    #                     unlist(lapply(split(multi_ecto_tab_nopres,
    #                                         f = multi_ecto_tab_nopres$LID),
    #                                   function(x) max(x$perc_change_act))),
    #                   label = unique(multi_ecto_tab_nopres$LID))+
    ggplot2::labs(title = "% of change in activity (per year) vs. absorptivity")+
    ggplot2::theme_bw()

  # save plot
  if(save_plot) {
    file_name <- "perc_change_act_absorp.png"
    ggplot2::ggsave(filename = file_name, plot = p, device = png(),
                    path = save_path, units = unit,
                    width = width, height = height, dpi = 500)

    message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
    # unlink(file_name)
  } else { print(p) }


  ### just RCP 8.5 ###
  multi_ecto_tab_85 <- multi_ecto_tab_nopres[which(multi_ecto_tab_nopres$rcp != "4.5"),]

  # percentage of change in hours active vs. absorptivity (all locations & scenarios)
  p <- ggplot2::ggplot(data = multi_ecto_tab_85)+
    ggplot2::geom_point(size = 2,
                        mapping = ggplot2::aes_string(x = 'absorp',
                                                      y = 'perc_change_act',
                                                      colour = 'timeper',
                                                      shape = 'timeper'))+
    ggplot2::geom_line(size = 1,
                       mapping = ggplot2::aes_string(x = 'absorp',
                                                     y = 'perc_change_act',
                                                     colour = 'timeper'))+
    ggplot2::geom_hline(ggplot2::aes(yintercept = 1, linetype = "present"),
                        colour = "black")+
    ggplot2::scale_linetype_manual(name = "Reference", values = 2,
                                   guide = ggplot2::guide_legend(override.aes = list(color = "black")))+
    # ggplot2::annotate(geom = "text", x = unique(multi_ecto_tab_85$absorp),
    #                   y = 0.1 * (max(multi_ecto_tab_85$perc_change_act) -
    #                                min(multi_ecto_tab_85$perc_change_act)) +
    #                     unlist(lapply(split(multi_ecto_tab_85,
    #                                         f = multi_ecto_tab_85$LID),
    #                                   function(x) max(x$perc_change_act))),
    #                   label = unique(multi_ecto_tab_85$LID))+
    ggplot2::labs(title = "% of change in activity (per year) vs. absorptivity")+
    ggplot2::theme_bw()

  # save plot
  if(save_plot) {
    file_name <- "perc_change_act_absorp_85.png"
    ggplot2::ggsave(filename = file_name, plot = p, device = png(),
                    path = save_path, units = unit,
                    width = width, height = height, dpi = 500)

    message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
    # unlink(file_name)
  } else { print(p) }


  # body weight #

  # # activity-basking hours ratio vs. body weight (all locations & scenarios)
  # p <- ggplot2::ggplot(data = multi_ecto_tab)+
  #   ggplot2::geom_point(size = 2,
  #                       mapping = ggplot2::aes_string(x = 'ww',
  #                                                     y = 'act_bask_ratio',
  #                                                     colour = 'id',
  #                                                     shape = 'id'))+
  #   ggplot2::geom_line(size = 1,
  #                      mapping = ggplot2::aes_string(x = 'ww',
  #                                                    y = 'act_bask_ratio',
  #                                                    colour = 'id'))+
  #   ggplot2::annotate(geom = "text", x = unique(multi_ecto_tab$ww),
  #                     y = 0.1 * (max(multi_ecto_tab$act_bask_ratio) -
  #                                  min(multi_ecto_tab$act_bask_ratio)) +
  #                       unlist(lapply(split(multi_ecto_tab,
  #                                           f = multi_ecto_tab$LID),
  #                                     function(x) max(x$act_bask_ratio))),
  #                     label = unique(multi_ecto_tab$LID))+
  #   ggplot2::labs(title = "Activity-basking ratio (per year) vs. body weight")+
  #   ggplot2::theme_bw()
  #
  # # save plot
  # if(save_plot) {
  #   file_name <- "act-bask_ratio_ww.png"
  #   ggplot2::ggsave(filename = file_name, plot = p, device = png(),
  #                   path = save_path, units = unit,
  #                   width = width, height = height, dpi = 500)
  #
  #   message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
  #   # unlink(file_name)
  # } else { print(p) }


  # # percantage of change in hours active vs. weight (all locations & scenarios)
  # p <- ggplot2::ggplot(data = multi_ecto_tab_nopres)+
  #   ggplot2::geom_point(size = 2,
  #                       mapping = ggplot2::aes_string(x = 'ww',
  #                                                     y = 'perc_change_act',
  #                                                     colour = 'id',
  #                                                     shape = 'id'))+
  #   ggplot2::geom_line(size = 1,
  #                      mapping = ggplot2::aes_string(x = 'ww',
  #                                                    y = 'perc_change_act',
  #                                                    colour = 'id'))+
  #   ggplot2::geom_hline(ggplot2::aes(yintercept = 1, linetype = "present"),
  #                       colour = "black")+
  #   ggplot2::scale_linetype_manual(name = "Reference", values = 2,
  #                                  guide = ggplot2::guide_legend(override.aes = list(color = "black")))+
  #   ggplot2::annotate(geom = "text", x = unique(multi_ecto_tab_nopres$ww),
  #                     y = 0.1 * (max(multi_ecto_tab_nopres$perc_change_act) -
  #                                  min(multi_ecto_tab_nopres$perc_change_act)) +
  #                       unlist(lapply(split(multi_ecto_tab_nopres,
  #                                           f = multi_ecto_tab_nopres$LID),
  #                                     function(x) max(x$perc_change_act))),
  #                     label = unique(multi_ecto_tab_nopres$LID))+
  #   ggplot2::labs(title = "% of change in activity (per year) vs. body weight")+
  #   ggplot2::theme_bw()
  #
  # # save plot
  # if(save_plot) {
  #   file_name <- "perc_change_act_weight.png"
  #   ggplot2::ggsave(filename = file_name, plot = p, device = png(),
  #                   path = save_path, units = unit,
  #                   width = width, height = height, dpi = 500)
  #
  #   message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
  #   # unlink(file_name)
  # } else { print(p) }


  # just RCP 8.5
  # percantage of change in hours active vs. weight (all locations & scenarios)
  p <- ggplot2::ggplot(data = multi_ecto_tab_85)+
    ggplot2::geom_point(size = 2,
                        mapping = ggplot2::aes_string(x = 'ww',
                                                      y = 'perc_change_act',
                                                      colour = 'timeper',
                                                      shape = 'timeper'))+
    ggplot2::geom_line(size = 1,
                       mapping = ggplot2::aes_string(x = 'ww',
                                                     y = 'perc_change_act',
                                                     colour = 'timeper'))+
    ggplot2::geom_hline(ggplot2::aes(yintercept = 1, linetype = "present"),
                        colour = "black")+
    ggplot2::scale_linetype_manual(name = "Reference", values = 2,
                                   guide = ggplot2::guide_legend(override.aes = list(color = "black")))+
    # ggplot2::annotate(geom = "text", x = unique(multi_ecto_tab_85$ww),
    #                   y = 0.1 * (max(multi_ecto_tab_85$perc_change_act) -
    #                                min(multi_ecto_tab_85$perc_change_act)) +
    #                     unlist(lapply(split(multi_ecto_tab_85,
    #                                         f = multi_ecto_tab_85$LID),
    #                                   function(x) max(x$perc_change_act))),
    #                   label = unique(multi_ecto_tab_85$LID))+
    ggplot2::labs(title = "% of change in activity (per year) vs. body weight")+
    ggplot2::theme_bw()

  # save plot
  if(save_plot) {
    file_name <- "perc_change_act_weight_85.png"
    ggplot2::ggsave(filename = file_name, plot = p, device = png(),
                    path = save_path, units = unit,
                    width = width, height = height, dpi = 500)

    message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
    # unlink(file_name)
  } else { print(p) }


  # body length #

  # # activity-basking hours ratio vs. body length (all locations & scenarios)
  # p <- ggplot2::ggplot(data = multi_ecto_tab)+
  #   ggplot2::geom_point(size = 2,
  #                       mapping = ggplot2::aes_string(x = 'ttl',
  #                                                     y = 'act_bask_ratio',
  #                                                     colour = 'id',
  #                                                     shape = 'id'))+
  #   ggplot2::geom_line(size = 1,
  #                      mapping = ggplot2::aes_string(x = 'ttl',
  #                                                    y = 'act_bask_ratio',
  #                                                    colour = 'id'))+
  #   ggplot2::annotate(geom = "text", x = unique(multi_ecto_tab$ttl),
  #                     y = 0.1 * (max(multi_ecto_tab$act_bask_ratio) -
  #                                  min(multi_ecto_tab$act_bask_ratio)) +
  #                       unlist(lapply(split(multi_ecto_tab,
  #                                           f = multi_ecto_tab$LID),
  #                                     function(x) max(x$act_bask_ratio))),
  #                     label = unique(multi_ecto_tab$LID))+
  #   ggplot2::labs(title = "Activity-basking ratio (per year) vs. body length")+
  #   ggplot2::theme_bw()
  #
  # # save plot
  # if(save_plot) {
  #   file_name <- "act-bask_ratio_ttl.png"
  #   ggplot2::ggsave(filename = file_name, plot = p, device = png(),
  #                   path = save_path, units = unit,
  #                   width = width, height = height, dpi = 500)
  #
  #   message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
  #   # unlink(file_name)
  # } else { print(p) }
  #
  # # percantage of change in hours active vs. absorptivity (all locations & scenarios)
  # p <- ggplot2::ggplot(data = multi_ecto_tab_nopres)+
  #   ggplot2::geom_point(size = 2,
  #                       mapping = ggplot2::aes_string(x = 'ttl',
  #                                                     y = 'perc_change_act',
  #                                                     colour = 'id',
  #                                                     shape = 'id'))+
  #   ggplot2::geom_line(size = 1,
  #                      mapping = ggplot2::aes_string(x = 'ttl',
  #                                                    y = 'perc_change_act',
  #                                                    colour = 'id'))+
  #   ggplot2::geom_hline(ggplot2::aes(yintercept = 1, linetype = "present"),
  #                       colour = "black")+
  #   ggplot2::scale_linetype_manual(name = "Reference", values = 2,
  #                                  guide = ggplot2::guide_legend(override.aes = list(color = "black")))+
  #   ggplot2::annotate(geom = "text", x = unique(multi_ecto_tab_nopres$ttl),
  #                     y = 0.1 * (max(multi_ecto_tab_nopres$perc_change_act) -
  #                                  min(multi_ecto_tab_nopres$perc_change_act)) +
  #                       unlist(lapply(split(multi_ecto_tab_nopres,
  #                                           f = multi_ecto_tab_nopres$LID),
  #                                     function(x) max(x$perc_change_act))),
  #                     label = unique(multi_ecto_tab_nopres$LID))+
  #   ggplot2::labs(title = "% of change in activity (per year) vs. body length")+
  #   ggplot2::theme_bw()
  #
  # # save plot
  # if(save_plot) {
  #   file_name <- "perc_change_act_length.png"
  #   ggplot2::ggsave(filename = file_name, plot = p, device = png(),
  #                   path = save_path, units = unit,
  #                   width = width, height = height, dpi = 500)
  #
  #   message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
  #   # unlink(file_name)
  # } else { print(p) }
  #
  #
  # # weight / length #
  #
  # multi_ecto_tab_nopres$wwttl_ratio <- multi_ecto_tab_nopres$ww / multi_ecto_tab_nopres$ttl
  #
  # # percantage of change in hours active vs. absorptivity (all locations & scenarios)
  # p <- ggplot2::ggplot(data = multi_ecto_tab_nopres)+
  #   ggplot2::geom_point(size = 2,
  #                       mapping = ggplot2::aes_string(x = 'wwttl_ratio',
  #                                                     y = 'perc_change_act',
  #                                                     colour = 'id',
  #                                                     shape = 'id'))+
  #   ggplot2::geom_line(size = 1,
  #                      mapping = ggplot2::aes_string(x = 'wwttl_ratio',
  #                                                    y = 'perc_change_act',
  #                                                    colour = 'id'))+
  #   ggplot2::geom_hline(ggplot2::aes(yintercept = 1, linetype = "present"),
  #                       colour = "black")+
  #   ggplot2::scale_linetype_manual(name = "Reference", values = 2,
  #                                  guide = ggplot2::guide_legend(override.aes = list(color = "black")))+
  #   ggplot2::annotate(geom = "text", x = unique(multi_ecto_tab_nopres$wwttl_ratio),
  #                     y = 0.1 * (max(multi_ecto_tab_nopres$perc_change_act) -
  #                                  min(multi_ecto_tab_nopres$perc_change_act)) +
  #                       unlist(lapply(split(multi_ecto_tab_nopres,
  #                                           f = multi_ecto_tab_nopres$LID),
  #                                     function(x) max(x$perc_change_act))),
  #                     label = unique(multi_ecto_tab_nopres$LID))+
  #   ggplot2::labs(title = "% of change in activity (per year) vs. weight-length ratio")+
  #   ggplot2::theme_bw()
  #
  # # save plot
  # if(save_plot) {
  #   file_name <- "perc_change_act_wwttl-ratio.png"
  #   ggplot2::ggsave(filename = file_name, plot = p, device = png(),
  #                   path = save_path, units = unit,
  #                   width = width, height = height, dpi = 500)
  #
  #   message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
  #   # unlink(file_name)
  # } else { print(p) }


  #   # total hours basking vs. absorptivity (all locations & scenarios)
  # ggplot2::ggplot(data = multi_ecto_tab)+
  #   ggplot2::geom_point(size = 2,
  #                       mapping = ggplot2::aes_string(x = 'absorp',
  #                                                     y = 'h_bask',
  #                                                     colour = 'id',
  #                                                     shape = 'id'))+
  #   ggplot2::geom_line(size = 1,
  #                      mapping = ggplot2::aes_string(x = 'absorp',
  #                                                    y = 'h_bask',
  #                                                    colour = 'id'))+
  #   ggplot2::labs(title = "Hours of basking (per year) vs. absorptivity")+
  #   ggplot2::theme_bw()



  #### activity vs. temperature ####

  shapiro.test(multi_ecto_tab_rcps$T_loc) # p = 0.1023 (-> normal)
  shapiro.test(multi_ecto_tab_rcps$h_active) # p = 0.09553 (-> normal)
  qqnorm(multi_ecto_tab_rcps$T_loc); qqline(multi_ecto_tab_rcps$T_loc)
  qqnorm(multi_ecto_tab_rcps$h_active); qqline(multi_ecto_tab_rcps$h_active)

  library(ggpubr)
  ggscatter(multi_ecto_tab_rcps, x = "T_loc", y = "h_active",
            use = "complete.obs",
            add = "reg.line", conf.int = TRUE,
            cor.coef = TRUE, cor.method = "pearson",
            xlab = "Microclimate temperature [°C]", ylab = "Hours of activity [h]")


  # total vs. total
  p <- ggplot2::ggplot(data = multi_ecto_tab_rcps)+
    ggplot2::geom_point(size = 3,
                        mapping = ggplot2::aes_string(x = 'T_loc',
                                                      y = 'h_active',
                                                      colour = 'timeper',
                                                      shape = 'timeper'))+
      ggplot2::labs(title = "Hours of activity (per year) vs. microclimate temperature")+
      ggplot2::theme_bw()
  # save plot
  if(save_plot) {
    file_name <- "activity_vs_temperature.png"
    ggplot2::ggsave(filename = file_name, plot = p, device = png(),
                    path = save_path, units = unit,
                    width = width, height = height, dpi = 500)

    message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
    # unlink(file_name)
  } else { print(p) }


  # # change vs. change
  # p <- ggplot2::ggplot(data = multi_ecto_tab_rcps)+
  #   ggplot2::geom_point(size = 3,
  #                       mapping = ggplot2::aes_string(x = 'change_T_loc',
  #                                                     y = 'change_act',
  #                                                     colour = 'timeper',
  #                                                     shape = 'timeper'))+
  #   ggplot2::labs(title = "Change of activity vs. change in micro temperature")+
  #   ggplot2::theme_bw()
  # # save plot
  # if(save_plot) {
  #   file_name <- "change_act_vs_change_temp.png"
  #   ggplot2::ggsave(filename = file_name, plot = p, device = png(),
  #                   path = save_path, units = unit,
  #                   width = width, height = height, dpi = 500)
  #
  #   message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
  #   # unlink(file_name)
  # } else { print(p) }
  #
  #
  # # percentage change vs. percentage change
  # p <- ggplot2::ggplot(data = multi_ecto_tab_rcps)+
  #   ggplot2::geom_point(size = 3,
  #                       mapping = ggplot2::aes_string(x = 'perc_T_loc',
  #                                                     y = 'perc_change_act',
  #                                                     colour = 'timeper',
  #                                                     shape = 'timeper'))+
  #   ggplot2::labs(title = "% change of act vs. % change of micro temp")+
  #   ggplot2::theme_bw()
  # # save plot
  # if(save_plot) {
  #   file_name <- "perc_activity_vs_temperature.png"
  #   ggplot2::ggsave(filename = file_name, plot = p, device = png(),
  #                   path = save_path, units = unit,
  #                   width = width, height = height, dpi = 500)
  #
  #   message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
  #   # unlink(file_name)
  # } else { print(p) }

}

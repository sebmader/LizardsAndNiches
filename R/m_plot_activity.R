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


  assertthat::assert_that(is.data.frame(multi_ecto))

  # make dataframe with 'present' being both rcp 4.5 and 8.5 instead of none
  present45 <- multi_ecto[which(stringr::str_detect(multi_ecto$timeper,
                                                        "pres")),]
  present85 <- present45
  present45$rcp <- "4.5"
  present85$rcp <- "8.5"

  multi_ecto_4585pres <- rbind(multi_ecto[which(
    !stringr::str_detect(multi_ecto$timeper,
                         "pres")),],
    present45, present85)

  #### plot the data ####

  # plot size
  unit <- "cm"
  width <- 22
  height <- 13.7

  ### split into locations ###

  #   # act-bask ratio vs. time point; facet grid locations
  # p <- ggplot2::ggplot(data = multi_ecto)+
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
  # p <- ggplot2::ggplot(data = multi_ecto)+
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
  # p <- ggplot2::ggplot(data = multi_ecto)+
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
  p <- ggplot2::ggplot(data = multi_ecto_4585pres)+
    ggplot2::geom_point(size = 2,
                        mapping = ggplot2::aes_(x = quote(timeper),
                                                y = quote(perc_change_act),
                                                colour = quote(ID)))+
    ggplot2::geom_line(size = 1,
                       mapping = ggplot2::aes_(x = quote(timeper),
                                               y = quote(perc_change_act),
                                               colour = quote(ID),
                                               group = quote(ID)))+
    ggplot2::geom_hline(ggplot2::aes(yintercept = 1, linetype = "present"),
                        colour = "black")+
    ggplot2::scale_linetype_manual(name = "Reference", values = 2,
                                   guide = ggplot2::guide_legend(override.aes = list(color = "black")))+
    ggplot2::scale_x_discrete(limits = c("pres", "40-59", "80-99"))+
    ggplot2::scale_color_discrete(guide = "none")+
    ggplot2::scale_shape(guide = "none")+
    ggplot2::facet_grid(rows = ggplot2::vars(rcp), cols = ggplot2::vars(LID))+
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

  multi_ecto_nopres <- multi_ecto[which(multi_ecto$timeper != "pres"),]
  # absorptivity #

  #   # activity-basking hours ratio vs. absorptivity (all locations & scenarios)
  # p <- ggplot2::ggplot(data = multi_ecto)+
  #   ggplot2::geom_point(size = 2,
  #                       mapping = ggplot2::aes_string(x = 'absorp',
  #                                                     y = 'act_bask_ratio',
  #                                                     colour = 'id',
  #                                                     shape = 'id'))+
  #   ggplot2::geom_line(size = 1,
  #                      mapping = ggplot2::aes_string(x = 'absorp',
  #                                                    y = 'act_bask_ratio',
  #                                                    colour = 'id'))+
  #   ggplot2::annotate(geom = "text", x = unique(multi_ecto$absorp),
  #                     y = 0.1 * (max(multi_ecto_nopres$act_bask_ratio) -
  #                                  min(multi_ecto_nopres$act_bask_ratio)) +
  #                       unlist(lapply(split(multi_ecto,
  #                                      f = multi_ecto$LID),
  #                                function(x) max(x$act_bask_ratio))),
  #                     label = unique(multi_ecto$LID))+
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
  # p <- ggplot2::ggplot(data = multi_ecto)+
  #   ggplot2::geom_point(size = 2,
  #                       mapping = ggplot2::aes_string(x = 'absorp',
  #                                                     y = 'h_active',
  #                                                     colour = 'id',
  #                                                     shape = 'id'))+
  #   ggplot2::geom_line(size = 1,
  #                      mapping = ggplot2::aes_string(x = 'absorp',
  #                                                    y = 'h_active',
  #                                                    colour = 'id'))+
  #   ggplot2::annotate(geom = "text", x = unique(multi_ecto$absorp),
  #                     y = 0.1 * (max(multi_ecto_nopres$h_active) -
  #                                  min(multi_ecto_nopres$h_active)) +
  #                       unlist(lapply(split(multi_ecto,
  #                                           f = multi_ecto$LID),
  #                                     function(x) max(x$h_active))),
  #                     label = unique(multi_ecto$LID))+
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
  # p <- ggplot2::ggplot(data = multi_ecto_nopres)+
  #   ggplot2::geom_point(size = 2,
  #                       mapping = ggplot2::aes_string(x = 'absorp',
  #                                                     y = 'change_act',
  #                                                     colour = 'id',
  #                                                     shape = 'id'))+
  #   ggplot2::geom_line(size = 1,
  #                      mapping = ggplot2::aes_string(x = 'absorp',
  #                                                    y = 'change_act',
  #                                                    colour = 'id'))+
  #   ggplot2::annotate(geom = "text", x = unique(multi_ecto_nopres$absorp),
  #                     y = 0.1 * (max(multi_ecto_nopres$change_act) -
  #                                  min(multi_ecto_nopres$change_act)) +
  #                       unlist(lapply(split(multi_ecto_nopres,
  #                                           f = multi_ecto_nopres$LID),
  #                                     function(x) max(x$change_act))),
  #                     label = unique(multi_ecto_nopres$LID))+
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
  p <- ggplot2::ggplot(data = multi_ecto_nopres)+
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
    # ggplot2::annotate(geom = "text", x = unique(multi_ecto_nopres$absorp),
    #                   y = 0.1 * (max(multi_ecto_nopres$perc_change_act) -
    #                                min(multi_ecto_nopres$perc_change_act)) +
    #                     unlist(lapply(split(multi_ecto_nopres,
    #                                         f = multi_ecto_nopres$LID),
    #                                   function(x) max(x$perc_change_act))),
    #                   label = unique(multi_ecto_nopres$LID))+
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
  multi_ecto_85 <- multi_ecto_nopres[which(multi_ecto_nopres$rcp != "4.5"),]

  # percentage of change in hours active vs. absorptivity (all locations & scenarios)
  p <- ggplot2::ggplot(data = multi_ecto_85)+
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
    # ggplot2::annotate(geom = "text", x = unique(multi_ecto_85$absorp),
    #                   y = 0.1 * (max(multi_ecto_85$perc_change_act) -
    #                                min(multi_ecto_85$perc_change_act)) +
    #                     unlist(lapply(split(multi_ecto_85,
    #                                         f = multi_ecto_85$LID),
    #                                   function(x) max(x$perc_change_act))),
    #                   label = unique(multi_ecto_85$LID))+
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
  # p <- ggplot2::ggplot(data = multi_ecto)+
  #   ggplot2::geom_point(size = 2,
  #                       mapping = ggplot2::aes_string(x = 'ww',
  #                                                     y = 'act_bask_ratio',
  #                                                     colour = 'id',
  #                                                     shape = 'id'))+
  #   ggplot2::geom_line(size = 1,
  #                      mapping = ggplot2::aes_string(x = 'ww',
  #                                                    y = 'act_bask_ratio',
  #                                                    colour = 'id'))+
  #   ggplot2::annotate(geom = "text", x = unique(multi_ecto$ww),
  #                     y = 0.1 * (max(multi_ecto$act_bask_ratio) -
  #                                  min(multi_ecto$act_bask_ratio)) +
  #                       unlist(lapply(split(multi_ecto,
  #                                           f = multi_ecto$LID),
  #                                     function(x) max(x$act_bask_ratio))),
  #                     label = unique(multi_ecto$LID))+
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
  # p <- ggplot2::ggplot(data = multi_ecto_nopres)+
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
  #   ggplot2::annotate(geom = "text", x = unique(multi_ecto_nopres$ww),
  #                     y = 0.1 * (max(multi_ecto_nopres$perc_change_act) -
  #                                  min(multi_ecto_nopres$perc_change_act)) +
  #                       unlist(lapply(split(multi_ecto_nopres,
  #                                           f = multi_ecto_nopres$LID),
  #                                     function(x) max(x$perc_change_act))),
  #                     label = unique(multi_ecto_nopres$LID))+
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
  p <- ggplot2::ggplot(data = multi_ecto_85)+
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
    # ggplot2::annotate(geom = "text", x = unique(multi_ecto_85$ww),
    #                   y = 0.1 * (max(multi_ecto_85$perc_change_act) -
    #                                min(multi_ecto_85$perc_change_act)) +
    #                     unlist(lapply(split(multi_ecto_85,
    #                                         f = multi_ecto_85$LID),
    #                                   function(x) max(x$perc_change_act))),
    #                   label = unique(multi_ecto_85$LID))+
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
  # p <- ggplot2::ggplot(data = multi_ecto)+
  #   ggplot2::geom_point(size = 2,
  #                       mapping = ggplot2::aes_string(x = 'ttl',
  #                                                     y = 'act_bask_ratio',
  #                                                     colour = 'id',
  #                                                     shape = 'id'))+
  #   ggplot2::geom_line(size = 1,
  #                      mapping = ggplot2::aes_string(x = 'ttl',
  #                                                    y = 'act_bask_ratio',
  #                                                    colour = 'id'))+
  #   ggplot2::annotate(geom = "text", x = unique(multi_ecto$ttl),
  #                     y = 0.1 * (max(multi_ecto$act_bask_ratio) -
  #                                  min(multi_ecto$act_bask_ratio)) +
  #                       unlist(lapply(split(multi_ecto,
  #                                           f = multi_ecto$LID),
  #                                     function(x) max(x$act_bask_ratio))),
  #                     label = unique(multi_ecto$LID))+
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
  # p <- ggplot2::ggplot(data = multi_ecto_nopres)+
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
  #   ggplot2::annotate(geom = "text", x = unique(multi_ecto_nopres$ttl),
  #                     y = 0.1 * (max(multi_ecto_nopres$perc_change_act) -
  #                                  min(multi_ecto_nopres$perc_change_act)) +
  #                       unlist(lapply(split(multi_ecto_nopres,
  #                                           f = multi_ecto_nopres$LID),
  #                                     function(x) max(x$perc_change_act))),
  #                     label = unique(multi_ecto_nopres$LID))+
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
  # multi_ecto_nopres$wwttl_ratio <- multi_ecto_nopres$ww / multi_ecto_nopres$ttl
  #
  # # percantage of change in hours active vs. absorptivity (all locations & scenarios)
  # p <- ggplot2::ggplot(data = multi_ecto_nopres)+
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
  #   ggplot2::annotate(geom = "text", x = unique(multi_ecto_nopres$wwttl_ratio),
  #                     y = 0.1 * (max(multi_ecto_nopres$perc_change_act) -
  #                                  min(multi_ecto_nopres$perc_change_act)) +
  #                       unlist(lapply(split(multi_ecto_nopres,
  #                                           f = multi_ecto_nopres$LID),
  #                                     function(x) max(x$perc_change_act))),
  #                     label = unique(multi_ecto_nopres$LID))+
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
  # ggplot2::ggplot(data = multi_ecto)+
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

  # library(ggpubr)
  # p <- ggscatter(multi_ecto, x = "T_loc", y = "h_active",
  #           use = "complete.obs",
  #           add = "reg.line", conf.int = TRUE,
  #           cor.coef = TRUE, cor.method = "pearson",
  #           xlab = "Microclimate temperature [°C]", ylab = "Hours of activity [h]")
  #
  # # save plot
  # if(save_plot) {
  #   file_name <- "relationship_h_act_vs_temp_ind.png"
  #   ggplot2::ggsave(filename = file_name, plot = p, device = png(),
  #                   path = save_path, units = unit,
  #                   width = width, height = height, dpi = 500)
  #
  #   message(paste0("Plot ", file_name, " has been saved in ", save_path, "\n"))
  #   # unlink(file_name)
  # } else { print(p) }


  # total vs. total
  p <- ggplot2::ggplot(data = multi_ecto)+
    ggplot2::geom_smooth(ggplot2::aes_(x = quote(T_loc), y = quote(h_active)),
                         color = "grey",
                         method = lm)+
    ggplot2::geom_point(size = 3,
                        mapping = ggplot2::aes_(x = quote(T_loc),
                                                y = quote(h_active),
                                                colour = quote(timeper),
                                                shape = quote(timeper)))+
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
  # p <- ggplot2::ggplot(data = multi_ecto)+
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
  # p <- ggplot2::ggplot(data = multi_ecto)+
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

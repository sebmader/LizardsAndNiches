#' @title Plot activity times over the organisms reflectance
#' @description This function plots the output of the biophysical model of ectotherms
#' from the NicheMapR package.
#' @name m_plot_activity
#' @param multi_ecto A list of different climate scenarios which are lists
#' of locations which are lists with the structure of the output of the
#' ectotherm function (see vignette of ectotherm in NicheMapR package).
#' @param save_plot Boolean whether the plot should be saved or not (default = FALSE).
#' @return Plot
# @importFrom graphics abline legend text
# @importFrom grDevices png dev.off
#' @export

m_plot_activity <- function(multi_ecto, save_plot = FALSE) {

  # make tidyverse data frame with activity times summed over the whole year
  # and absorptivity per location per climate scenario and time period

    # vector of scenario names
  scenarios <- names(multi_ecto)

    # vector of location names
  locations <- names(multi_ecto[[1]])

  # loop through multi_ecto and save total activity time per year
  for(scen in scenarios) {
    for(loc in locations) {
      ndays <- length(unique(multi_ecto[[scen]][[loc]]$environ[,1]))
      nyears <- length(unique(multi_ecto[[scen]][[loc]]$environ[,2]))

      # count hours with activity == 2 for activity times
      h_active <- length(which(multi_ecto[[scen]][[loc]]$environ[,9] == 2))
      perc_active <- h_active/(nyears*ndays*24)

      # count hours with activity == 1 for basking times
      h_bask <- length(which(multi_ecto[[scen]][[loc]]$environ[,9] == 1))
      perc_bask <- h_bask/(nyears*ndays*24)

      # save in multi_ecto
      multi_ecto[[scen]][[loc]]$h_active <- h_active
      multi_ecto[[scen]][[loc]]$perc_active <- perc_active
      multi_ecto[[scen]][[loc]]$h_bask <- h_bask
      multi_ecto[[scen]][[loc]]$perc_bask <- perc_bask
      multi_ecto[[scen]][[loc]]$ndays <- ndays
      multi_ecto[[scen]][[loc]]$nyears <- nyears
    }
  }

  # no need for the environ table anymore (and it makes unlisting very tricky)
  # multi_ecto2 <- multi_ecto
  for(scen in scenarios) {
    for(loc in locations) {
      multi_ecto[[scen]][[loc]]$environ <- NULL
    }
  }

  # unlist multi_ecto into a dataframe
  multi_ecto_tab <- data.table::rbindlist(lapply(multi_ecto,
                                             function(x) data.table::rbindlist(x)),
                                      idcol = "id")
  # calculate the activity-basking ratio and add it to dataframe
  act_bask_ratio <- multi_ecto_tab$h_active/multi_ecto_tab$h_bask
  multi_ecto_tab <- cbind(multi_ecto_tab, act_bask_ratio)



  #### plot the data ####

    # make dataframe with 'present' being both rcp 4.5 and 8.5 instead of none
  present45 <- multi_ecto_tab[which(multi_ecto_tab$timeper == "present")]
  present85 <- present45
  present45$rcp <- "45"
  present85$rcp <- "85"

  multi_ecto_tab_rcps <- rbind(multi_ecto_tab[which(multi_ecto_tab$timeper != "present")],
                               present45, present85)
  multi_ecto_tab_rcps$id <- as.factor(multi_ecto_tab_rcps$id)
  multi_ecto_tab_rcps$timeper <- as.factor(multi_ecto_tab_rcps$timeper)
  multi_ecto_tab_rcps$rcp <- as.factor(multi_ecto_tab_rcps$rcp)

    # act-bask ratio vs. time point; facet grid locations
  ggplot2::ggplot(data = multi_ecto_tab_rcps)+
    ggplot2::geom_point(size = 2,
                        mapping = ggplot2::aes_string(x = 'timeper',
                                                      y = 'act_bask_ratio',
                                                      colour = 'rcp',
                                                      shape = 'rcp'))+
    ggplot2::geom_line(size = 1,
                       mapping = ggplot2::aes_string(x = 'timeper',
                                                     y = 'act_bask_ratio',
                                                     colour = 'rcp',
                                                     group = 'rcp'))+
    ggplot2::geom_hline(ggplot2::aes(yintercept = 1), linetype = "dashed",
                         colour = "black")+
    ggplot2::scale_x_discrete(limits = c("present", "2040_2059", "2080_2099"))+
    ggplot2::facet_wrap(~LID, scales = "free")+
    ggplot2::theme_bw()

    # save plot
  ggplot2::ggsave("act-bask_ratio_scenario.png",)


    # total active hours vs. time point; facet grid locations
  ggplot2::ggplot(data = multi_ecto_tab_rcps)+
    ggplot2::geom_point(size = 2,
                        mapping = ggplot2::aes_string(x = 'timeper',
                                                      y = 'h_active',
                                                      colour = 'rcp',
                                                      shape = 'rcp'))+
    ggplot2::geom_line(size = 1,
                       mapping = ggplot2::aes_string(x = 'timeper',
                                                     y = 'h_active',
                                                     colour = 'rcp',
                                                     group = 'rcp'))+
    ggplot2::scale_x_discrete(limits = c("present", "2040_2059", "2080_2099"))+
    ggplot2::facet_wrap(~LID, scales = "free")+
    ggplot2::theme_bw()




  #   # total hours active vs. absorptivity (all locations & scenarios)
  # ggplot2::ggplot(data = multi_ecto_tab)+
  #   ggplot2::geom_point(size = 2,
  #                       mapping = ggplot2::aes_string(x = 'absorp',
  #                                                     y = 'h_active',
  #                                                     colour = 'id',
  #                                                     shape = 'id'))+
  #   ggplot2::geom_line(size = 1,
  #                      mapping = ggplot2::aes_string(x = 'absorp',
  #                                                    y = 'h_active',
  #                                                    colour = 'id'))+
  #   ggplot2::labs(title = "Hours of activity (per year) vs. absorptivity")+
  #   ggplot2::theme_bw()
  #
  #
  #
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
  #
  #   # activity-basking hours ratio vs. absorptivity (all locations & scenarios)
  # ggplot2::ggplot(data = multi_ecto_tab)+
  #   ggplot2::geom_point(size = 2,
  #                       mapping = ggplot2::aes_string(x = 'absorp',
  #                                                     y = 'act_bask_ratio',
  #                                                     colour = 'id',
  #                                                     shape = 'id'))+
  #   ggplot2::geom_line(size = 1,
  #                      mapping = ggplot2::aes_string(x = 'absorp',
  #                                                    y = 'act_bask_ratio',
  #                                                    colour = 'id'))+
  #   ggplot2::labs(title = "activity-basking ratio (per year) vs. absorptivity")+
  #   ggplot2::theme_bw()


}
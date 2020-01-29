#' @title Plot microclimate conditions
#' @description This function plots the microclimate conditions from the output of
#' multiple ectotherm models of different conditions (locations, times, emission
#' scenarios) together in one plot
#' @name m_plot_micro
#' @param multi_ecto A list of lists with the structure of the output of the micro_global()
#' function (see vignette of microclimate in NicheMapR package).
#' @param save_plot Boolean whether the microclimate plot should be saved or not
#' (default = FALSE)
#' @return Plot
#' @importFrom ggplot2 ggplot aes geom_line facet_grid vars scale_x_discrete labs
#' @importFrom grDevices png dev.off
#' @export


m_plot_micro <- function(multi_ecto, save_plot = FALSE) {

  # calculate average microclimate conditions (over all locations per month)
  T_loc <- vector(mode = "numeric", length = 12)
  T_ref <- vector(mode = "numeric", length = 12)
  RH_loc <- vector(mode = "numeric", length = 12)
  RH_ref <- vector(mode = "numeric", length = 12)
  days <- seq(1,12,1)

  # save sim_name for plot title and saving directory
  sim_name <- multi_ecto[[1]]$timeper
  if(multi_ecto[[1]]$timeper != "present") {
    sim_name <- gsub(pattern = "_", replacement = "-", x = sim_name)
    rcp_name <- ifelse(multi_ecto[[1]]$rcp == "45", yes = "4.5", no = "8.5")
    if(rcp_name == "8.5" & multi_ecto[[1]]$rcp != "85") {
      cat("something's fishy...\n")
    }
    sim_name <- paste0(sim_name, ", RCP", rcp_name)
  }

  # sum the monthly averages of all locations
  for(ecto in multi_ecto) {
    list_days <- split.data.frame(x = ecto$metout[,3:6], f = ecto$metout[,1])
    avg_days <- lapply(X = list_days,
                       FUN = function(x) c(mean(x[,1]),
                                           mean(x[,2]),
                                           mean(x[,3]),
                                           mean(x[,4]))
    )
    T_loc <- T_loc + unsplit(value = lapply(X = avg_days,
                                            FUN = function(x) x[1]),
                             f = days)
    T_ref <- T_ref + unsplit(value = lapply(X = avg_days,
                                            FUN = function(x) x[2]),
                             f = days)
    RH_loc <- RH_loc + unsplit(value = lapply(X = avg_days,
                                              FUN = function(x) x[3]),
                               f = days)
    RH_ref <- RH_ref + unsplit(value = lapply(X = avg_days,
                                              FUN = function(x) x[4]),
                               f = days)
  }

  # divide by number of locations and make dataframe from climate variables
  n_loc <- length(multi_ecto)
  # rm(multi_ecto)
  T_loc <- T_loc/n_loc
  Temp <- data.frame(T_loc, rep("local", length(T_loc)))
  T_ref <- T_ref/n_loc
  Temp_ref <- data.frame(T_ref, rep("reference", length(T_ref)))
  colnames(Temp_ref) <- colnames(Temp)
  Temp <- rbind(Temp, Temp_ref)
  Temp <- cbind(Temp, rep("Temperature (°C)", length(Temp[,1])))
  Temp <- cbind(rep(days, length(Temp[,1])/length(days)), Temp)
  colnames(Temp) <- c("Month", "Value", "Height", "Climate variable")

  RH_loc <- RH_loc/n_loc
  RH_df <- data.frame(RH_loc, rep("local", length(RH_loc)))
  RH_ref <- RH_ref/n_loc
  RH_df_ref <- data.frame(RH_ref, rep("reference", length(RH_ref)))
  colnames(RH_df_ref) <- colnames(RH_df)
  RH_df <- rbind(RH_df, RH_df_ref)
  RH_df <- cbind(RH_df, rep("Rel. humidity (%)", length(RH_df[,1])))
  RH_df <- cbind(rep(days, length(RH_df[,1])/length(days)), RH_df)
  colnames(RH_df) <- c("Month", "Value", "Height", "Climate variable")

  clim_data <- data.frame(rbind(Temp, RH_df))

  # directory to save plots
  save_path <- paste0("Plots/", "microclim/")
  # save plot if applicable
  if(save_plot) {

    if(!dir.exists(save_path)) {
      dir.create(save_path, recursive = T)
      cat(paste0("Created folder ", save_path, "\n"))
    }

    # make the plot and save
    grDevices::png(filename = paste0(save_path, "/", sim_name, ".png"),
                   type = "cairo", units = "in",
                   width = 6, height = 6, res = 300)
  }

  plot_title <- paste0("Microclimate at ", sim_name)
  # plot the data
  ggplot2::ggplot(data = clim_data,
                  mapping = ggplot2::aes_string(x = 'Month',
                                         y = 'Value',
                                         colour = 'Height'),
                  environment = environment())+
    ggplot2::geom_line(size = 1)+
    ggplot2::facet_grid(rows = ggplot2::vars(clim_data$Climate.variable),
                        scales = "free")+
    ggplot2::scale_x_discrete(limits = as.character(days))+
    ggplot2::labs(title = plot_title, y = NULL)

  # save plot if applicable
  if(save_plot) {
    cat(paste0("\nplotted and saved micro climate in ", save_path, "\n"))
    grDevices::dev.off()
  }
}

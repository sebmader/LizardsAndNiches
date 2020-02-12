#' @title Plot results of ectotherm function
#' @description This function plots the output of the biophysical model of ectotherms
#' from the NicheMapR package.
#' @name m_plot_ecto
#' @param ecto A list with the structure of the output of the ectotherm function (see
#' vignette of ectotherm in NicheMapR package).
#' @param sim_name A character string naming the simulation output.
#' @param save_plot Boolean whether the plot should be saved or not (default = FALSE).
#' @return Plot
# @importFrom graphics abline legend text
# @importFrom grDevices png dev.off
#' @export

m_plot_ecto <- function(ecto, sim_name = ecto$LID, save_plot = F) {

  # directory to save plots
  save_path <- paste0("./Plots/ecophysio_plots/", sim_name)

  # retrieve output
  environ <- as.data.frame(ecto$environ) # behaviour, Tb and environment
  enbal <- as.data.frame(ecto$enbal) # heat balance outputs
  masbal <- as.data.frame(ecto$masbal) # mass balance outputs
  metout <- as.data.frame(ecto$metout) # above ground microclimate
  environ <- cbind(environ,metout$SOLR) # add solar radiation for activity window plots
  colnames(environ)[ncol(environ)] <- "Solar"
  nyears <- ecto$nyears

  # append dates
  ndays <- ecto$ndays
  days <- rep(seq(1, ndays),24)
  days <- days[order(days)]
  dates <- days + metout$TIME / 60 / 24 - 1 # dates for hourly output
  dates2 <- seq(1, 12, 1) # dates for daily output
  metout <- cbind(dates, metout)
  environ <- cbind(dates, environ)
  masbal <- cbind(dates, masbal)
  enbal <- cbind(dates, enbal)

  T_F_max <- ecto$T_F_max
  T_F_min <- ecto$T_F_min
  CT_max <- ecto$CT_max
  CT_min <- ecto$CT_min
  # TODO: pipe preferred body temperature
  T_pref <- ecto$T_pref

  # substitute the underscore in 'timeper' with a dash
  # time <- ecto$timeper
  if(!stringr::str_detect(ecto$timeper, "present")) {
    ecto$timeper <- gsub(pattern = "_", replacement = "-",
                                     x = ecto$timeper)
  }

  # make the rcp to a decimal (correct value of radiative forcing)
  sim_title <- paste0(sim_name, ", ", ecto$timeper)
  if(ecto$rcp != "none") {
    rcp_name <- ifelse(ecto$rcp == "45", yes = "4.5", no = "8.5")
    if(rcp_name == "8.5" & ecto$rcp != "85") {
      stop("something's fishy...\n")
    }
    sim_title <- paste0(sim_title, ", RCP", rcp_name)
  }

  # make the subtitle more flexible and applicable for the situation (sigular or plural)
  subtitle <- paste0(ecto$ndays/12)
  if(ecto$ndays/12 > 1) {
    subtitle <- paste0(subtitle, " days per month, ")
  } else {
    assertthat::are_equal(ecto$ndays/12, 1)
    subtitle <- paste0(subtitle, " day per month, ")
  }
  if(ecto$nyears > 1) {
    subtitle <- paste0(subtitle, ecto$nyears, " years")
  } else {
    assertthat::are_equal(ecto$nyears, 1)
    subtitle <- paste0(subtitle, ecto$nyears, " year")
  }


  # save plot if applicable
  if(save_plot) {

    if(!dir.exists(save_path)) {
      dir.create(save_path, recursive = T)
      message(paste0("Created folder ", save_path, "\n"))
    }

    # make the plot and save
    grDevices::png(filename = paste0(save_path, "/", sim_title, ".png"),
                   type = "cairo", units = "in",
                   width = 6, height = 6, res = 300)
  }

  ylim_min <- -8
  depth_div <- 10
  xlab <- ifelse(ecto$ndays == 12, "months", "days")
  if(ecto$burrow) {
      with(environ, graphics::plot(TC ~ dates, ylab = "T_b, activity, shade & depth",
                               xlab = xlab, ylim = c(ylim_min, 50), type = "l",
                               main = sim_title, sub = subtitle))
  } else {
    with(environ, graphics::plot(TC ~ dates, ylab = "T_b, activity & shade",
                     xlab = xlab, ylim = c(ylim_min, 50), type = "l",
                     main = sim_title, sub = subtitle))
  }
  with(environ, graphics::points(ACT * 5 ~ dates, type = "l", col = "orange"))
  with(environ, graphics::points(SHADE / 10 ~ dates, type = "h", col = "dark green"))
  if(ecto$burrow) {
      with(environ, points(DEP / depth_div ~ dates, type = "l",col = "brown"))
  }
  graphics::abline(T_F_max, 0, lty = 5, lwd = 1.6, col = "orange")
  graphics::abline(T_F_min, 0, lty = 5, lwd = 1.6, col = "lightblue3")
  graphics::abline(T_pref, 0, lty = 5, lwd = 1.6, col = "green")
  graphics::abline(h = CT_max, lty = 5, lwd = 1.6, col = "red")
  graphics::abline(h = CT_min, lty = 5, lwd = 1.6, col = "mediumblue")
  graphics::abline(v = dates[which(environ$TIME == 12)], lty = 3, lwd = 0.7, col = "yellow3")
  graphics::abline(v = dates[which(environ$TIME == 0)], lty = 3, lwd = 0.7, col = "violet")
  graphics::text(x = 0, y = T_F_max + 2, "T_F_max", col = "orange", adj = c(0,0.5))
  graphics::text(x = 0, y = T_F_min + 2, "T_F_min", col = "lightblue3", adj = c(0,0.5))
  graphics::text(x = 0, y = T_pref + 2, "T_pref", col = "green", adj = c(0,0.5))
  graphics::text(x = 0, y = CT_max + 2, "CT_max", col = "red", adj = c(0,0.5))
  graphics::text(x = 0, y = CT_min + 2, "CT_min", col = "mediumblue", adj = c(0,0.5))
  if(ecto$burrow) {
      graphics::legend(x = "topright",
                   legend = c("T_b (°C)", "activity (0, 5 or 10)", "shade (%/10)",
                              paste0("depth (cm/", depth_div, ")")),
                   col = c("black", "orange", "dark green", "brown"), lty = rep(1, 3), bty = "n")
  } else {
      graphics::legend(x = "topright",
         legend = c("T_b (°C)", "activity (0, 5 or 10)", "shade (%/10)"),
         col = c("black", "orange", "dark green"), lty = rep(1, 3), bty = "n")
  }

  # save plot if applicable
  if(save_plot) {
    message(paste0("\nplotted and saved results in ", save_path, "\n"))
    grDevices::dev.off()
  }
}

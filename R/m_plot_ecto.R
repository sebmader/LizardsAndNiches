#' @title Plot results of ectotherm function
#' @description This function plots the output of the biophysical model of ectotherms
#' from the NicheMapR package.
#' @name m_plot_ecto
#' @param ecto A list with the structure of the output of the ectotherm function (see
#' vignette of ectotherm in NicheMapR package).
#' @param sim_name A character string naming the simulation output.
#' @param sub_title A character string to be shown as subtitle of the plot.
#' This is to display some parameter values, like days and years the model is run for.
#' @return Plot
#' @importFrom graphics abline legend text
#' @importFrom grDevices png
#' @export

m_plot_ecto <- function(ecto, sim_name, sub_title) {
  # retrieve output
  environ <- as.data.frame(ecto$environ) # behaviour, Tb and environment
  enbal <- as.data.frame(ecto$enbal) # heat balance outputs
  masbal <- as.data.frame(ecto$masbal) # mass balance outputs
  metout <- as.data.frame(ecto$metout) # above ground microclimate
  environ <- cbind(environ,metout$SOLR) # add solar radiation for activity window plots
  colnames(environ)[ncol(environ)] <- "Solar"
  nyears <- ecto$nyears

  # append dates
  ndays <- length(ecto$rainfall)
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
  # TODO: pipe preferred body temperature
  T_pref <- 33.6

  # make the plot and save
  grDevices::png(filename = paste0("Plots/", sim_name, ".png"),
                 type = "cairo", units = "in",
                 width = 6, height = 6, res = 300)
  with(environ, graphics::plot(TC ~ dates, ylab = "T_b, activity and shade",
                     xlab = "month of year", ylim = c(0, 50), type = "l",
                     main = sim_name, sub = sub_title))
  with(environ, graphics::points(ACT * 5 ~ dates, type = "l", col = "orange"))
  with(environ, graphics::points(SHADE / 10 ~ dates, type = "h", col = "dark green"))
  # with(environ, points(DEP / 10 ~ dates, type = "l",col = "brown"))
  graphics::abline(T_F_max, 0, lty = 2,col = 'red')
  graphics::abline(T_F_min, 0, lty = 2,col = 'blue')
  graphics::abline(T_pref, 0, lty = 2,col = 'orange')
  graphics::text(x = 6, y = T_F_max + 2, "T_F_max", col = 'red', adj = c(0,0.5))
  graphics::text(x = 6, y = T_F_min + 2, "T_F_min", col = 'blue', adj = c(0,0.5))
  graphics::text(x = 6, y = T_pref + 2, "T_pref", col = 'orange', adj = c(0,0.5))
  graphics::legend(x = "topright",
         legend = c("T_b (°C)", "activity (0, 5 or 10)", "shade (%/10)"),
         col = c("black", "orange", "dark green"), lty = rep(1, 3), bty = "n")
  grDevices::dev.off()
}
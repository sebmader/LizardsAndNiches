#' @title m_run_ectotherm
#' @description This function runs the ectotherm function of NicheMapR given the input
#' parameters with or without DEB model.
#' @name m_run_ectotherm
#' @param param List of input parameters for ectotherm function.
#' @param micro List of microclimate data as the output of micro_global function.
#' @param burrow Boolean whether lizard is allowed to seek shelter in burrow.
#' @param DEB Boolean whether Dynamic Energy Budget (DEB) model should be included.
#' @export

m_run_ectotherm <- function(param,
                            micro,
                            burrow = FALSE,
                            DEB = FALSE) {
  assertthat::assert_that(is.data.frame(param))
  assertthat::assert_that(is.list(micro))
  assertthat::assert_that(is.logical(DEB))
  assertthat::assert_that(is.logical(burrow))
  # require(NicheMapR)

  loc_name <- param$LID
  ww <- param$WW_mean
  absorp <- param$absorp_mean
  temp_f_min <- param$tf_min
  temp_f_max <- param$tf_max
  ct_min <- param$ct_min
  ct_max <- param$ct_max
  temp_pref <- param$t_pref
  temp_bask <- param$t_bask

  # micro <- micro # I don't get why I need to do this but otherwise it cannot find
                 # the variable 'micro'

  # as in example on github:
  # TODO: I don't use them as input for ectotherm()... even need it ??? -> nope
  # retrieve output
  # metout <- as.data.frame(micro$metout) # above ground microclimatic conditions, min shade
  # shadmet <- as.data.frame(micro$shadmet) # above ground microclimatic conditions, max shade
  # soil <- as.data.frame(micro$soil) # soil temperatures, minimum shade
  # shadsoil <- as.data.frame(micro$shadsoil) # soil temperatures, maximum shade
  #
  # # append dates
  # dates <- micro$dates
  # metout <- cbind(dates, metout)
  # soil <- cbind(dates, soil)
  # shadmet <- cbind(dates, shadmet)
  # shadsoil <- cbind(dates, shadsoil)

  # some fixed parameter values
  minshade <- 0
  maxdepth <- 9
  mindepth <- 2  # because soil node 1 is the surface
  # burrow <- as.numeric(burrow)

  # m_estimate_deb(param)

  # run ectotherm function without DEB model
  ecto <- NicheMapR::ectotherm(Ww_g = ww, shape = 3, alpha_max = absorp, alpha_min = absorp,
                               T_F_min = temp_f_min, T_F_max = temp_f_max, T_B_min = temp_bask,
                               T_RB_min = temp_bask, T_pref = temp_pref, CT_min = ct_min,
                               CT_max = ct_max, burrow = as.numeric(burrow), shdburrow = 2,
                               DEB = DEB, maxdepth = maxdepth, mindepth = mindepth,
                               nyears = micro$nyears,
                               minshade = minshade,
                               minshades = rep(minshade, length(micro$MAXSHADES)),
                               maxshades = micro$MAXSHADES,
                               alpha_sub = (1 - micro$REFL),
                               DEP = micro$DEP,
                               metout = micro$metout,
                               shadmet = micro$shadmet,
                               soil = micro$soil,
                               shadsoil = micro$shadsoil,
                               soilmoist = micro$soilmoist,
                               shadmoist = micro$shadmoist,
                               humid = micro$humid,
                               shadhumid = micro$shadhumid,
                               soilpot = micro$soilpot,
                               shadpot = micro$shadpot,
                               rainfall = micro$RAINFALL,
                               rainhr = rep(-1,nrow(micro$metout)),
                               elev = as.numeric(micro$elev),
                               longitude = as.numeric(micro$longlat[1]),
                               latitude = as.numeric(micro$longlat[2])
  )
  ecto$LID <- loc_name
  ecto$burrow <- burrow
  ecto$timeper <- micro$timeper
  ecto$rcp <- micro$rcp
  ecto
}

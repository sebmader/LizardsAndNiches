#' @title m_run_ectotherm
#' @description This function runs the ectotherm function of NicheMapR given the input
#' parameters with or without DEB model.
#' @name m_run_ectotherm
#' @param param List of input parameters for ectotherm function.
#' @param micro List of microclimate data as the output of micro_global function.
#' @param burrow Boolean whether lizard is allowed to seek shelter in burrow.
#' @param burtype Numeric to specify type burrow: 0 (always in the sun),
#' 1 (organism decides if burrow is in the sun or in the shade),
#' 2 (always in the shade)
#' @param burdepth The burrows depth in "soil nodes", a numeric ranging from 2 to 10.
#' 2 equals to 2.5 cm, 3 to 5 cm, 4 to 10 cm, 5 to 15 cm, 6 to 20 cm, 7 to 30 cm, 8 to 50 cm,
#' 9 to 100 cm and 10 to 200 cm. These are also the steps the lizard takes while burrowing.
#' @param DEB Boolean whether Dynamic Energy Budget (DEB) model shall be included.
#' @export

m_run_ectotherm <- function(param,
                            micro,
                            burrow = FALSE,
                            burtype = 0,
                            burdepth = 9,
                            DEB = FALSE) {
  assertthat::assert_that(is.data.frame(param))
  assertthat::assert_that(is.list(micro))
  assertthat::assert_that(is.logical(DEB))
  assertthat::assert_that(is.logical(burrow))
  assertthat::assert_that(is.numeric(burtype))
  assertthat::assert_that(is.numeric(burdepth))
  # require(NicheMapR)

  loc_name <- param$LID
  temp_f_min <- param$tf_min
  temp_f_max <- param$tf_max
  ct_min <- param$ct_min
  ct_max <- param$ct_max
  temp_pref <- param$t_pref
  temp_bask <- param$t_bask

  ttl <- param$ttl
  ww <- param$ww
  absorp <- param$absorp


  # some fixed parameter values
  minshade <- 0
  # maxdepth <- 9  # because the last jump from -100 to -200 cm causes problems
  mindepth <- 2  # because soil node 1 is the surface

  # m_estimate_deb(param)

  # run ectotherm function without DEB model
  ecto <- NicheMapR::ectotherm(Ww_g = ww, shape = 3, alpha_max = absorp, alpha_min = absorp,
                               T_F_min = temp_f_min, T_F_max = temp_f_max, T_B_min = temp_bask,
                               T_RB_min = temp_bask, T_pref = temp_pref, CT_min = ct_min,
                               CT_max = ct_max, burrow = as.numeric(burrow), shdburrow = burtype,
                               DEB = DEB, maxdepth = burdepth, mindepth = mindepth,
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
                               latitude = as.numeric(micro$longlat[2]),
                               CT_kill = 0,    # TODO: maybe allow it to die ??!!
                               CT_minthresh = 12    # and give a threshold time period it can endure CT_min (1?)
  )
  # ecto$LID <- droplevels(loc_name)
  ecto$LID <- micro$LID
  # ecto$coor <- micro$coor
  ecto$Latitude <- micro$Latitude
  ecto$Longitude <- micro$Longitude
  ecto$burrow <- burrow
  ecto$T_pref <- temp_pref
  ecto$timeper <- micro$timeper
  ecto$rcp <- micro$rcp
  ecto$nyears <- micro$nyears
  ecto$ndays <- micro$ndays
  ecto$absorp <- absorp
  ecto$ttl <- ttl
  ecto$ww <- ww
  ecto
}

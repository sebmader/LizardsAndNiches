#' @title m_run_ectotherm
#' @description This function runs the ectotherm function of NicheMapR given the input
#' parameters with or without DEB model.
#' @name m_run_ectotherm
#' @param param List of input parameters for ectotherm function.
#' @param micro List of microclimate data as the output of micro_global function.
#' @param DEB Boolean wheather Dynamic Energy Budget (DEB) model should be included.
#' @export

library(NicheMapR)

m_run_ectotherm <- function(param, micro, DEB = FALSE) {
  assertthat::assert_that(is.list(param))
  assertthat::assert_that(is.list(micro))
  assertthat::assert_that(is.logical(DEB))

  ww <- param$mean_W
  absorp <- param$mean_absorp
  temp_f_min <- param$min_forage
  temp_f_max <- param$max_forage
  ct_min <- param$min_CT
  ct_max <- param$max_CT
  temp_pref <- param$pref_T
  temp_bask <- param$bask_temp

  micro <- micro # I don't get why I need to do this but otherwise it cannot find
                 # the variable 'micro'

    if(DEB) {
    # m_estimate_deb(param)

    # ectotherm(..., DEB = 1)
  } else {
    # run ectotherm function without DEB model
    ecto <- ectotherm(Ww_g = ww, shape = 3, alpha_max = absorp, alpha_min = absorp,
                         T_F_min = temp_f_min, T_F_max = temp_f_max, T_B_min = temp_bask,
                         T_RB_min = temp_bask, T_pref = temp_pref, CT_min = ct_min,
                         CT_max = ct_max, burrow = 0, DEB = DEB)
  }
  ecto
}

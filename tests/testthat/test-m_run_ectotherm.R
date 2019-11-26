context("Testing functionality of running ectotherm model")

{
  loc_data <- read.csv("example_coordinates.csv")
  loc_row <- m_extract_microclim_input("DAR", loc_data = loc_data)
  micro <- m_get_microclim(loc_row = loc_row)
  params <- read.csv("example_physio.csv")
  ecto <- m_run_ectotherm(param = params[1,], micro = micro)
}

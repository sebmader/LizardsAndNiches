% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/m_run_ectotherm.R
\name{m_run_ectotherm}
\alias{m_run_ectotherm}
\title{m_run_ectotherm}
\usage{
m_run_ectotherm(
  param,
  micro,
  burrow = FALSE,
  burtype = 0,
  burdepth = 9,
  DEB = FALSE
)
}
\arguments{
\item{param}{List of input parameters for ectotherm function.}

\item{micro}{List of microclimate data as the output of micro_global function.}

\item{burrow}{Boolean whether lizard is allowed to seek shelter in burrow.}

\item{burtype}{Numeric to specify type burrow: 0 (always in the sun),
1 (organism decides if burrow is in the sun or in the shade),
2 (always in the shade)}

\item{burdepth}{The burrows depth in "soil nodes", a numeric ranging from 2 to 10.
2 equals to 2.5 cm, 3 to 5 cm, 4 to 10 cm, 5 to 15 cm, 6 to 20 cm, 7 to 30 cm, 8 to 50 cm,
9 to 100 cm and 10 to 200 cm. These are also the steps the lizard takes while burrowing.}

\item{DEB}{Boolean whether Dynamic Energy Budget (DEB) model shall be included.}
}
\description{
This function runs the ectotherm function of NicheMapR given the input
parameters with or without DEB model.
}

#' England and Wales human population for year 2014.
#'
#' @format A list containing:
#' \describe{
#'   \item{age_pyramid}{Data frame containing the aggregated human population of England and Wales for year 2014, by age and sex.}
#'   \item{rates}{Data frame containing birth rates, death rates for males and death rates for females.}
#'   \item{sample}{Data frame containing a population of size $10^5$ sampled from `age_pyramid`.}
#' }
"EW_pop_14"


#' England and Wales human population with IMD for year 2014.
#'
#' @format A list containing:
#' \describe{
#'   \item{age_pyramid}{Data frame containing the human population of England and Wales for year 2014, by age, sex and IMD.}
#'   \item{rates}{List containing 4 fields:
#'       \itemize{
#'         \item birth_params: List of 3 parameters (alpha, beta, TFR_weights) for creating the birth rates.
#'         \item death_male: Data frame of death rates for males by IMD.
#'         \item death_female: Data frame of death rates for females by IMD.
#'         \item swap: List of 3 parameters (ages, intensities and distribution) for creating the swap intensities.
#'       }
#'   }
#'   \item{sample}{Data frame containing a population of size $10^5$ sampled from `age_pyramid`.}
#' }
"EW_popIMD_14"


#' England and Wales human population output after simulation.
#'
#' @format Data frame containing a human population of England and Wales by age and sex,
#' simulated with an initial population of size $10^5$ sampled from `EW_pop_14$age_pyramid`
#' over 100 years.
"EW_pop_out"

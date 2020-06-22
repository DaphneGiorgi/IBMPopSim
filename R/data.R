#' England and Wales (EW) 2014 population, death and birth rates.
#'
#' @description EW 2014 population and death rates by age  and gender  (Source: Office for National Statistics, reference number 006518).
#'
#' Female birth rates by age of the mother (Source: Office for National Statistics birth summary tables).
#'
#' @format A list containing:
#' \describe{
#'   \item{\code{age_pyramid}}{Data frame containing EW age pyramid for year 2014, by gender and single year of age (0-115).}
#'   \item{\code{rates}}{A list containing three data frames:\describe{
#'                \item{\code{birth}}{Birth rates data frame, by age of mother and 5 years age groups.}
#'                \item{\code{death_male}}{Male death rates data frame, by single year of age (0-90+).}
#'                \item{\code{death_female}}{Female death rates dataframe, by single year of age (0-90+).}}}
#'   \item{\code{sample}}{Population dataframe composed of 100 000 individuals, sampled from \code{age_pyramid}.}
#' }
"EW_pop_14"

#' England and Wales (EW) 2014 population and death rates by Index of Multiple Deprivation (IMD).
#'
#' @description EW population, death rates by age, gender and IMD for year 2014 (Source: Office for National Statistics, reference number 006518).
#'
#' @format A list containing:
#' \describe{
#'   \item{\code{age_pyramid}}{Data frame containing EW age pyramid for year 2014, by gender, IMD and single year of age (0-115).
#'
#'   Individuals in the age class 90+ are distributed in the single year of age classes as in the EW population.}
#'   \item{\code{death_rates}}{List containing 4 fields:
#'       \itemize{
#'         \item{\code{male}}{ Male death rates data frame, by IMD and single year of age (0-90+).}
#'         \item{\code{female}}{ Female death rates dataframe, by IMD and single year of age (0-90+).}
#'       }
#'   }
#'   \item{\code{sample}}{Population dataframe composed of 100 000 individuals, sampled from \code{age_pyramid}.}
#' }
"EW_popIMD_14"


#' Example of "human population" after 100 years of simulation.
#'
#' @description   Example of "human population"  data frame after 100 years of simulation, based on a sample of England and Wales 2014 population and demographic rates.
#'
#' @format Data frame containing a population structured by age and gender,
#' simulated with an initial population of 100 000 individuals sampled from \code{EW_pop_14$age_pyramid}
#' over 100 years, with birth and death events.
"EW_pop_out"




#' Toy parameters for IBMPopSim-human_popIMD vignette example.
#'
#' @format A list containing:
#' \describe{
#'   \item{\code{birth}}{A list of 3 numeric vectors  (\code{alpha}, \code{beta}, \code{TFR_weights}) for creating birth intensity with the Weibull probability density function.}
#'   \item{\code{swap}}{A List of one numeric vector and two data frames  (\code{ages}, \code{intensities} and \code{distribution}) for creating the swap intensity and kernel functions.}
#' }
"toy_params"



#' England and Wale mortality data (source: Human Mortality Database)
#'
#' @description  Obtained with 
#' 
#' \code{EWdata_hmd <- hmd.mx(country = "GBRTENW", username = ... , password = ...,label = "England and Wales")}
"EWdata_hmd"

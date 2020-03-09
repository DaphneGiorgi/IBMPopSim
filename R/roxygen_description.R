# This add the lines
# importFrom(...,...)
# to the NAMESPACE file
# when calling roxygen2::roxygenize()

#' @importFrom Rcpp sourceCpp
#' @importFrom reshape melt cast
#' @importFrom dplyr filter count transmute mutate group_by group_by_at select summarise n %>%
#' @importFrom tidyr complete
#' @importFrom stats dweibull runif
#' @importFrom utils read.table
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_step geom_bar ggtitle theme xlab ylab coord_flip scale_x_continuous scale_y_continuous scale_fill_manual element_text
#' @importFrom ggthemes theme_wsj
#' @import readr
NULL

# definition of 'fake' global variables for a dplyr usage without warning using "R CMD check"
utils::globalVariables(c("popsim_cpp", "birth", "death", "age", "male", "value", "d_time", "scientific"))

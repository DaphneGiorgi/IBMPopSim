# This add the lines
# importFrom(...,...)
# to the NAMESPACE file
# when calling roxygen2::roxygenize()

#' @importFrom Rcpp sourceCpp
#' @importFrom reshape melt cast
#' @importFrom rlang .data
#' @importFrom dplyr filter count transmute mutate group_by group_by_at select select_at summarise n %>% everything full_join
#' @importFrom purrr reduce
#' @importFrom tidyr complete
#' @importFrom stats runif
#' @importFrom utils read.table
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_step geom_bar ggtitle theme xlab ylab coord_flip scale_x_discrete scale_y_continuous scale_fill_manual geom_col element_text
#' @import readr
#' @import checkmate
NULL

# definition of 'fake' global variables for a dplyr usage without warning using "R CMD check"
# no more used, replaced by the trick "@importFrom rlang .data"
#utils::globalVariables(c("popsim_cpp", "death", "age", "male", "value", "time", #"group", "d_time", "scientific", "group_name"))
utils::globalVariables(c("popsim_cpp"))

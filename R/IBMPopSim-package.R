#' @useDynLib IBMPopSim, .registration=TRUE

#' @exportPattern "^[[:alpha:]]+"

#' @importFrom Rcpp sourceCpp evalCpp
#' @importFrom rlang .data
#' @importFrom dplyr filter count mutate group_by group_by_at select select_at summarise n %>% everything
#' @importFrom stats runif
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_step geom_bar ggtitle theme xlab ylab coord_flip scale_x_discrete scale_y_continuous scale_fill_manual geom_col element_text
#' @import readr
#' @import checkmate
"_PACKAGE"

utils::globalVariables(c("popsim_cpp"))

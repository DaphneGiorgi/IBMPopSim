#' Get names and C types of the characteristics of the individuals in a population
#'
#' @param population Data frame representing the population, two columns must be 'birth' for the birth date and 'death' for the death date (eventually NA)
#'
#' @return List of names of characteristics associated with their and C types which describe an individual
#'
#' @export
get_characteristics <- function(population) {
    nams <- names(population)
    if (length(intersect(nams, c('birth', 'death'))) < 2) {
        stop("Argument population must have 'birth' and 'death' columns")
    }
    chars <- setdiff(nams, c('birth', 'death'))
    characteristics <- list()
    if (length(chars) > 0) {
        typC <- sapply(subset(population, select=chars), function(t) {
                           switch(typeof(t),
                                  "logical" = "bool",
                                  "integer" = "int",
                                  "numeric" = "double",
                                  "double" = "double",
                                  "character" = "char",
                                  stop("Type of a characteristic must be 'logical' or 'numeric' or 'double' or 'integer' or character'")
                           )
        })
        characteristics <- as.list(typC)
    }
    return(characteristics)
}

#' Create a model
#'
#' This function creates a model describing the population, the events which can occur in the population,
#' the available parameters.
#' It builds the C++ code using the code given in the events and produces the function `popsim_cpp`
#' which will be used for simulating a population
#'
#' @param characteristics List containing names and types of characteristics of initial population
#' @param events List of events in the model
#' @param parameters R parameters available from cpp
#'
#' @return model List containing the built model :
#' \itemize{
#'   \item individual_type: Names and types (R and C++) of an individual in the population
#'   \item parameters_types: Names and types (R and C++) of model parameters and functions
#'   \item parameters: Parameters, if there is a swap event in the list of events, it adds a column `id`
#'   \item events: List of events
#'   \item cpp: Output of C++ compilation
#'   \item popsim_cpp: Function for simulating the population resulting from the build process, which is called inside the R function `popsim``
#' }
#'
#'
#' @export
mk_model <- function(characteristics = NULL,
                     events,
                     parameters = NULL,
                     with_compilation = TRUE) {
    # population,
    #                  events,
    #                  parameters,
    #                  characteristics=NULL,
    #                  parameters_types=NULL) {
    # if (missing(population) && is.null(characteristics))
    #     stop("argument 'population' or 'characteristics' is missing")
    # if (missing(parameters) && is.null(parameters_types)) {
    #     warning("model without parameters")
    # } else {
    #     if (is.null(parameters_types)) {
    #         parameters_types = mk_parameters_types(parameters)
    #     }
    #     else parameters_types = parameters_types
    # }
    # if (is.null(individual_type))
    #     individual_type = mk_individual_type(population)

    individual_type = mk_individual_type(characteristics)
    if (missing(parameters)) {
        warning("model without parameters")
        parameters_types = NULL #list(names=NULL, typesR=NULL, typesC=NULL)
    } else {
        parameters_types = mk_parameters_types(parameters)
    }

    with_id = (!is.null(parameters$with_id) && parameters$with_id)
    with_swap = FALSE
    for (k  in 1:length(events)) {
        with_swap = with_swap | ("swap" %in% events[[k]])
    }
    if (with_swap && !with_id) {
        warning("with_id is forced to TRUE by presence of swap event")
    }
    with_id = with_swap | with_id
    if (with_id && (!'id' %in% individual_type$names)) {
        warning("add 'id' as individual attributes")
        n = length(individual_type$names) + 1
        individual_type$names[[n]] = 'id'
        individual_type$typesR[[n]] = 'integer'
        individual_type$typesC[[n]] = 'int'
    }

    model <-  list("individual_type" = individual_type,
                   "parameters_types" = parameters_types,
                   "parameters" = parameters,
                   "events" = events)

    model$cpp_code <- mkcpp_popsim(model, with_id)
    class(model) <- "model"
    if (with_compilation == TRUE) {
        model$sourceCpp = sourceCpp(code = model$cpp_code)
        model$popsim_cpp <- popsim_cpp
    }
    return(model)
}

#' Summary of a model
#'
#' @param object argument of class 'model'
#' @param ... additional arguments affecting the summary produced.
#'
#' @export
summary.model <- function(object, ...) {
    stopifnot(inherits(object, "model"))

    cat("Events:")
    for (i in 1:length(object$events))
        cat("\n", sep="",
            sprintf("#%s", i),
            ": ",
            sprintf("%s event of type %s", object$events[[i]]$type[1], object$events[[i]]$type[2])
        )

    cat("\n---------------------------------------",
        "\nIndividual description:\nnames: ", object$individual_type$names,
        "\nR types: ", object$individual_type$typesR,
        "\nC types: ", object$individual_type$typesC)

    if (!is.null(object$parameters_types))
        cat("\n---------------------------------------",
            "\nR parameters available in C++ code:\nnames: ", object$parameters_types$names,
            "\nR types: ", object$parameters_types$typesR,
            "\nC types: ", object$parameters_types$typesC)
    if (!is.null(object$R_functions))
        cat("\n---------------------------------------",
            "\nR functions available in C++ code:\nnames: ", names(object$R_functions)
        )
}

#' @keywords internal
mk_individual_type <- function(characteristics) {
    typR <- sapply(characteristics, function(t) {
                       switch(t,
                              "bool" = "logical",
                              "int" = "integer",
                              "double" = "double",
                              "char" = "character"
                       )
    })
    individual_type = list("names" = c('birth', 'death', names(characteristics)),
                           "typesR" = c('double', 'double', as.character(typR)),
                           "typesC" = c('double', 'double', as.character(characteristics)))
    return(individual_type)
}

#' @keywords internal
get_Rtype <- function(x) {
    try(
        # unidimensional value
        if (length(x) == 1) {
            ifelse(typeof(x) %in% c('integer', 'double', 'logical', 'closure'), return (typeof(x)), stop("The only accepted values for unidimensional parameters are: integer, double, logical, closure."))
        }
        # list
        else{ if(typeof(x) == 'list'){ return("list")}
        else { if(is.matrix(x) && typeof(x) %in% c('integer', 'double')){ return("matrix")}
        else{ if(is.null(dim(x)) && typeof(x) %in% c('integer', 'double')){return("vector")}
        else{stop("Invalid R parameter type")}
        }
        }
        }
    )
}

#' @keywords internal
get_Ctype <- function(x) {
    typR <- get_Rtype(x)
    if (typR == "closure") {
        if ('function' %in% class(x) && length(class(x)) == 1)
            typC <- "function"
        else{
            typ_fun <- setdiff(class(x), 'function')
            typC <- switch(typ_fun,
                           "stepfun" = "function_x",
                           "weibull" = "function_x",
                           "piecewise_x" = "function_x",
                           "piecewise_xy" = "function_xy")
        }
    }
    else if (typR == "list") {
        if (length(x) == 0)
            stop("List empty")
        if (typeof(x[[1]]) != 'closure') stop("Lists can be only lists of functions")
        inner_type = get_Ctype(x[[1]])
        typC <- paste0("list_of_", inner_type)
    }
    else {
        typC <- switch(typR,
                       "logical" = "bool",
                       "integer" = "int",
                       "double" = "double",
                       "character" = "char",
                       "vector" = "arma::vec",
                       "matrix" = "arma::mat")
    }
    return(typC)
}

#' Extract types (R and C++) of the parameters
#'
#' @param parameters parameters
#'
#' @return list
#'
#' @keywords internal
mk_parameters_types <- function(parameters) {
    nam <- names(parameters)
    typR <- sapply(parameters, get_Rtype)
    typC <- sapply(parameters, get_Ctype)
    list("names" = nam,
         "typesR" = as.vector(typR),
         "typesC" = as.vector(typC))
}


#' Returns names and C types of the characteristics.
#'
#' @description  Returns names and C types of the characteristics of the individuals in a population, from a population data frame.
#'
#' @param population Population data frame: characteristics in columns and individuals in rows. At least two columns \code{birth} (dates of birth) and \code{death} (dates of death) are required.
#' @return Named vector composed of characteristics names and C types.
#'
#' @examples
#' get_characteristics(EW_pop_14$sample)
#'
#' @export
get_characteristics <- function(population) {
    assertPopulation(population)
    chis <- setdiff(names(population), c('birth', 'death'))
    if (length(chis) > 0) {
        result <- sapply(subset(population, select=chis), function(t) {
                             switch(typeof(t),
                                    "logical" = "bool",
                                    "integer" = "int",
                                    "numeric" = "double",
                                    "double" = "double",
                                    "character" = "char",
                                    stop("Type of a characteristic must be 'logical' or 'numeric' or 'double' or 'integer' or character'")
                             )})
        return(result)
    } else {
        return(NULL)
    }
}

#' Creates a model for IBMPopSim.
#'
#' @description  This function creates an Individual Based Model describing the population, events which can occur in the population, and the model parameters.
#'
#' @param characteristics List containing names and types of characteristics of individuals in the population. See \code{\link{get_characteristics}}.
#' @param events List of events in the model. See \code{\link{mk_event_poisson}}, \code{\link{mk_event_inhomogeneous_poisson}}, \code{\link{mk_event_individual}}, and \code{\link{mk_event_interaction}}.
#' @param parameters Model parameters. A list of parameters of the model.
#' @param with_id _(Optional)_ Logical argument, \code{FALSE} by default. If \code{TRUE}, each individuals is given a unique \code{id}, which allows the identification of individual life histories in the presence of swap events.
#' @param with_compilation _(Optional)_ Logical parameter, \code{TRUE} by default. If \code{FALSE} the \code{sourceCpp} function is not called.
#'
#' @details It builds the C++ model code and produces the function \code{popsim_cpp} which will be used for simulating the model. The function used to simulate a population from a model is \code{\link{popsim}}.
#'
#' @return model List containing the built model :
#' \itemize{
#'   \item \code{individual_type}: Names and types (R and C++) of characteristics.
#'   \item \code{parameters_types}: Names and types (R and C++) of model parameters.
#'   \item \code{events}: List of events.
#'   \item \code{cpp_code}: Output of C++ compilation.
#' }
#'
#' @examples
#'\dontrun{
#'params <- list("p_male"= 0.51,
#'               "birth_rate" = stepfun(c(15,40),c(0,0.05,0)),
#'               "death_rate" = gompertz(0.008,0.02))
#'
#'death_event <- mk_event_individual(type = "death",
#'                                   intensity_code = "result = death_rate(age(I,t));")
#'
#'birth_event <- mk_event_individual(type = 'birth',
#'                                   intensity_code = "if (I.male) result = 0;
#'                                     else result=birth_rate(age(I,t));",
#'                                   kernel_code = "newI.male = CUnif(0, 1) < p_male;")
#'
#'model <- mk_model(characteristics = get_characteristics(population_df),
#'                  events = list(death_event,birth_event),
#'                  parameters = params)
#'
#'summary(model)
#'}
#'
#'@seealso \code{\link{popsim}}, \code{\link{mk_event_poisson}}, \code{\link{mk_event_individual}}, \code{\link{mk_event_interaction}}.
#'
#' @export
mk_model <- function(characteristics = NULL,
                     events,
                     parameters = NULL,
                     with_id = FALSE,
                     with_compilation = TRUE) {
    assertCharacteristics(characteristics)
    individual_type <- mk_individual_type(characteristics)
    assertList(events, types = 'event', min.len = 1, unique = TRUE,
               null.ok = FALSE, any.missing = FALSE)
    name_events <- sapply(events, function(e) { e$name })
    assertCharacter(name_events, unique = TRUE, null.ok = FALSE,
                    any.missing = FALSE, pattern = "^[a-zA-Z0-9_]*$")
    if (missing(parameters)) {
        warning("model without parameters")
        parameters_types = NULL
    } else {
        assertParameters(parameters)
        parameters_types = mk_parameters_types(parameters)
    }
    assertFlag(with_id)
    assertFlag(with_compilation)

    if (with_id) {
        print("add 'id' as individual attributes")
        n = length(individual_type$names) + 1
        individual_type$names[[n]] = 'id'
        individual_type$typesR[[n]] = 'integer'
        individual_type$typesC[[n]] = 'int'
    }
    if (has_event_type(events, 'entry')) {
        print("entry event: add 'entry' as individual attributes")
        n = length(individual_type$names) + 1
        individual_type$names[[n]] = 'entry'
        individual_type$typesR[[n]] = 'double'
        individual_type$typesC[[n]] = 'double'
    }
    if (has_event_type(events, 'exit')) {
        print("exit event: add 'out' as individual attributes")
        n = length(individual_type$names) + 1
        individual_type$names[[n]] = 'out'
        individual_type$typesR[[n]] = 'logical'
        individual_type$typesC[[n]] = 'bool'
    }


    model <-  list("individual_type" = individual_type,
                   "parameters_types" = parameters_types,
                   "events" = events)

    model$cpp_code <- mkcpp_popsim(model, with_id)
    class(model) <- "model"
    if (with_compilation == TRUE) {
        model$sourceCpp = sourceCpp(code = model$cpp_code)
        model$popsim_cpp <- popsim_cpp
    }
    return(model)
}

#' Summary of a model.
#'
#' @param object argument of class \code{model}
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
                           "linfun" = "function_x",
                           "weibull" = "function_x",
                           "gompertz" = "function_x",
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
         "typesC" = as.vector(typC),
         "lengths" = sapply(parameters, length))
}

has_event_type <- function(events, str) {
    b = FALSE
    for (ev in events) {
        b = b | (str %in% ev$type)
    }
    return(b)
}

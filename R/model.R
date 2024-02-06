#' Class population
#'
#' Data frame containing a population,
#' with at least a birth and a death column, and eventually some other characteristics
#'
#' @param x Data frame or list of data frames, containing at least a birth and a death column
#' @param entry Boolean flag. By default set to FALSE. If set to TRUE the population must contain a column of numerical values named "entry",
#' If the column doesn't exist a column named "entry" is added to the data frame with all values set to NA.
#' @param out Boolean flag. By default set to FALSE. If set to TRUE the population must contain a column of boolean values named "out",
#' If the column doesn't exist a column named "out" is added to the data frame with all the values set to FALSE.
#' @param id Boolean flag. By default set to FALSE. If set to TRUE the population must contain a column of integer distinct values named "id".
#' If the column doesn't exist a column named "id" is added to the data frame with values seq(1, nrow(x)).
#'
#' @return Given data frame augmented of the "population" class. If a list of data frames is given,
#' the column names should contain the string "id" and the list corresponds to the evolution of a population
#' at different times. The constructor then returns the last population observed in the list
#' (corresponding to the final state of the population).
#'
#'
#' @docType class
#' @export
population <- function(x, entry=FALSE, out=FALSE, id=FALSE) {
  if (inherits(x, "data.frame")){
    # if entry and not existing entry column add entry column
    if (entry){
      if (!'entry' %in% names(x))
        x = cbind(x, "entry" = NA)
    }
    # if out and not existing out column add out column
    if (out){
      if (!'out' %in% names(x))
        x = cbind(x, "out" = FALSE)
    }
    # if id and not existing id column add id column
    if(id){
      if(!'id' %in% names(x))
        x = cbind("id" = seq(1, nrow(x)), x)
    }
    assertPopulation(x)
    class(x) <- c("population", "data.frame")
  }
  else if (inherits(x, "population")){
    # if entry and not existing entry column add entry column
    if (entry){
      if (!'entry' %in% names(x))
        x = cbind(x, "entry" = NA)
    }
    # if out and not existing out column add out column
    if (out){
      if (!'out' %in% names(x))
        x = cbind(x, "out" = FALSE)
    }
    # if id and not existing id column add id column
    if(id){
      if(!'id' %in% names(x))
        x = cbind("id" = seq(1, nrow(x)), x)
    }
    assertPopulation(x)
  }
  else if (inherits(x,"list")){
    for (p in x){
      assertPopulation(p)
      assertChoice("id",colnames(p))
    }
    n <- length(x)
    return(population(x[[n]]))
  }
  return(x)
}


#' Printing population
#'
#' @description
#' Print a population
#'
#' @param x Object of \link{population} class representing a population.
#' @param ... Additional arguments
#' @return Print the population
#'
#' @export
print.population <- function(x, ...){
  print.data.frame(x)
}


#' Summary population
#'
#' @description
#' Summary of a population with column names and number of individuals
#'
#' @param object Object of \link{population} class representing a population.
#' @param ... Additional arguments affecting the summary produced
#'
#' @return Print column names and number of individuals
#'
#' @export
summary.population <- function(object, ...){
  cat(paste(c("Population of", nrow(object), "individuals with columns names :", colnames(object),"\n"), collapse=" "))
}


#' Generic method for get_characteristics
#'
#' @param object An object.
#' @param ... Additional parameters.
#'
#' @export
get_characteristics <- function(object, ...) {
  UseMethod("get_characteristics")
}


#' Returns names and C types of the characteristics.
#'
#' @description  Returns names and C types of the characteristics (other than birth and death) of the individuals in a population, from a population data frame.
#'
#' @param object Object of \link{population} class representing a population.
#' @param ... additional arguments.
#'
#' @return Named vector composed of characteristics names and C types. If the population has no characteristics, which means that it has only the \code{birth} and \code{death} columns, this returns \code{NULL}.
#'
#' @examples
#' get_characteristics(population(EW_pop_14$sample))
#'
#' @export
get_characteristics.population <- function(object,...) {
  chars <- sapply(object, class)
  if ("entry" %in% names(chars))
    chars <- replace(chars, "entry", "numeric")

  chars <- chars[!names(chars) %in% c("birth", "death")]

  if (length(chars) > 0) {
    result <- sapply(chars, function(t){switch(t,
                                               "logical" = "bool",
                                               "integer" = "int",
                                               "numeric" = "double",
                                               "double" = "double",
                                               "character" = "char",
                                               stop("Type of a characteristic must be 'logical' or 'numeric' or 'double' or 'integer' or character'"))})
    return(result)
  } else
    return(NULL)
}


#' Generic method for add_characteristic
#'
#' @param x An object.
#' @param name  Name of the characteristic to add.
#' @param value Value of the characteristic. By default \code{NA}.
#'
#' @export
add_characteristic <- function(x, name, value=NA) {
  UseMethod("add_characteristic")
}


#' Add characteristic to a population
#'
#' @param x Object of \link{population} class representing a population.
#' @param name Name of the characteristic to add.
#' @param value Value of the characteristic. By default \code{NA}.
#'
#' @export
add_characteristic.population <- function(x, name, value=NA){
  if (name %in% names(x))
    stop(paste0("Unable to add characteristic ", name," to population. ",name, " already in population characteristics."))
  x = cbind(x, name = value)
}


#' Generic method for population_alive
#'
#' @param object A population.
#' @param  t  A numeric indicating the time at which alive individuals are observed.
#' @param  a1  0 by default. Lower bound for age.
#' @param  a2 Inf by default. Upper bound for age.
#' @param ... Additional params.
#'
#' @return  All individuals alive at time \code{t} and of age in \code{[a1,a2)}.
#'
#' @export
population_alive <- function(object, t, a1=0, a2=Inf,...) {
  UseMethod("population_alive")
}


#' Returns a population of individuals alive.
#'
#' @param object A population data frame containing at least a column \code{birth} and \code{death}.
#' @param  t  A numeric indicating the time.
#' @param  a1  0 by default. All individuals of age over \code{a1} at \code{t} are selected.
#' @param  a2 Inf by default. All individuals of age below \code{a2} at \code{t} are selected.
#' @param ... Additional params.
#'
#'@return  The function returns a population data frame containing all individuals alive at time \code{t} and of age in \code{[a1,a2)}.
#'
#'@export
population_alive.population <- function(object, t, a1=0, a2=Inf, ...) {
  select = (is.na(object$death) | (object$death > t)) &
    (t - object$birth>=a1) & (t - object$birth< a2)
  if ('entry' %in% colnames(object)){
    select <- select & (is.na(object$entry) | (object$entry <=t))
  }

  df_alive  <- population(subset(object, select))
  return(df_alive)
}


#' Creates a model for IBMPopSim.
#'
#' @description  This function creates an Individual Based Model describing the population, events which can occur in the population, and the model parameters.
#'
#' @param characteristics List containing names and types of characteristics of individuals in the population. See \code{\link{get_characteristics}}.
#' @param events List of events in the model. See \code{\link{mk_event_poisson}}, \code{\link{mk_event_inhomogeneous_poisson}}, \code{\link{mk_event_individual}}, and \code{\link{mk_event_interaction}}.
#' @param parameters Model parameters. A list of parameters of the model.
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
#'\donttest{
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
#'model <- mk_model(characteristics = get_characteristics(population(EW_pop_14$sample)),
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
                     with_compilation = TRUE) {
    assertCharacteristics(characteristics)
    assertEvents(events)

    if (missing(parameters)) {
        warning("model without parameters")
        parameters_types = NULL
    } else {
        assertParameters(parameters)
        parameters_types = mk_parameters_types(parameters)
    }
    assertFlag(with_compilation)

    compatibility_chars_events(characteristics, events)

    individual_type <- mk_individual_type(characteristics)

    with_id = FALSE

    if("id" %in% individual_type$names)
      with_id = TRUE

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


#' Printing of a model
#'
#' @description \code{print} method for class \code{model}.
#'
#' @param x argument of class \code{model}
#' @param ... additional arguments affecting the summary produced.
#'
#' @export
print.model <- function(x, ...) {
  stopifnot(inherits(x, "model"))

  cat("Events:")
  for (i in 1:length(x$events))
    cat("\n", sep="",
        sprintf("#%s", i),
        ": ",
        sprintf("%s event of type %s", x$events[[i]]$type[1], x$events[[i]]$type[2])
    )

  cat("\n---------------------------------------",
      "\nIndividual description:\nnames: ", x$individual_type$names,
      "\nR types: ", x$individual_type$typesR,
      "\nC types: ", x$individual_type$typesC, "\n")

  if (!is.null(x$parameters_types))
    cat("---------------------------------------",
        "\nR parameters available in C++ code:\nnames: ", x$parameters_types$names,
        "\nR types: ", x$parameters_types$typesR,
        "\nC types: ", x$parameters_types$typesC, "\n")
  if (!is.null(x$R_functions))
    cat("---------------------------------------",
        "\nR functions available in C++ code:\nnames: ", names(x$R_functions), "\n"
    )
}


#' Summary of a model
#'
#' @description \code{summary} method for class \code{model}.
#'
#' @param object argument of class \code{model}
#' @param ... additional arguments affecting the summary produced.
#'
#' @export
summary.model <- function(object, ...) {
    stopifnot(inherits(object, "model"))

    cat("Events description:\n")
    for (i in 1:length(object$events)){
      cat(paste0("[[", i , "]]"))
      summary.event(object$events[[i]])
    }

    cat("\n---------------------------------------",
        "\nIndividual description:\nnames: ", object$individual_type$names,
        "\nR types: ", object$individual_type$typesR,
        "\nC types: ", object$individual_type$typesC, "\n")

    if (!is.null(object$parameters_types))
        cat("---------------------------------------",
            "\nR parameters available in C++ code:\nnames: ", object$parameters_types$names,
            "\nR types: ", object$parameters_types$typesR,
            "\nC types: ", object$parameters_types$typesC, "\n")
    if (!is.null(object$R_functions))
        cat("---------------------------------------",
            "\nR functions available in C++ code:\nnames: ", names(object$R_functions), "\n"
        )
}


#' Function for internal use
#' @description Get R and C++ type of characteristics.
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


#' Function for internal use
#' @description Get R type of a parameter.
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


#' Function for internal use
#' @description Get C++ type of a parameter.
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


#' Search for a given string in event types
#'
#' @param events list of events
#' @param str string to search
#'
#' @return boolean
#'
#' @keywords internal
has_event_type <- function(events, str) {
    b = FALSE
    for (ev in events) {
        b = b | (str %in% ev$type)
    }
    return(b)
}


#' Summary logs
#'
#' @description
#' Summary of the logs of a simulation
#'
#' @param object Logs of the output of a call to \link{popsim} function
#' @param ... Additional arguments affecting the summary produced
#'
#' @return Print column names and number of individuals
#'
#' @export
summary.logs <- function(object, ...){
  cat("Simulation executed in ", object["duration_ns"]*1e-06,"s\n")
  cat("Proposed events : ",object["proposed_events"],"\n")
  cat("Effective events : ",object["effective_events"],"\n")
  cat("Population was cleaned from dead individuals ", object["cleanall_count"], " times.\n")
}


#' Summary simulation output
#'
#' @description
#' Summary of a simulation output
#'
#' @param object Output of a call to \link{popsim} function
#' @param ... Additional arguments affecting the summary produced
#'
#' @return Summary of population(s) and the logs
#'
#' @export
summary.simulation_output <- function(object, ...){
  if ("list" %in% class(object$population) && "list" %in% class(object$logs)){

    cat("List of ", length(object)," output populations and correpondings logs : \n")
    for (p in object$population)
      summary(p)
    for (l in object$logs)
      summary(l)
  }
  else {
    summary(object$population)
    summary(object$logs)
  }
}

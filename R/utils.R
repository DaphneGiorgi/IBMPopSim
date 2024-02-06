#' Function for internal use
#' @description Check on the population.
#' @keywords internal
assertPopulation <- function(object) {
    assertDataFrame(object, min.rows = 1, col.names = "named",
                    types = c('logical', 'integer', 'double', 'numeric', 'character'))
    assertNames(names(object), must.include = c('birth', 'death'))
    assertNumeric(object$birth, finite = TRUE, any.missing = FALSE)
    assertNumeric(object$death, finite = TRUE)
    assertNumeric(object$entry, finite = TRUE, null.ok = TRUE, any.missing = TRUE)
    # if 'male', it must be a logical vector
    assertLogical(object$male, null.ok = TRUE, any.missing = FALSE)
    # if 'out', it must be a logical vector
    assertLogical(object$out, null.ok = TRUE, any.missing = FALSE)
    # if 'id', it must be numeric vector of distinct positive integers
    assertInteger(object$id, null.ok = TRUE, any.missing = FALSE,
                lower = 0, unique = TRUE)
    # if 'group_name', it must be a vector of type character
    assertCharacter(object$group, null.ok = TRUE, any.missing = FALSE)
}

#' Function for internal use
#' @description Check on a pyramid.
#' @keywords internal
assertPyramid <- function(pyramid) {
    assertDataFrame(pyramid, min.rows = 1, col.names = "named")
    assertNames(names(pyramid), must.include = c('age', 'value'))
    # if 'male', it must be a logical vector
    assertLogical(pyramid$male, null.ok = TRUE, any.missing = FALSE)
    # if 'group', it must be a vector of type character
    assertCharacter(pyramid$group, null.ok = TRUE, any.missing = FALSE)
}

#' Function for internal use
#' @description Check characteristics.
#' @keywords internal
assertCharacteristics <- function(characteristics) {
    assertCharacter(characteristics, null.ok = TRUE, any.missing = FALSE)
    if (!is.null(characteristics)) {
        assertNames(characteristics, subset.of = c('bool', 'int', 'double', 'char'))
        assertNames(names(characteristics), type = "unique", disjunct.from = c("I", "J", "t", "pop", "newI"))
        # must be compatible with the identifier of a variable in C++
        assertCharacter(names(characteristics), pattern = "^[a-zA-Z0-9_]*$")
    }
}

#' Function for internal use
#' @description Check events
#' @keywords internal
assertEvents <- function(events) {
  assertList(events, types = 'event', min.len = 1, unique = TRUE,
             null.ok = FALSE, any.missing = FALSE)
  name_events <- sapply(events, function(e) { e$name })
  assertCharacter(name_events, unique = TRUE, null.ok = FALSE,
                  any.missing = FALSE, pattern = "^[a-zA-Z0-9_]*$")
}

#' Function for internal use
#' @description Check parameters.
#' @keywords internal
assertParameters <- function(parameters) {
    assertList(parameters, types = c('logical', 'integer', 'double', 'character', 'vector', 'matrix', 'function', 'list'), any.missing = FALSE, null.ok = TRUE)
    if (!is.null(parameters)) {
        assertNames(names(parameters), type = "unique", disjunct.from = c("I", "J", "t", "pop", "newI","id", "entry", "out"))
        # must be compatible with the identifier of a variable in C++
        assertCharacter(names(parameters), pattern = "^[a-zA-Z0-9_]*$")
    }
}

#' Check population-model compatibility
#' @description A function to check the compatibility between a population and a model
#' @param pop An object of class \link{population}
#' @param model An Individual Based Model created with the \link{mk_model} function
#'
#' @export
compatibility_pop_model <- function(pop, model){
  if (!setequal(names(pop), model$individual_type$names)){
    msg <- "Population must be compatible with model and characteristics name.\n"

    if ('id' %in% model$individual_type$names){
      if (!'id' %in% names(pop)){
        msg <- paste0(msg,"Model has an 'id' characteristic name and population has no 'id' column.\nAdd an id column to the population, for example by calling the population constructor with the flag 'id' set to TRUE.")
      }
    }

    if ('id' %in% names(pop)){
      if (!('id' %in% model$individual_type$names)){
        msg <- paste0(msg,"Population has an 'id' column and model has no 'id' characteristic name.\nAdd id to model characteristics, for example by calling mk_model with characteristics = get_characteristics(pop).")
      }
    }

    if ('entry' %in% model$individual_type$names){
      if (!'entry' %in% names(pop)){

        msg <- paste0(msg, "Model contains an 'entry' event and population has no 'entry' column.\nAdd an entry column to the population, for example by calling the population constructor with the flag 'entry' set to TRUE.")
      }
    }

    if ('entry' %in% names(pop)){
      if (!('entry' %in% model$individual_type$names)){

        msg <- paste0(msg, "Population has an 'entry' column and model has no 'entry' characteristic name.\nAdd entry to model characteristics, for example by calling mk_model with characteristics = get_characteristics(pop).")
      }
    }

    if ('out' %in% model$individual_type$names){
      if (!'out' %in% names(pop)) {
        msg <- paste0(msg, "Model contains an 'exit' event and population has no 'out' column.\nAdd an out column to the population, for example by calling the population constructor with the flag 'out' set to TRUE.")
      }
    }

    if ('out' %in% names(pop)){
      if (!'out' %in% model$individual_type$names){
        msg <- paste0(msg, "Population has an 'out' column and model has no 'out' characteristic name.\nAdd out to model characteristics, for example by calling mk_model with characteristics = get_characteristics(pop).")
      }
    }

    stop(msg)
  }
}

#' Check characteristics-events compatibility
#' @description A function to check the compatibility between characteristics and events
#' @param characteristics List of characteristics
#' @param events List of events
#'
#' @export
compatibility_chars_events <- function(characteristics, events){
  # if there is a swap event and no id characteristic, warn the user
  if(has_event_type(events, 'swap') && !('id' %in% names(characteristics))){
    warning("The list of events contains a 'swap' event and there is no 'id' in the characteristics.\nAdd 'id' to the characteristics if tracking changes along time is desired.")
  }

  # for incompatibilities with entry and exit events stop execution
  # for an id characteristic without a corresponding swap event stop execution

  if (has_event_type(events, 'entry') && !('entry' %in% names(characteristics))){
    stop("The list of events contains an 'entry' event and there is no 'entry' in the characteristics names.\nAdd 'entry' (of type 'double') to the characteristics by hand, or create the population with the flag 'entry' set to TRUE.")
  }

  if (has_event_type(events, 'exit') && !('out' %in% names(characteristics))){
    stop("The list of events contains an 'exit' event and there is no 'out' in the characteristics names.\nAdd 'out' (of type 'logical') to the characteristics by hand, or create the population with the flag 'out' set to TRUE.")
  }

  if ('id' %in% names(characteristics) && !(has_event_type(events, 'swap'))){
    stop("'id' is a characteristics name reserved for 'swap' events.\nEither add a 'swap' event or change characteristic 'id' name.")
  }

  if ('entry' %in% names(characteristics) && !(has_event_type(events, 'entry'))){
    stop("'entry' is a characteristics name reserved for 'entry' events.\nEither add an 'entry' event or change characteristic 'entry' name.")
  }

  if ('out' %in% names(characteristics) && !(has_event_type(events, 'exit'))){
    stop("'out' is a characteristics name reserved for 'exit' events.\nEither add an 'exit' event or change characteristic 'out' name.")
  }
}

#' A function returning a merged dataframe from a list of population dataframes with id.
#'
#' @param pop_df_list A list of population dataframe where the first three columns of each dataframe are id, birth and death.
#' @param chars_tracked A vector of characteristics to be tracked over time.
#'
#' @return A dataframe composed of all individuals with their characteristics at each simulation time.
#'
#'
#' @export
merge_pop_withid <- function(pop_df_list, chars_tracked = NULL){
  if (!any(chars_tracked %in% colnames(pop_df_list[[1]]))) {
    stop("Argument chars_tracked must be a vector of characteristics of a dataframe of pop_df_list.")
  }
  n = length(pop_df_list)
  result = pop_df_list[[n]][setdiff(colnames(pop_df_list[[n]]), chars_tracked)]
  for (i in 1:n) {
    names = paste0(chars_tracked, "_", i)
    select = pop_df_list[[n]]$id %in% pop_df_list[[i]]$id
    result[select, names] = pop_df_list[[i]][chars_tracked]
  }
  return(result)
}



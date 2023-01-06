#' Function for internal use
#' @description Check on the population.
#' @keywords internal
assertPopulation <- function(population) {
    assertDataFrame(population, min.rows = 1, col.names = "named",
                    types = c('logical', 'integer', 'double', 'numeric', 'character'))
    assertNames(names(population), must.include = c('birth', 'death'))
    assertNumeric(population$birth, finite = TRUE, any.missing = FALSE)
    assertNumeric(population$death, finite = TRUE)
    # if 'male', it must be a logical vector
    assertLogical(population$male, null.ok = TRUE, any.missing = FALSE)
    # if 'id', it must be numeric vector of distinct positive integers
    assertInteger(population$id, null.ok = TRUE, any.missing = FALSE,
                  lower = 0, unique = TRUE)
    # if 'group_name', it must be a vector of type character
    assertCharacter(population$group, null.ok = TRUE, any.missing = FALSE)
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
        assertNames(names(characteristics), type = "unique", disjunct.from = c("I", "J", "t", "pop", "newI", "id", "entry", "out"))
        # must be compatible with the identifier of a variable in C++
        assertCharacter(names(characteristics), pattern = "^[a-zA-Z0-9_]*$")
    }
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

#' Returns a population of individuals alive.
#'
#' @param population A population data frame containing at least a column \code{'birth'} and \code{'death'}.
#' @param  t  A numeric indicating the time.
#' @param  a1  0 by default. All individuals of age over \code{a1} at \code{t} are selected.
#' @param  a2 Inf by default. All individuals of age below \code{a2} at \code{t} are selected.
#'
#'@return  The function returns a population data frame containing all individuals alive at time \code{t} and of age in \code{[a1,a2)}.
#'
#'@export
population_alive <- function(population, t, a1=0, a2=Inf) {
  select = (is.na(population$death) | (population$death > t)) &
    (t - population$birth>=a1) & (t - population$birth< a2)
  if ('entry' %in% colnames(population)){
    df_alive <- subset(population,
                       select & (is.na(population$entry) | (population$entry <=t)))
  }
  else{
    df_alive  <- subset(population, select)
  }
  return(df_alive)
}

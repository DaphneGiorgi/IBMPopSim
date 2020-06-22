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

#' @keywords internal
assertPyramid <- function(pyramid) {
    assertDataFrame(pyramid, min.rows = 1, col.names = "named")
    assertNames(names(pyramid), must.include = c('age', 'value'))
    # if 'male', it must be a logical vector
    assertLogical(pyramid$male, null.ok = TRUE, any.missing = FALSE)
    # if 'group', it must be a vector of type character
    assertCharacter(pyramid$group, null.ok = TRUE, any.missing = FALSE)
}

#' @keywords internal
assertCharacteristics <- function(characteristics) {
    assertCharacter(characteristics, null.ok = TRUE, any.missing = FALSE)
    if (!is.null(characteristics)) {
        assertNames(characteristics, subset.of = c('bool', 'int', 'double', 'char'))
        assertNames(names(characteristics), type = "unique")
        # must be compatible with the identifier of a variable in C++
        assertCharacter(names(characteristics), pattern = "^[a-zA-Z0-9_]*$")
    }
}

#' @keywords internal
assertParameters <- function(parameters) {
    assertList(parameters, types = c('logical', 'integer', 'double', 'character', 'vector', 'matrix', 'function', 'list'), any.missing = FALSE, null.ok = TRUE)
    if (!is.null(parameters)) {
        assertNames(names(parameters), type = "unique")
        # must be compatible with the identifier of a variable in C++
        assertCharacter(names(parameters), pattern = "^[a-zA-Z0-9_]*$")
    }
}

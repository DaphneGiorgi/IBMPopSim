check_event_type <- function(type) {
    if (!(type %in% c("death", "birth", "entry", "exit", "swap", "custom"))) {
        stop("The string argument 'type' must be 'death', 'birth', 'entry', 'exit', 'swap' or 'custom'")
    }
}

check_intensity_code <- function(code) {
    if (!grepl("result", code)) {
        stop("The string argument 'intensity_code' must contain keyword 'result'")
    }
}

check_kernel_code <- function(code) {
    # to do
}

#' Create Poisson class event
#'
#' @param type Possible types are :
#' \describe{
#'   \item{birth}{By default, a new individual \emph{newI} is created, with the same caracteristics of the parent \emph{I} and birth date equal to the current time.}
#'   \item{death}{By default, the individual \emph{I} dies.}
#'   \item{entry}{A new individual \emph{newI} is added to the population, and its caracteristics have to be defined by the user in the entry kernel code.}
#'   \item{exit}{An individual I exits from the population.}
#'   \item{swap}{The user can change the caracteristics of the selected individual \emph{I}.}
#'   \item{\emph{custom}}{None of the above types, the user defines the kernel that can act on the selected individual \emph{I} and on the population \emph{pop}.}
#' }
#' @param intensity String containing some constant value or name of a parameter which is a constant value.
#' @param kernel_code C++ code describing the kernel.
#' @param name optional name parameter.
#'
#' @return Poisson event
#'
#' @export
mk_event_poisson <- function(type,
                             intensity,
                             kernel_code = '',
                             name = NULL) {
    check_kernel_code(kernel_code)
    event = list("type" = c("poisson", type),
                 "name" = name,
                 "intensity_code" = intensity,
                 "kernel_code" = kernel_code)

    event$cpp_code <- mkcpp_event(event, type, "poisson")
    class(event) <- "event"
    return(event)
}

#' Create inhomogeneous Poisson class event
#'
#' @param type Possible types are :
#' \describe{
#'   \item{birth}{By default, a new individual \emph{newI} is created, with the same caracteristics of the parent \emph{I} and birth date equal to the current time.}
#'   \item{death}{By default, the individual \emph{I} dies.}
#'   \item{entry}{A new individual \emph{newI} is added to the population, and its caracteristics have to be defined by the user in the entry kernel code.}
#'   \item{exit}{An individual I exits from the population.}
#'   \item{swap}{The user can change the caracteristics of the selected individual \emph{I}.}
#'   \item{\emph{custom}}{None of the above types, the user defines the kernel that can act on the selected individual \emph{I} and on the population \emph{pop}.}
#' }
#' @param intensity_code String containing some C++ code which describes the intensity and stores the intensity value into the variable \emph{result}.
#' The available parameters for the C++ code are :
#' \describe{
#'   \item{k}{Index of the current individual in the population}
#'   \item{pop}{Population}
#'   \item{t}{Current time}
#'   \item{I}{Current individual (equal to pop[k])}
#' }
#' @param kernel_code C++ code describing the kernel.
#' @param name optional name parameter.
#'
#' @return Poisson event
#'
#' @export
mk_event_inhomogeneous_poisson <- function(type,
                                             intensity_code,
                                             kernel_code = '',
                                             name = NULL) {
    check_kernel_code(kernel_code)
    event = list("type" = c("inhomogeneous_poisson", type),
                 "name" = name,
                 "intensity_code" = intensity_code,
                 "kernel_code" = kernel_code)

    event$cpp_code <- mkcpp_event(event, type, "inhomogeneous_poisson")
    class(event) <- "event"
    return(event)
}

#' Create individual class event
#'
#' @param type Possible types are :
#' \describe{
#'   \item{birth}{By default, a new individual \emph{newI} is created, with the same caracteristics of the parent \emph{I} and birth date equal to the current time.}
#'   \item{death}{By default, the individual \emph{I} dies.}
#'   \item{entry}{A new individual \emph{newI} is added to the population, and its caracteristics have to be defined by the user in the entry kernel code.}
#'   \item{exit}{An individual I exits from the population.}
#'   \item{swap}{The user can change the caracteristics of the selected individual \emph{I}.}
#'   \item{\emph{custom}}{None of the above types, the user defines the kernel that can act on the selected individual \emph{I} and on the population \emph{pop}.}
#' }
#' @param intensity_code String containing some C++ code which describes the intensity and stores the intensity value into the variable \emph{result}.
#' The available parameters for the C++ code are :
#' \describe{
#'   \item{k}{Index of the current individual in the population}
#'   \item{pop}{Population}
#'   \item{t}{Current time}
#'   \item{I}{Current individual (equal to pop[k])}
#' }
#' @param kernel_code C++ code describing the kernel.
#' @param name optional name parameter.
#' 
#' @return Individual event
#'
#' @export
mk_event_individual <- function(type,
                                intensity_code,
                                kernel_code = NULL,
                                name = NULL) {
    if (type == "death") {
        if (!is.null(kernel_code))
            warning("The argument 'kernel_code' is ignored for a death event")
        kernel_code = ''
    } else {
        if (is.null(kernel_code)) kernel_code = ''
    }
    check_intensity_code(intensity_code)
    # a faire: check_intensity_bound(intensity_bound) (expression pas de ;)
    check_kernel_code(kernel_code)

    event = list("type" = c("individual", type),
                 "name" = name,
                 "intensity_code" = intensity_code,
                 "kernel_code" = kernel_code)

    event$cpp_code <- mkcpp_event(event, type, "individual")
    class(event) <- "event"
    return(event)
}

#' Create an interaction event to the model
#'
#' @param type Possible types are :
#' \describe{
#'   \item{birth}{By default, a new individual \emph{newI} is created, with the same caracteristics of the parent \emph{I} and birth date equal to the current time.}
#'   \item{death}{By default, the individual \emph{I} dies.}
#'   \item{entry}{A new individual \emph{newI} is added to the population, and its caracteristics have to be defined by the user in the entry kernel code.}
#'   \item{exit}{An individual I exits from the population.}
#'   \item{swap}{The user can change the caracteristics of the selected individual \emph{I}.}
#'   \item{\emph{custom}}{None of the above types, the user defines the kernel that can act on the selected individual \emph{I} and on the population \emph{pop}.}
#' }
#' @param interaction_type Can be full or random. The default type is full.
#' If type is full, the intensity is the sum over all the individuals in the population of the interaction kernel; if type is random,
#' the intensity is the interaction kernel applied to an individual randomly picked in the population.
#' @param interaction_code C++ code describing the interaction kernel and storing its value into the variable \emph{result}.
#' The interaction kernel is a function of the following parameters:
#' \describe{
#'   \item{k}{Index of the current individual in the population}
#'   \item{pop}{Population}
#'   \item{t}{Current time}
#'   \item{I}{Current individual (equal to pop[k])}
#'   \item{J}{Another individual in the population. }
#' }
#' @param kernel_code C++ code describing the kernel.
#' @param name optional name parameter.
#'
#' @return The original model with the added event.
#'
#' @export
mk_event_interaction <- function(type,
                                 interaction_type,
                                 interaction_code,
                                 kernel_code = NULL,
                                 name = NULL) {
    if (type == "death") {
        if (!is.null(kernel_code))
            warning("The argument 'kernel_code' is ignored for a death event")
        kernel_code = ''
    } else {
        if (is.null(kernel_code)) kernel_code = ''
    }
    if (!(interaction_type %in% c("full", "random"))) {
        stop("The string argument 'interaction_type' must be 'full' or 'random'")
    }

    check_kernel_code(kernel_code)

    event = list("type" = c("interaction", type),
                 "name" = name,
                 "intensity_type" = interaction_type,
                 "intensity_code" = interaction_code,
                 "kernel_code" = kernel_code)

    event$cpp_code <- mkcpp_event(event, type, paste0(interaction_type, "_interaction"))
    class(event) <- "event"
    return(event)
}

#' Summary of an event
#'
#' @param object argument of class 'event'
#' @param ... additional arguments affecting the summary produced.
#'
#' @export
summary.event <- function(object, ...) {
    stopifnot(inherits(object, "event"))

    cat("\t\n", sep="",
        sprintf("%s, type: %s", object$type[1], object$type[2]),
        if (is.null(object$name)) sprintf("\n") else sprintf(", name: %s\n", object$name)
    )
}


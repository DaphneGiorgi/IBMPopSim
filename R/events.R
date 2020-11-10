details_type_event <- function() {
    "@details The \\code{type} argument is one of the following
    \\describe{
        \\item{\\code{'birth'}}{By default, a new individual \\code{newI} is created, with the same characteristics of the parent \\code{I} and birth date equal to the current time. Optional code can be precised in \\code{kernel_code}.}
        \\item{\\code{'death'}}{By default, the individual \\code{I} dies. Optional code can be precised in \\code{kernel_code}.}
        \\item{\\code{'entry'}}{A new individual \\code{newI} is added to the population, and its characteristics have to be defined by the user in the entry \\code{kernel_code}.}
        \\item{\\code{'exit'}}{An individual \\code{I} exits from the population. Optional code can be precised in \\code{kernel_code}.}
        \\item{\\code{'swap'}}{The user can change the characteristics of the selected individual \\code{I}. This requires \\code{kernel_code}.}
        \\item{\\code{'custom'}}{None of the above types, the user defines \\code{kernel_code} that can act on the selected individual \\code{I} and on the population \\code{pop}.}
    }"
}

details_intensity_code <- function() {
    "@details The \\code{intensity_code} argument is a string containing some C++ code describing the event intensity for individual \\code{I} at time \\code{t}. The intensity value \\strong{must be stored} in the variable \\code{result}.
Some of available variables in the C++ code are: \\code{t} (the current time), \\code{I} (the current individual selected for the event), the name of the model parameters (some variables, or functions, see \\code{\\link{mk_model}}).
See \\code{vignette('IBMPopSim_Cpp')} for more details."
}

details_interaction_code <- function() {
    "@details The \\code{interaction_code} argument is a string containing some C++ code describing the event interaction function $U$ at time \\code{t}. The interaction value \\strong{must be stored} in the variable \\code{result}.
Some of available variables in the C++ code are: \\code{t} (the current time), \\code{I} (the current individual selected for the event), \\code{J} (another individual if \\code{interaction_type} is \\code{'random'}), the name of the model parameters (some variables, or functions, see \\code{\\link{mk_model}}).
See \\code{vignette('IBMPopSim_Cpp')} for more details."
}

details_kernel_code <- function() {
    "@details The \\code{kernel_code} argument is a string containing some C++ code which describing the action of the event. Some of available variables in the C++ code are: \\code{t} (the current time), \\code{pop} (the current population), \\code{I} (the current individual selected for the event), \\code{newI} (the new individual if \\code{'birth'} or \\code{'entry'} event), the name of the model parameters (some variables, or functions, see \\code{\\link{mk_model}}).
See \\code{vignette('IBMPopSim')} for more details."
}

#' Creates Poisson class event.
#'
#' @description The function \code{mk_event_poisson} is used to create an event with intensity of type Poisson (constant intensity which does not depend on population or time).
#' When the event occurs, something happens in the population.
#' The created event must be used with \code{\link{mk_model}}. 
#' @param type Must be one of \code{'birth'}, \code{'death'}, \code{'entry'}, \code{'exit'}, \code{'swap'} or \code{'custom'}. See details.
#' @param name _(Optional)_ If not specified, the name given to the event is its type.
#' @param intensity String containing some constant positive value, or name of a parameter which is a constant positive value.
#' @param kernel_code String containing some C++ code describing the event action. Optional for \code{'birth'}, \code{'death'} and \code{'exit'} events. See details.
#'
#' @eval details_type_event()
#' @eval details_kernel_code()
#' @return An S3 object of class \code{event} of type Poisson.
#'
#' @examples
#' birth <- mk_event_poisson('birth', intensity = 10)
#'
#' \donttest{
#' params <- list(beta = 10)
#' death <- mk_event_poisson('death', intensity = 'beta') # name of one parameter
#' mk_model(events = list(birth, death), parameters = params)
#' }
#' 
#' @seealso  See also \code{\link{mk_model}}, \code{\link{mk_event_inhomogeneous_poisson}}, \code{\link{mk_event_individual}}, \code{\link{mk_event_interaction}}.
#'
#' @export
mk_event_poisson <- function(type, name, intensity, kernel_code = '') {
    assertChoice(type, c('birth', 'death', 'entry', 'exit', 'swap', 'custom'))
    if (is.numeric(intensity))
        assertNumeric(intensity, lower = 0, len = 1, any.missing = FALSE)
    else if (is.character(intensity))
        assertCharacter(intensity, pattern = "^[a-zA-Z0-9_]*$")
    else
        stop("Intensity must be a numerical constant or a name of a variable.")
    check_kernel_code(kernel_code)
    if (missing(name)) name = type
    assertCharacter(name, null.ok = TRUE, len = 1, pattern = "^[a-zA-Z0-9_]*$")

    event = list("name" = name,
                 "type" = c("poisson", type),
                 "intensity_code" = as.character(intensity),
                 "kernel_code" = kernel_code)
    event$cpp_code <- mkcpp_event(event, type, "poisson")

    class(event) <- "event"
    return(event)
}

#' Creates inhomogeneous Poisson class event.
#'
#' @description The function \code{mk_event_inhomogeneous_poisson} is used to create an event with intensity type inhomogeneous Poisson (time dependent intensity which does not depend on population).
#' When the event occurs, something happens in the population.
#' The created event must be used with \code{\link{mk_model}}.
#' @inheritParams mk_event_poisson
#' @param intensity_code String containing some C++ code describing the intensity function. See details.
#'
#' @eval details_type_event()
#' @eval details_intensity_code()
#' @eval details_kernel_code()
#' @return An S3 object of class \code{event} of type inhomogeneous Poisson.
#'
#'@seealso \code{\link{mk_model}}, \code{\link{mk_event_poisson}}, \code{\link{mk_event_individual}}, \code{\link{mk_event_interaction}}.
#'
#' @export
mk_event_inhomogeneous_poisson <- function(type, name, intensity_code,
                                           kernel_code='') {
    assertChoice(type, c('birth', 'death', 'entry', 'exit', 'swap', 'custom'))
    check_intensity_code(intensity_code)
    check_kernel_code(kernel_code)
    if (missing(name)) name = type
    assertCharacter(name, null.ok = TRUE, len = 1, pattern = "^[a-zA-Z0-9_]*$")

    event = list("name" = name,
                 "type" = c("inhomogeneous_poisson", type),
                 "intensity_code" = intensity_code,
                 "kernel_code" = kernel_code)

    event$cpp_code <- mkcpp_event(event, type, "inhomogeneous_poisson")
    class(event) <- "event"
    return(event)
}

#' Creates an event with intensity of class individual.
#'
#' @description Creates an event with intensity of class individual (without interactions). When the event occurs, something happens to an individual \code{I} in the population.
#' The created event must be used with \code{\link{mk_model}}. 
#'
#' @inheritParams mk_event_inhomogeneous_poisson
#'
#' @eval details_type_event()
#' @eval details_intensity_code()
#' @eval details_kernel_code()
#' @return An S3 object of class \code{event} of type individual.
#'
#' @examples
#'params <- list("p_male"= 0.51,
#'               "birth_rate" = stepfun(c(15,40), c(0,0.05,0)),
#'               "death_rate" = gompertz(0.008, 0.02))
#'
#'death_event <- mk_event_individual(type = "death",
#'                 name = "my_death_event",
#'                 intensity_code = "result = death_rate(age(I,t));")
#'
#'birth_event <- mk_event_individual(type = "birth",
#'                 intensity_code = "if (I.male) result = 0;
#'                                   else result = birth_rate(age(I,t));",
#'                 kernel_code = "newI.male = CUnif(0, 1) < p_male;")
#'          
#'@seealso \code{\link{mk_model}}, \code{\link{mk_event_poisson}}, \code{\link{mk_event_inhomogeneous_poisson}}, and \code{\link{mk_event_interaction}}.        
#'
#' @export
mk_event_individual <- function(type, name, intensity_code, kernel_code='') {
    assertChoice(type, c('birth', 'death', 'entry', 'exit', 'swap', 'custom'))
    check_intensity_code(intensity_code)
    check_kernel_code(kernel_code)
    if (missing(name)) name = type
    assertCharacter(name, null.ok = TRUE, len = 1, pattern = "^[a-zA-Z0-9_]*$")

    event = list("name" = name,
                 "type" = c("individual", type),
                 "intensity_code" = intensity_code,
                 "kernel_code" = kernel_code)

    event$cpp_code <- mkcpp_event(event, type, "individual")
    class(event) <- "event"
    return(event)
}


#' Creates an event with intensity of type interaction.
#'
#' @description Creates an event whose intensity depends on an individual and interactions with the population. When the event occurs, something happens to an individual \code{I} in the population. The intensity of the event can depend on time, the characteristics of I and other individuals in the population, and can be written as
#' \deqn{d(I,t,pop) = \sum_{J \in pop} U(I,J,t),}
#' where \eqn{U} is called the interaction function.
#' The created event must be used with \code{\link{mk_model}}. 
#'
#' @inheritParams mk_event_inhomogeneous_poisson
#' @param interaction_code String containing some C++ code describing the interaction function. See details.
#' @param interaction_type _(Optional)_ Either \code{'random'} or \code{'full'}. By default \code{'random'} which is faster than \code{'full'}.
#'
#' @eval details_type_event()
#' @eval details_interaction_code()
#' @eval details_kernel_code()
#' @return An S3 object of class \code{event} of type interaction.
#'
#'
#'@examples 
#'
#'death_interaction_code<- " result = max(J.size -I.size,0);"
#'event <- mk_event_interaction(type="death", 
#'                              interaction_code = death_interaction_code)
#'
#'@seealso \code{\link{mk_model}}, \code{\link{mk_event_poisson}}, \code{\link{mk_event_inhomogeneous_poisson}}, \code{\link{mk_event_individual}}.
#'
#' @export
mk_event_interaction <- function(type, name, interaction_code, kernel_code='',
                                 interaction_type = 'random') {
    assertChoice(type, c('birth', 'death', 'entry', 'exit', 'swap', 'custom'))
    assertChoice(interaction_type, c('random', 'full'))
    check_interaction_code(interaction_code)
    check_kernel_code(kernel_code)
    if (missing(name)) name = type
    assertCharacter(name, null.ok = TRUE, len = 1, pattern = "^[a-zA-Z0-9_]*$")

    event = list("name" = name,
                 "type" = c("interaction", type),
                 "intensity_type" = interaction_type,
                 "intensity_code" = interaction_code,
                 "kernel_code" = kernel_code)

    event$cpp_code <- mkcpp_event(event, type, paste0(interaction_type, "_interaction"))
    class(event) <- "event"
    return(event)
}

#' Summary of an event.
#'
#' @param object Argument of class \code{event}.
#' @param ... Additional arguments affecting the summary produced.
#'
#' @export
summary.event <- function(object, ...) {
    stopifnot(inherits(object, "event"))

    cat("\t\n", sep="",
        sprintf("%s, type: %s", object$type[1], object$type[2]),
        if (is.null(object$name)) sprintf("\n") else sprintf(", name: %s\n", object$name)
    )
}

check_intensity_code <- function(code) {
    if (!grepl("result", code)) {
        stop("The string argument 'intensity_code' must contain keyword 'result'")
    }
}

check_interaction_code <- function(code) {
  if (!grepl("result", code)) {
    stop("The string argument 'interaction_code' must contain keyword 'result'")
  }
    # to do
}

check_kernel_code <- function(code) {
    # to do
}


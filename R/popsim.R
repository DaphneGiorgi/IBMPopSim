#' Simulation of a model.
#'
#' @description This function simulates the random evolution of an IBM.
#'
#' @param model Model resulting from a call to the function \code{\link{mk_model}}.
#' @param initial_population Object of \link{population} class representing the initial population.
#' @param events_bounds Named vector of events bounds, with names corresponding to events names.
#' @param parameters List of model parameters.
#' @param age_max Maximum age of individuals in the population (\code{Inf} by default).
#' @param time Final time (Numeric). Can be of length 1 or a vector of simulation discretized times.
#' @param multithreading Logical for multithread activation, \code{FALSE} by default. Should be only activated for IBM simulation with no interactions.
#' @param num_threads _(Optional)_ Number of threads used for multithreading. Set by default to the number of concurrent threads supported by the available hardware implementation.
#' @param clean_step _(Optional)_ Optional parameter for improving simulation time. Time step for removing dead (or exited) individuals from the population. By default, equal to age_max.
#' @param clean_ratio _(Optional)_ Optional parameter for improving simulation time. 0.1 by default.
#' @param seed _(Optional)_ Random generator seed, random by default.
#' @param verbose _(Optional)_ Activate verbose output, FALSE by default.
#'
#' @seealso \code{\link{mk_model}}.
#'
#' @return List composed of \describe{
#'   \item{arguments}{Simulation inputs (initial population, parameters value, multithreading...)}
#'   \item{logs}{Simulation logs (algorithm duration, accepted/rejected events...).}
#'   \item{population}{If \code{time} is of length 1, population is an object of type \link{population} containing of all individuals who lived in the population in the time interval \code{[0,time]}. If \code{time} is a vector (\code{time[1], ..., time[n]}), \code{population} is a list of \code{n} objects of type \link{population}, each representing the state of the population at time \code{time[i]}, for \code{i = 1,\ldots, n}.}
#' }
#'
#'@examples
#'\donttest{
#'init_size <- 100000
#'pop_df <- data.frame(birth = rep(0, init_size), death = NA)
#'pop <- population(pop_df)
#'
#'birth = mk_event_poisson(type = 'birth', intensity = 'lambda')
#'death = mk_event_poisson(type = 'death', intensity = 'mu')
#'params = list('lambda' = 100, 'mu' = 100)

#'birth_death <- mk_model(events = list(birth, death),
#'                        parameters = params)
#'
#'sim_out <- popsim(model = birth_death,
#'                  initial_population = pop,
#'                  events_bounds = c('birth' = params$lambda, 'death' = params$mu),
#'                  parameters = params,
#'                  time = 10)
#'                        }
#' @export
popsim <- function(model, initial_population, events_bounds, parameters=NULL,
                   age_max=Inf, time,
                   multithreading=FALSE, num_threads=NULL,
                   clean_step=NULL, clean_ratio=0.1,
                   seed=NULL, verbose=FALSE) {
    assertNumeric(events_bounds, lower = 0, finite = TRUE,
                  null.ok = FALSE, any.missing = FALSE, names = 'unique')
    name_events <- sapply(model$events, function(e) { e$name })
    assertNames(names(events_bounds), type = "unique",
                permutation.of = name_events)
    # reordering events_bounds if needed
    events_bounds <- events_bounds[name_events]
    assertNumber(age_max, lower = 0)
    assertNumeric(time, lower = 0, finite = TRUE, sorted = TRUE,
                  null.ok = FALSE, any.missing = FALSE)
    assertNumber(clean_step, lower = 0, null.ok = TRUE)
    assertNumber(clean_ratio, lower = 1e-3, upper = 1)
    assertFlag(multithreading)
    assertCount(num_threads, null.ok = TRUE, positive = TRUE)
    assertCount(seed, null.ok = TRUE)
    assertFlag(verbose)

    ## check that initial_population inherits from 'population'
    if (!inherits(initial_population, "population"))
      stop("'initial_population' has to be an object of 'population' class. Try to call 'initial_population <- population(initial_population)' first.")

    arguments = list("population" = initial_population, "events_bounds" = events_bounds, "parameters" = parameters,
                     "age_max" = age_max, "time" = time, "multithreading" = multithreading,
                     "num_threads" = num_threads, "clean_step" = clean_step, "clean_ratio" = clean_ratio,
                     "seed" = seed, "verbose"=verbose)

    ## check parameters argument: to be completed
    if (!all(model$parameters_types$names %in% names(parameters))) {
        stop("The 'parameters' argument must be compatible with the model.")
    }

    ## check population and model compatibility
    compatibility_pop_model(initial_population, model)

    ## about events_bounds
    for (k in 1:length(events_bounds)){
        if (events_bounds[[k]] == 0) {
            print(paste('event', model$events[[k]]$name, 'is deactivated'))
        } else {
            if ('poisson' %in% model$events[[k]]$type) {
                int_value_k = with(model$events[[k]],
                                 ifelse(is.na(suppressWarnings(as.numeric(intensity_code))), parameters[[intensity_code]], as.numeric(intensity_code)))
                if (int_value_k != events_bounds[[k]]) {
                    warning(paste("set 'events_bounds' at", int_value_k, "for Poisson event", model$events[[k]]$name))
                }
            }
        }
    }
    arguments$events_bounds <- events_bounds
    if (is.null(arguments$parameters)) arguments$parameters = list()
    if (is.null(arguments$age_max)) arguments$age_max = age_max
    if (is.null(arguments$clean_step)) arguments$clean_step = age_max
    if (is.null(arguments$clean_ratio)) arguments$clean_ratio = clean_ratio
    if (is.null(arguments$multithreading)) arguments$multithreading = multithreading
    if (is.null(arguments$num_threads)) arguments$num_threads = NA

    arguments$seed <- ifelse(is.null(arguments$seed),
                             as.integer(runif(1,0,2**31)), seed)
    set.seed(arguments$seed)

    result = list()

    arguments$time = NULL
    names(arguments)[1] = 'pop_df'
    if (verbose){
      cat("Simulation on ")
    }

    if (length(time) <= 2) {
        if (length(time) == 1) {
            arguments[c("T0", "T")] <- c(0, time)
        } else {
            arguments[c("T0", "T")] <- c(time[1], time[2])
        }
        if (verbose){
          cat(paste0(" [", arguments$T0, ", ", arguments$T, "]\n"))
        }
        arguments$seed <- as.integer(runif(1,0,2**31))
        out <- do.call(model$popsim_cpp, arguments)
        result$logs = out$logs
        class(result$logs) <- c("logs", "numeric")

        result$population <- population(id_complete(out$population))

        class(result) <- c("simulation_output", "list")
        return(result)
    } else {
        logs = list()
        pops = list()
        pop = arguments$pop_df
        for (k in 1:(length(time)-1)) {
            arguments[c("T0", "T")] <- c(time[k], time[k+1])
            if (verbose){
              cat(paste0(" [", arguments$T0, ", ", arguments$T, "] "))
            }
            arguments$seed <- as.integer(runif(1,0,2**31))
            out = do.call(model$popsim_cpp, arguments)
            pop = id_complete(out$population)

            pop <- population(pop)
            class(logs) <- c("logs", "numeric")

            logs[[k]] = out$logs
            pops[[k]] = pop[, colnames(initial_population)]
            arguments$pop_df <- pop
        }
        result$logs <- Reduce(`+`, logs)
        result$population <- pops
        class(result) <- c("simulation_output", "list")
        return(result)
    }
}


#' Complete a population id
#'
#' @description In a population with an \code{id} column, verifies that there are no NA \code{id} values,
#' and if there are, completes the missing \code{id}s
#'
#' @param object A data frame containing at least a column \code{birth} and \code{death}, that will be converted in \link{population} class.
#'
#' @keywords internal
id_complete <- function(object, ...) {
    if (!is.null(object$id)) {
        xx = is.na(object$id)
        n_to_add = sum(xx)
        if (n_to_add == 0) return(object)
        min_id = max(object$id, na.rm = TRUE)
        object[xx,]$id = seq(min_id+1, min_id+n_to_add)
    }
    return(object)
}

#' Simulation of a model.
#'
#' @description This function simulates the random evolution of an IBM.
#'
#' @param model Model resulting from a call to the function \code{\link{mk_model}}.
#' @param population Data frame representing the initial population.
#' @param events_bounds Named vector of events bounds, with names corresponding to events names.
#' @param parameters List of model parameters.
#' @param age_max Maximum age of individuals in the population (\code{Inf} by default).
#' @param time Final time (Numeric). If \code{time} is a vector or vector of simulation discretized times.
#' @param multithreading Logical for multithread activation, \code{FALSE} by default. Should be only activated for IBM simulation with no interactions.
#' @param num_threads _(Optional)_ Number of threads used for multithreading. Set by default to the number of concurrent threads supported by the available hardware implementation.
#' @param clean_step _(Optional)_ Optional parameter for improving simulation time. Time step for removing dead (or exited) individuals from the population. By default, equal to age_max.
#' @param clean_ratio _(Optional)_ Optional parameter for improving simulation time. 0.1 by default.
#' @param seed _(Optional)_ Random generator seed, random by default.
#'
#' @seealso \code{\link{mk_model}}.
#'
#' @return  popsim returns a list of composed of \describe{
#'   \item{arguments}{Simulation inputs (initial population, parameters value, multithreading...)}
#'   \item{logs}{Simulation logs (algorithm duration, accepted/rejected events...).}
#'   \item{population}{If \code{time} is of length 1, population is a data frame composed of all individuals who lived in the population of \code{[0,time]}. If \code{time} is a vector, \code{population} is a list of population data frames.}
#' }
#'
#'@examples
#'\dontrun{
#'init_size <- 100000
#'pop <- data.frame(birth = rep(0, init_size), death = NA)
#'
#'birth = mk_event_poisson(type = 'birth', intensity = 'lambda')
#'death = mk_event_poisson(type = 'death', intensity = 'mu')
#'params = list('lambda' = 100, 'mu' = 100)

#'birth_death <- mk_model(events = list(birth, death),
#'                        parameters = params)
#'                        
#'sim_out <- popsim(model = birth_death, 
#'                  population = pop, 
#'                  events_bounds = c('birth' = params$lambda, 'death' = params$mu),
#'                  parameters = params, 
#'                  time = 10)
#'                        }
#' @export
popsim <- function(model, population, events_bounds, parameters=NULL,
                   age_max=Inf, time,
                   multithreading=FALSE, num_threads=NULL,
                   clean_step=NULL, clean_ratio=0.1,
                   seed=NULL) {
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

    arguments = as.list(match.call(expand.dots = TRUE)[-c(1,2)])

    ## check parameters argument: to be completed
    if (!all(model$parameters_types$names %in% names(parameters))) {
        stop("The 'parameters' argument must be compatible with the model.")
    }

    ## check argument population
    characts = setdiff(model$individual_type$names, c('id', 'entry', 'out'))
    if (!all(characts %in% names(population))) {
        stop("The 'population' argument must be compatible with the model and the characteristics name.")
    }
    if ('id' %in% model$individual_type$names) {
        if (!'id' %in% names(population)) {
            print("Add 'id' attributes to the population.")
            population = cbind("id" = seq(1, nrow(population)), population)
            arguments$population = population
        }
    }
    if ('entry' %in% model$individual_type$names) {
        if (!'entry' %in% names(population)) {
            warning("Add 'entry' attributes to the population.")
            population = cbind(population, "entry" = NA)
            arguments$population = population
        }
    }
    if ('out' %in% model$individual_type$names) {
        if (!'out' %in% names(population)) {
            warning("Add 'out' attributes to the population.")
            population = cbind(population, "out" = FALSE)
            arguments$population = population
        }
    }

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
    arguments$verbose = FALSE
    arguments$seed <- ifelse(is.null(arguments$seed),
                             as.integer(runif(1,0,2**31)), seed)
    set.seed(arguments$seed)

    result = list("arguments" = sapply(arguments, eval))

    arguments$time = NULL
    names(arguments)[1] = 'pop_df'
    cat("Simulation on ")
    if (length(time) <= 2) {
        if (length(time) == 1) {
            arguments[c("T0", "T")] <- c(0, time)
        } else {
            arguments[c("T0", "T")] <- c(time[1], time[2])
        }
        cat(paste0(" [", arguments$T0, ", ", arguments$T, "] "))
        arguments$seed <- as.integer(runif(1,0,2**31))
        out <- do.call(model$popsim_cpp, arguments)
        result$logs = out$logs
        result$population = id_complete(out$population)
        return(result)
    } else {
        logs = list()
        pops = list()
        pop = arguments$pop_df
        for (k in 1:(length(time)-1)) {
            arguments[c("T0", "T")] <- c(time[k], time[k+1])
            cat(paste0(" [", arguments$T0, ", ", arguments$T, "] "))
            arguments$seed <- as.integer(runif(1,0,2**31))
            out = do.call(model$popsim_cpp, arguments)
            pop = id_complete(out$population)
            logs[[k]] = out$logs
            pops[[k]] = pop[, colnames(population)]
            arguments$pop_df <- pop
        }
        result$logs <- Reduce(`+`, logs)
        result$population <- pops
        return(result)
    }
}

id_complete <- function(population) {
    if (!is.null(population$id)) {
        xx = is.na(population$id)
        n_to_add = sum(xx)
        if (n_to_add == 0) return(population)
        min_id = max(population$id, na.rm = TRUE)
        population[xx,]$id = seq(min_id+1, min_id+n_to_add)
    }
    return(population)
}

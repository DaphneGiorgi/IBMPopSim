#' Simulation of a model
#'
#' @param model Model resulting from a call to the function `mk_model`
#' @param population Data frame representing the initial population
#' @param events_bounds Vector of bounds for the events (`numeric` or `list`)
#' @param parameters R parameters and functions that we want to be available in the C++ code
#' @param age_max Age max of the population (by default 130)
#' @param times vector of simulating discretised times
#' @param clean_step Clean step for cleaning population from dead (or exit) individuals, by default equal to population age max
#' @param clean_ratio Ratio of dead (or exit) individuals in the population for cleaning population from dead (or exit) individuals, 0.1 by default
#' @param multithreading Boolean for multithread activation, FALSE by default
#' @param num_threads Number of threads used for multithreading, NA by default
#' @param seed Random generator seed, random by default
#' @param verbose verbose, FALSE by default
#'
#' @export
popsim <- function(model, population, events_bounds, parameters=NULL,
                   age_max=Inf, times=c(0, 100),
                   clean_step=NULL, clean_ratio=0.1,
                   multithreading=FALSE, num_threads=NULL, seed=NULL,
                   verbose=FALSE) {
    args = as.list(match.call(expand.dots = TRUE)[-c(1,2)])
    if (!identical(names(population), model$individual_type$names)) {
        if (setdiff(model$individual_type$names, names(population)) == 'id') {
            warning("add 'id' attributes to the population")
            population = cbind("id" = seq(1, nrow(population)), population)
            args$population = population
        } else {
            stop("argument 'population' must be compatible with the model")
        }
    }
    if (!identical(names(parameters), model$parameters_types$names)) {
        stop("argument 'parameters' must be compatible with the model")
    }
    if (!is.numeric(times) || length(times) < 2) {
        stop("argument 'times' must be numeric of length >= 2")
    }
    if (is.null(args$parameters)) args$parameters = list()
    if (is.null(args$age_max)) args$age_max = age_max
    if (is.null(args$clean_step)) args$clean_step = args$age_max
    if (is.null(args$clean_ratio)) args$clean_ratio = clean_ratio
    if (is.null(args$seed)) args$seed = as.integer(runif(1,0,2**31))
    if (is.null(args$multithreading)) args$multithreading = FALSE
    if (is.null(args$num_threads)) args$num_threads = NA
    args$times = NULL
    names(args)[1] = 'pop_df'
    if (length(times) == 2) {
        args$T0 = times[1]
        args$T = times[-1]
        result <- do.call(model$popsim_cpp, args)
        result$population = id_complete(result$population)
        if (!verbose) return(result$population)
        else return(result)
    } else {
        result = list()
        result$populations = list(population)
        result$logs = list(info = "initial population")
        pop_init = population
        for (k in 1:(length(times)-1)) {
            args$T0 = times[k]
            args$T = times[k+1]
            args$pop_df = pop_init
            sim_out = do.call(model$popsim_cpp, args)
            pop_out = id_complete(sim_out$population)
            result$populations[[k]] = pop_out[, colnames(population)]
            result$logs[[k]] = sim_out$logs
            pop_init = pop_out
        }
        if (!verbose) return(result$populations)
        else return(result)
    }
}

id_complete <- function(population) {
    if (!is.null(population$id)) {
        min_id = max(population$id, na.rm = TRUE)
        xx = is.na(population$id)
        population[xx,]$id = seq(min_id+1, min_id+sum(xx))
    }
    return(population)
}


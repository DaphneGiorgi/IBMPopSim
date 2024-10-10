#' Function for internal use
#' @description Build source code for individual creation.
#' @keywords internal
mkcpp_declaration_individual <- function(ind_t) {
    file <- system.file("include", "individual_template.cpp", package="IBMPopSim")
    code <- read_file(file)

    code_decl <- '_TYPE_ _NAME_, '
    code_def <- '_NAME_(_NAME_), '

    code_declare_caract <- ''
    code_define_caract <- ''
    if (length(ind_t$names) >= 3) {
        for (j in 3:length(ind_t$names)) {
            code_decl_j <- sub("_TYPE_", ind_t$typesC[j], code_decl)
            code_decl_j <- sub("_NAME_", ind_t$names[j], code_decl_j)
            code_def_j <- gsub("_NAME_", ind_t$names[j], code_def)

            code_declare_caract <- paste0(code_declare_caract, code_decl_j)
            code_define_caract <- paste0(code_define_caract, code_def_j)
        }
    }
    code <- sub("_DECLARE_CARACTERISTICS_", code_declare_caract, code)
    code <- sub("_DEFINE_CARACTERISTICS_", code_define_caract, code)
    code_caract <- gsub(", ", ";\n\t", code_declare_caract)
    code <- sub("_CARACTERISTICS_", code_caract, code)

    return(code)
}

#' Function for internal use
#' @description Capitalize first letter of a word
#' @keywords internal
capitalize <- function(x) {
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    return(x)
}

#' Function for internal use
#' @description Build source code for population initialization.
#' @keywords internal
mkcpp_initialisation_population <- function(ind_t) {
    code <- paste0('Rcpp::DoubleVector _birth_date = pop_df["', ind_t$names[1], '"];\n\t')
    code <- paste0(code, 'Rcpp::DoubleVector _death_date = pop_df["', ind_t$names[2], '"];\n\t')
    if (length(ind_t$names) >= 3) {
        for (j in 3:length(ind_t$names)) {
            code_j <- paste0('Rcpp::', capitalize(ind_t$typesR[j]), 'Vector _', ind_t$names[j], ' = pop_df["', ind_t$names[j], '"];\n\t')
            code <- paste0(code, code_j)
        }
    }
    code <- paste0(code, 'for (unsigned _k = 0; _k < pop_df.nrow(); ++_k)\n\t    ')
    code <- paste0(code, 'pop.add_init(individual(')
    code_individual <- '_birth_date[_k]'
    if (length(ind_t$names) >= 3) {
        for (j in 3:length(ind_t$names)) {
            code_individual <- paste0(code_individual, ', _', ind_t$names[j], '[_k]')
        }
    }
    code <- paste0(code, code_individual, ", _death_date[_k]));\n\t")
    return(code)
}

#' Function for internal use
#' @description Build source code for output population creation.
#' @keywords internal
mkcpp_output_population <- function(ind_t) {
    code_decl <- paste0('if (verbose) std::cout << "Before output conversion " << std::endl;\n\t')
    code_decl <- paste0(code_decl,'std::vector<double> _birth_date_out;\n\t')
    code_decl <- paste0(code_decl,'_birth_date_out.reserve(pop.individuals.size()+pop.dead_individuals.size());\n\t')
    code_decl <- paste0(code_decl, 'std::vector<double> _death_date_out;\n\t')
    code_decl <- paste0(code_decl,'_death_date_out.reserve(pop.individuals.size()+pop.dead_individuals.size());\n\t')
    if (length(ind_t$names) >= 3) {
        for (j in 3:length(ind_t$names)) {
            code_j <- paste0('std::vector<', ind_t$typesC[j], '> _', ind_t$names[j], '_out;\n\t')
            code_j <- paste0(code_j, '_', ind_t$names[j], '_out.reserve(pop.individuals.size()+pop.dead_individuals.size());\n\t')
            code_decl <- paste0(code_decl, code_j)
        }
    }
    code_for_indiv <- 'if (verbose) std::cout << "individuals size : " << pop.individuals.size() << std::endl;\n\t'
    code_for_indiv <- paste0(code_for_indiv, 'for (unsigned _k = 0; _k < pop.individuals.size(); ++_k) {\n\t    ')
    code_for_indiv <- paste0(code_for_indiv, '_birth_date_out.push_back( pop.individuals[_k].birth_date );\n\t    ')
    code_for_indiv <- paste0(code_for_indiv, '_death_date_out.push_back( pop.individuals[_k].death_date );\n\t    ')
    if (length(ind_t$names) >= 3) {
        for (j in 3:length(ind_t$names)) {
            code_for_indiv <- paste0(code_for_indiv, '_', ind_t$names[j] ,'_out.push_back( pop.individuals[_k].', ind_t$names[j], ' );\n\t    ')
        }
    }
    code_for_indiv <- paste0(code_for_indiv, '}\n\t')
    code_for_dead <- gsub('individuals', 'dead_individuals', code_for_indiv)

    code_df <- 'Rcpp::DataFrame new_pop_df = Rcpp::DataFrame::create(\n\t    Rcpp::Named("_NAME1_") = _birth_date_out,\n\t    Rcpp::Named("_NAME2_") = _death_date_out'
    code_df <- sub('_NAME1_', ind_t$names[1], code_df)
    code_df <- sub('_NAME2_', ind_t$names[2], code_df)
    temp <- ',\n\t    Rcpp::Named("_NAME_") = __NAME__out'
    if (length(ind_t$names) >= 3) {
        for (j in 3:length(ind_t$names)) {
            code_df <- paste0(code_df, gsub('_NAME_', ind_t$names[j], temp))
        }
    }
    code_df <- paste0(code_df, ');\n\t')
    code_df <- paste0(code_df, 'if (verbose) std::cout << "After output conversion " << std::endl;\n\t')

    code <- paste0(code_decl, code_for_indiv, code_for_dead, code_df)
    return(code)
}

#' Function for internal use
#' @description Build source code for parameters creation.
#' @keywords internal
mkcpp_parameters <- function(parameters, parameters_t) {
    decl_code <- ''
    defn_code <- ''

    for (j in 1:length(parameters_t$names)) {
        if (parameters_t$names[j] != "intensity_bound" && parameters_t$names[j] != "age_max"){

            Ctype <- parameters_t$typesC[j]
            if (Ctype %in% c('function_x', 'function_xy')) {
                decl_code_j <- '\t_TYPE_ _NAME_'
                defn_code_j <- 'init__TYPE_(Rcpp::as<Rcpp::Function>(parameters["_NAME_"]))'
                decl_code_j <- gsub("_TYPE_", Ctype, decl_code_j)
                defn_code_j <- gsub("_TYPE_", Ctype, defn_code_j)
            }
            else if (Ctype %in% c('list_of_function_x', 'list_of_function_xy', 'list_of_function')) {
                size_j <- parameters_t$lengths[j] - 1
                decl_code_j <- '\tstd::vector<_TYPE_> _NAME_'
                defn_code_j <- '{'
                for (k in 0:size_j) {
                    defn_code_j <- paste0(defn_code_j, 'init__TYPE_(Rcpp::as<Rcpp::Function>(Rcpp::as<Rcpp::List>(parameters["_NAME_"])[_k_]))')
                    defn_code_j <- gsub("_k_", k, defn_code_j)
                    if (k < size_j)
                        defn_code_j <- paste0(defn_code_j, ', ')
                }
                defn_code_j <- paste0(defn_code_j, '}')
                decl_code_j <- gsub("_TYPE_", gsub('list_of_', '', 'list_of_function_x'), decl_code_j)
                defn_code_j <- gsub("_TYPE_", gsub('list_of_', '', 'list_of_function_x'), defn_code_j)
            }
            else {
                decl_code_j <- '\t_TYPE_ _NAME_'
                defn_code_j <- 'Rcpp::as<_TYPE_>(parameters["_NAME_"])'
                decl_code_j <- gsub("_TYPE_", Ctype, decl_code_j)
                defn_code_j <- gsub("_TYPE_", Ctype, defn_code_j)
            }
            decl_code_j <- paste0(decl_code_j, ';\n#define _NAME_ cntxt._NAME_')
            decl_code_j <- gsub("_NAME_", parameters_t$names[j], decl_code_j)
            defn_code_j <- gsub("_NAME_", parameters_t$names[j], defn_code_j)

            decl_code <- paste0(decl_code, decl_code_j, "\n")
            if (j < length(parameters_t$names))
                defn_code <- paste0(defn_code, defn_code_j, ",\n\t\t")
            else
                defn_code <- paste0(defn_code, defn_code_j, "\n\t\t")
        }
        else
            stop("Parameters name can't be 'intensity_bound' or 'age_max'.")
    }
    return(list("decl_code" = decl_code, "defn_code" = defn_code))
}

#' Function for internal use
#' @description Build source code for event creation.
#' @keywords internal
mkcpp_event <- function(event, kernel_type, name_type) {
    event_file <- system.file("include",
                              paste0("event_", name_type, "_template.cpp"),
                              package="IBMPopSim")
    cpp_code <- read_file(event_file)
    kernel_method <- switch(kernel_type,
        "birth" = "individual newI = I;\n\tnewI.birth_date = t;\n\t_CORRECTION_IF_ENTRY_\n\t_KERNEL_CODE_\n\tpop.add(newI);",
        "death" = "_KERNEL_CODE_\n\tpop.kill(_k, t);",
        "entry" = "individual newI;\n\t_KERNEL_CODE_\n\tnewI.entry = t;\n\t_CORRECTION_IF_EXIT_\n\tpop.add(newI);",
        "swap" = "_KERNEL_CODE_",
        "exit" = "_KERNEL_CODE_\n\tI.out = true;\n\tpop.kill(_k, t);\n",
        "custom" = "_KERNEL_CODE_")
    cpp_code <- gsub("_KERNEL_METHOD_", kernel_method, cpp_code)
    cpp_code <- gsub("_KERNEL_CODE_", event$kernel_code, cpp_code)
    if (!is.null(event$intensity_code))
        cpp_code <- gsub("_INTENSITY_CODE_", event$intensity_code, cpp_code)
    if (!is.null(event$name))
        cpp_code <- gsub("_NAME_", event$name, cpp_code)
    if (name_type %in% c("poisson", "inhomogeneous_poisson")) {
        if (kernel_type == "entry")
            cpp_code <- gsub("_PICK_IFNOT_ENTRY_", "0", cpp_code)
        else
            cpp_code <- gsub("_PICK_IFNOT_ENTRY_", "pop.pick_alive(t, cntxt)", cpp_code)
    }
    return(cpp_code)
}

#' Function for internal use
#' @description Build source code for event definition.
#' @keywords internal
mkcpp_definition_events <- function(events) {
    Nevents <- length(events)
    code <- sub("_N_", Nevents, 'constexpr int NEVENTS = _N_;\n')
    for (j in 1:Nevents) {
        code <- paste0(code, events[[j]]$cpp_code)
        code <- gsub("_J_", j-1, code)
    }
    code <- paste0(code, 'std::array<event*, NEVENTS> events { ')
    for (j in 1:Nevents) {
        code <- paste0(code, 'new event', j-1)
        if (j < Nevents)
            code <- paste0(code, ', ')
    }
    for (j in 1:Nevents){
        if ("entry" %in% events[[j]]$type) {
            code <- gsub("_CORRECTION_IF_ENTRY_", "newI.entry = NA_REAL;", code)
        }
    }
    code <- gsub("_CORRECTION_IF_ENTRY_", "", code)

    for (j in 1:Nevents){
      if ("exit" %in% events[[j]]$type) {
        code <- gsub("_CORRECTION_IF_EXIT_", "newI.out = false;", code)
      }
    }
    code <- gsub("_CORRECTION_IF_EXIT_", "", code)

    code <- paste0(code, ' };')
    return(code)
}

#' Function for internal use
#' @description Build source main code for model simulation.
#' @keywords internal
mkcpp_popsim <- function(model, with_id) {
    file <- system.file("include", "popsim_template.cpp", package="IBMPopSim")
    code <- read_file(file)
    if (with_id) {
        code <- paste("#define WITH_ID\n", code)
    }
    code <- sub("_DECLARATION_INDIVIDUAL_", mkcpp_declaration_individual(model$individual_type), code)

    Nevents <- length(model$events)
    for (j in 1:Nevents){
        if ("entry" %in% model$events[[j]]$type) {
            code <- gsub("_NO_ENTRY_", "false", code) 
        }
    }
    code <- gsub("_NO_ENTRY_", "true", code) 

    if (!is.null(model$parameters)) {
        params <- mkcpp_parameters(model$parameters, model$parameters_type)
        code <- sub("_DECLARATION_PARAMETERS_", params$decl_code, code)
    } else {
        code <- sub("_DECLARATION_PARAMETERS_", "", code)
    }
    code <- sub("_DEFINITION_EVENTS_", mkcpp_definition_events(model$events), code)
    code <- sub("_INITIALISATION_POPULATION_", mkcpp_initialisation_population(model$individual_type), code)
    if (!is.null(model$parameters)) {
        code <- sub("_DEFINITION_PARAMETERS_", params$defn_code, code)
    } else {
        code <- sub("_DEFINITION_PARAMETERS_", "", code)
    }
    code <- sub("_OUTPUT_POPULATION_", mkcpp_output_population(model$individual_type), code)
    return(code)
}

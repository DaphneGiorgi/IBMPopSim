// -*- coding: utf-8 -*-
//[[Rcpp::depends(IBMPopSim)]]
//[[Rcpp::depends(RcppArmadillo)]]
//[[Rcpp::plugins(cpp11)]]
#include <RcppArmadillo.h>
#include <cmath>
#include <limits>
#include <array>
#include <vector>
#include <string>
#include <thread>
#include <chrono>

#include "functions.hpp"

// ***** DECLARATION_INDIVIDUAL
_DECLARATION_INDIVIDUAL_
// *****

// ***** DECLARATION_PARAMS MK_CONTEXT
struct context {
    std::mt19937_64 __gen;
    int __num_threads;
    std::vector<double> __intensity_bound;
_DECLARATION_PARAMETERS_

    template<typename ... Args>
    double Unif(Args&&... arg) { return std::uniform_real_distribution<double>(std::forward<Args>(arg)...)(__gen); }
#define CUnif cntxt.Unif
    template<typename ... Args>
    double Exp(Args&&... arg) { return std::exponential_distribution<double>(std::forward<Args>(arg)...)(__gen); }
#define CExp  cntxt.Exp
    template<typename ... Args>
    double Norm(Args&&... arg) { return std::normal_distribution<double>(std::forward<Args>(arg)...)(__gen); }
#define CNorm cntxt.Norm
    template<typename ... Args>
    unsigned Poisson(Args&&... arg) { return std::poisson_distribution<>(std::forward<Args>(arg)...)(__gen); }
#define CPoisson cntxt.Poisson
    template<typename ... Args>
    double Gamma(Args&&... arg) { return std::gamma_distribution<double>(std::forward<Args>(arg)...)(__gen); }
#define CGamma cntxt.Gamma
    template<typename ... Args>
    double Weibull(Args&&... arg) { return std::weibull_distribution<double>(std::forward<Args>(arg)...)(__gen); }
#define CWeibull cntxt.Weibull
    template<typename ... Args>
    int UnifInt(Args&&... arg) { return std::uniform_int_distribution<int>(std::forward<Args>(arg)...)(__gen); }
#define CUnifInt  cntxt.UnifInt
    template<typename ... Args>
    unsigned Discrete(Args&&... arg) { return std::discrete_distribution<int>(std::forward<Args>(arg)...)(__gen); }
#define CDiscrete  cntxt.Discrete
    template<typename ... Args>
    bool Bernoulli(Args&&... arg) { return std::bernoulli_distribution(std::forward<Args>(arg)...)(__gen); }
#define CBern  cntxt.Bernoulli
    template<typename ... Args>
    unsigned Binomial(Args&&... arg) { return std::binomial_distribution<int>(std::forward<Args>(arg)...)(__gen); }
#define CBinom  cntxt.Binomial
};
// *****

#include "population.hpp"

// ***** DEFINITION_EVENTS
struct event {
    virtual double __lambda(unsigned k, population const & pop, double t, context & cntxt) const = 0;
    virtual double __lambda_bound(population const & pop, context & cntxt) const = 0;
    virtual void __kernel(unsigned k, population & pop, double t, context & cntxt) const = 0;
    virtual bool __apply(double t, population & pop, context & cntxt) const = 0;
};

_DEFINITION_EVENTS_
// *****

#include "popsim_algorithm.hpp"

using namespace Rcpp;

//' @param pop_df Dataframe representing the initial population
//' @param events_bounds NumericVector containing the events bounds
//' @param parameters R parameters and functions available in C++ code
//' @param age_max Age max of the population
//' @param T0 Initial time
//' @param T Final time
//' @param clean_step Clean step for cleaning population from dead (or exit) individuals, by default equal to population age max
//' @param clean_ratio Ratio of dead (or exit) individuals in the population for cleaning population from dead (or exit) individuals
//' @param seed Seed for the random number generator
//' @param multithreading Boolean for multithread activation, by default multithread is deactivated
//' @param num_threads Number of threads for multithread run, if multihtread is activated the default value is the maximum of threads
//' @param verbose verbose by default false
//'
//' @export
// [[Rcpp::export]]
Rcpp::List popsim_cpp(Rcpp::DataFrame pop_df,
        Rcpp::NumericVector events_bounds,
        Rcpp::List parameters,
        double age_max, double T0, double T,
        double clean_step, double clean_ratio,
        int seed,
        bool multithreading, int num_threads = NA_INTEGER,
        bool verbose = false)
{
    std::vector<double> intensity_bound = Rcpp::as<std::vector<double>>(events_bounds);
    population pop(age_max);

    if (verbose)
        Rcpp::Rcout << "Simulation on the interval [" << T0 << ", " << T << "] " << std::endl;
    if (multithreading) {
#ifdef __INTERACTION
        Rcpp::Rcout << "Warning: Multithreading should be deactivated..." << std::endl;
#endif
        if (num_threads == NA_INTEGER) {
            num_threads = std::thread::hardware_concurrency();
        }
        if (verbose) Rcpp::Rcout << "Multithreading activated with " << num_threads << " threads" << std::endl;
    } else {
        if (verbose) Rcpp::Rcout << "Multithreading deactivated" << std::endl;
        num_threads = 1;
    }

    if (verbose) Rcpp::Rcout << "Seed: " << seed << std::endl;
    std::vector<uint32_t> seeds(2*num_threads);
    std::seed_seq seq { seed };
    seq.generate(seeds.begin(), seeds.end());

    auto start = std::chrono::system_clock::now();

    // ***** INITIALISATION_POPULATION
    _INITIALISATION_POPULATION_
    // *****

    // ***** DEFINITION_PARAMETERS
    context cntxt {
        std::mt19937_64(),
        num_threads,
        intensity_bound,
        _DEFINITION_PARAMETERS_
    };
    // *****

    auto end = std::chrono::system_clock::now();

    std::vector<std::thread> threads(num_threads);
    std::vector<counter> counters(num_threads);
    std::vector<population> pops = split(pop, num_threads);

    std::chrono::duration<double> elapsed_seconds = end-start;
    double secs_initialize = elapsed_seconds.count();
    if (verbose) {
        std::cout << "before main algorithm \t" << std::endl;
        std::cout << "parameters age max " << age_max  << std::endl;
        std::cout << "parameters clean_step " << clean_step  << std::endl;
        std::cout << "parameters clean_ratio " << clean_ratio  << std::endl;
        std::cout << "parameters num_threads " << num_threads  << std::endl;
    }
    start = std::chrono::system_clock::now();

    // Main algorithm
    for (unsigned k = 0; k < num_threads; ++k) {
        cntxt.__gen.seed(static_cast<uint64_t>(seeds[2*k]) << 32 | seeds[2*k+1]);
        auto & counter_k = counters[k];
        auto & pop_k = pops[k];
        if (verbose) std::cout << pop_k.individuals.size() << std::endl;

        threads[k] = std::thread(
                [=, &pop_k, &counter_k]() mutable {
                counter_k = popsim_algorithm(pop_k, cntxt, events,
                        T0, T, clean_step, clean_ratio);
                });
    }
    for (auto & th : threads) th.join();

    end = std::chrono::system_clock::now();
    elapsed_seconds = end-start;
    double secs_main_algorithm = elapsed_seconds.count();

    pop = merge(pops);
    counter C {0, 0, 0 };
    for (auto & Cs : counters) C += Cs;

    // ***** OUTPUT_POPULATION
    start = std::chrono::system_clock::now();
    _OUTPUT_POPULATION_
    // *****
    end = std::chrono::system_clock::now();
    elapsed_seconds = end-start;
    double secs_finalize = elapsed_seconds.count();

    Rcpp::NumericVector logs = Rcpp::NumericVector::create(
            Rcpp::Named("proposed_events")= C.proposedEvents,
            Rcpp::Named("effective_events")= C.effectiveEvents,
            Rcpp::Named("cleanall_counter")= C.cleanAll,
            Rcpp::Named("duration_main_algorithm")=secs_main_algorithm);
    // Returns population and simulation info
    Rcpp::List L = Rcpp::List::create(
            Rcpp::Named("population") = new_pop_df,
            Rcpp::Named("logs") = logs);
    return L;
};

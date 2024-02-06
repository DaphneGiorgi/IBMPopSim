// -*- coding: utf-8 -*-

#include <cmath>
#include <limits>
#include <vector>
#include <string>
#include <chrono>
#include <string>
#include <numeric>
#include <iostream>

#include <Rcpp.h>
using namespace Rcpp;


//' Death table
//'
//' @name death_table
//'
//' @description Creates a death table from a population object.
//' For each \code{i=1..N-1} and \code{j=1..M}, the number of individuals with age at last birthday in \code{[ages[i],ages[i+1])} and died in \code{[times[j],times[j+1])} is computed.
//'
//' @param pop Object of class \code{\link{population}}.
//' @param ages A vector of size \code{N} composed of age groups.
//' @param period A vector of size \code{M} composed of time intervals.
//'
//' @details The function computes the number of death in each time interval \code{[times[j],times[j+1])}, \code{j=1..M}.
//'
//' @return A death table matrix.
//'
//' @examples
//' dth_table <-  death_table(population(EW_pop_out), 0:101, 0:11)
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericMatrix death_table(Rcpp::DataFrame pop,
        Rcpp::NumericVector ages,
        Rcpp::NumericVector period)
{
    if (! pop.inherits("population")) stop("Input must be a population() model object.");

    std::vector<double> _ages = Rcpp::as<std::vector<double>>(ages);
    std::vector<double> _period = Rcpp::as<std::vector<double>>(period);

    if (_ages.size()<2)
    	Rcpp::stop("Argument 'ages' must be of length at least 2.");

    if (_period.size()<2)
    	Rcpp::stop("Argument 'period' must be of length at least 2.");

    if (std::any_of(_period.begin(), _period.end(), [](double x){return x<0;}))
    	Rcpp::stop("Period times must be positive values.");

    if (std::any_of(_ages.begin(), _ages.end(), [](double x){return x<0;}))
    	Rcpp::stop("Vector of ages must be positive.");


    int N = _ages.size();
    int M = _period.size();

    Rcpp::NumericMatrix death_matrix(N-1,M-1);

    std::vector<double> diff_ages(N);
    std::adjacent_difference(_ages.begin(), _ages.end(), diff_ages.begin());

    bool uniform_ages = false;
    double h_age = diff_ages[1];

    if(diff_ages.size() == 2){
    	uniform_ages = true;
    }
    else{
    	uniform_ages =  !(std::any_of(std::next(diff_ages.begin()), diff_ages.end(), [h_age](double h) {return std::abs(h-h_age) > std::numeric_limits<double>::epsilon();}));
    }

    std::vector<double> diff_period(M);
    std::adjacent_difference(_period.begin(), _period.end(), diff_period.begin());

    bool uniform_period = false;
    double h_period = diff_period[1];

    if(diff_period.size() == 2){
    	uniform_period = true;
    }
    else{
    	uniform_period = !(std::any_of(std::next(diff_period.begin()), diff_period.end(), [h_period](double h) {return std::abs(h-h_period) > std::numeric_limits<double>::epsilon();}));
    }

    auto names = as<std::vector<std::string>>(pop.names());

    Rcpp::NumericVector births = pop["birth"];
    Rcpp::NumericVector deaths = pop["death"];

    int pop_size = births.size();

    // get boolean column out if existing
    auto is_out = std::any_of(names.begin(), names.end(), [](std::string name){return name == "out";});
    std::vector<bool> out(pop_size,false);

    if(is_out)
    	out = Rcpp::as<std::vector<bool>>(pop["out"]);

    // if uniform period and ages
    if (uniform_period && uniform_ages){
      // loop on all individuals in the population
      for (int i=0; i<pop_size; ++i){
        // get age and death date of individual
        double birth = births[i];
        double death = deaths[i];
        double age = death-birth;
        bool is_out = out[i];

        // if individual is dead and is not out of the pop
        if (!std::isnan(death) && !is_out){
          // if individual died in the given period and its death's age is in the requested age interval
          if ( (death>=_period[0]) && (death<=_period[M-1]) && (age>=_ages[0]) && (age<=_ages[N-1]) ){

            // find the period and age interval
            int idx_period = floor((death-_period[0])/h_period);
            int idx_age = floor((age-_ages[0])/h_age);

            	death_matrix(idx_age,idx_period) += 1;
          }
        }
      }
    }

    // if uniform period and non uniform ages
    if (uniform_period && !uniform_ages){
      // loop on all individuals in the population
      for (int i=0; i<pop_size; ++i){
        // get age and death date of individual
        double birth = births[i];
        double death = deaths[i];
        double age = death-birth;
        bool is_out = out[i];

        // if individual is dead and is not out of the pop
        if (!std::isnan(death) && !is_out){
          // if individual died in the given period and its death's age is in the requested age interval
          if ( (death>=_period[0]) && (death<=_period[M-1]) && (age>=_ages[0]) && (age<=_ages[N-1]) ){

            // find the period and age interval
            int idx_period = floor((death-_period[0])/h_period);
            auto it_age = std::find_if(_ages.begin(), _ages.end(), [age](double x){return age<x;});
            int idx_age = it_age-_ages.begin()-1;

            death_matrix(idx_age,idx_period) += 1;
          }
        }
      }
    }

    // if not uniform period and uniform ages
    if (!uniform_period && uniform_ages){
      // loop on all individuals in the population
      for (int i=0; i<pop_size; ++i){
        // get age and death date of individual
        double birth = births[i];
        double death = deaths[i];
        double age = death-birth;
        bool is_out = out[i];

        // if individual is dead and is not out of the pop
        if (!std::isnan(death) && !is_out){
          // if individual died in the given period and its death's age is in the requested age interval
          if ( (death>=_period[0]) && (death<=_period[M-1]) && (age>=_ages[0]) && (age<=_ages[N-1]) ){

            // find the period and age interval
            auto it_period = std::find_if(_period.begin(), _period.end(), [death](double x){return death<x;});
            int idx_period = it_period-_period.begin()-1;
            int idx_age = floor((age-_ages[0])/h_age);

            death_matrix(idx_age,idx_period) += 1;
          }
        }
      }
    }

    // if not uniform period and not uniform ages
    if (!uniform_period && !uniform_ages){
      // loop on all individuals in the population
      for (int i=0; i<pop_size; ++i){
        // get age and death date of individual
        double birth = births[i];
        double death = deaths[i];
        double age = death-birth;
        bool is_out = out[i];

        // if individual is dead and is not out of the pop
        if (!std::isnan(death) && !is_out){
          // if individual died in the given period and its death's age is in the requested age interval
          if ( (death>=_period[0]) && (death<=_period[M-1]) && (age>=_ages[0]) && (age<=_ages[N-1]) ){

            // find the period and age interval
            auto it_period = std::find_if(_period.begin(), _period.end(), [death](double x){return death<x;});
            int idx_period = it_period-_period.begin()-1;
            auto it_age = std::find_if(_ages.begin(), _ages.end(), [age](double x){return age<x;});
            int idx_age = it_age-_ages.begin()-1;

            death_matrix(idx_age,idx_period) += 1;
          }
        }
      }
    }

  Rcpp::CharacterVector death_row_names(N-1);
  Rcpp::CharacterVector death_col_names(M-1);
  for (int i=0; i<N-1; ++i)
      death_row_names[i] = std::to_string(int(ages[i]));
  for (int i=0; i<M-1; ++i)
    death_col_names[i] = std::to_string(int(period[i]));

  rownames(death_matrix) = death_row_names;
  colnames(death_matrix) = death_col_names;

  return death_matrix;
}

double exposure(double c_i, double d_i, double a, double t, double a_step = 1., double t_step = 1., double e_i = 0.){

  double a_i = t;
  if (c_i+a > a_i)
    a_i = c_i+a;
  if (e_i > a_i)
    a_i = e_i;

  double b_i = t+t_step;
  if(c_i+a+a_step < b_i)
    b_i = c_i+a+a_step;
  if(d_i < b_i)
    b_i = d_i;

  return std::max(0., b_i-a_i);
}

//' Exposure table
//'
//' @name exposure_table
//'
//' @description  Returns the Central Exposure-to-Risk for given ages groups and time period.
//' The central Exposure-to-risk is computed as the sum of the time spent by individuals in a given age group over a given period, where age is the age at last birthday.
//'
//' @inheritParams death_table
//'
//' @details The function computes the central exposure-to-risk in each time interval \code{[t[j],t[j+1])}, \code{j=1..M}, and age groups.
//'
//' @return An exposure matrix
//'
//' @examples
//' ex_table <- exposure_table(population(EW_pop_out),0:101,0:11)
//'
//' @export
// [[Rcpp::export]]
Rcpp::NumericMatrix exposure_table(Rcpp::DataFrame pop,
                                    Rcpp::NumericVector ages,
                                    Rcpp::NumericVector period)
{
  if (! pop.inherits("population")) stop("Input must be a population() model object.");

  std::vector<double> _ages = Rcpp::as<std::vector<double>>(ages);
  std::vector<double> _period = Rcpp::as<std::vector<double>>(period);

	if (_ages.size()<2)
  	Rcpp::stop("Argument 'ages' must be of length at least 2.");

	if (_period.size()<2)
  	Rcpp::stop("Argument 'period' must be of length at least 2.");

	if(std::any_of(_period.begin(), _period.end(), [](double x){return x<0;}))
		Rcpp::stop("Period times must be positive values.");

	if(std::any_of(_ages.begin(), _ages.end(), [](double x){return x<0;}))
		Rcpp::stop("Vector of ages must be positive.");

  int N = _ages.size();
  int M = _period.size();

  Rcpp::NumericMatrix exposure_matrix(N-1,M-1);

  auto names = as<std::vector<std::string>>(pop.names());

  auto is_entry = std::any_of(names.begin(), names.end(), [](std::string n){return n == "entry";});

  std::vector<double> entry;

  if(is_entry)
    entry = Rcpp::as<std::vector<double>>(pop["entry"]);

  Rcpp::NumericVector births = pop["birth"];
  Rcpp::NumericVector deaths = pop["death"];

  int pop_size = births.size();

  // loop on all individuals in the population
  for (int i=0; i<pop_size; ++i){
    // get age and death date of individual
    double c_i = births[i];
    double d_i = deaths[i];
    // handle NA case
    if (std::isnan(d_i))
			d_i = std::numeric_limits<double>::infinity();

		double e_i = (is_entry && !std::isnan(entry[i])) ? entry[i] : 0. ; // handle NA case

    for (int j=0; j<N-1; ++j){
      for(int k=0; k<M-1; ++k){
        double exposure_i = exposure(c_i, d_i, _ages[j], _period[k], _ages[j+1]-_ages[j], _period[k+1]-_period[k], e_i);

        exposure_matrix(j,k) += exposure_i;
      }
    }
  }

  Rcpp::CharacterVector exposure_row_names(N-1);
  Rcpp::CharacterVector exposure_col_names(M-1);
  for (int i=0; i<N-1; ++i)
    exposure_row_names[i] = std::to_string(int(ages[i]));
  for (int i=0; i<M-1; ++i)
    exposure_col_names[i] = std::to_string(int(period[i]));

  rownames(exposure_matrix) = exposure_row_names;
  colnames(exposure_matrix) = exposure_col_names;

  return exposure_matrix;
}

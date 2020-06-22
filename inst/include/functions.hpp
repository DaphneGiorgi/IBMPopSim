#pragma once

#include <iostream>
#include <fstream>
#include <vector>
#include <random>
#include <algorithm>
#include <Rcpp.h>

#include "punctual_function.hpp"

using std::max;
using std::min;
using std::log;
using std::exp;
using std::sin;
using std::cos;
using std::tan;
using std::pow;
using std::sqrt;
using std::ceil;
using std::floor;
using std::abs;

using Rcpp::NumericVector;
using Rcpp::IntegerVector;
using Rcpp::CharacterVector;
using Rcpp::LogicalVector;

using Rcpp::NumericMatrix;
using Rcpp::IntegerMatrix;
using Rcpp::CharacterMatrix;
using Rcpp::LogicalMatrix;

using Rcpp::_;

// ==============================
// ========= DECLARE ============
// ==============================
using function_x = std::function<double(double x)>;
using function_xy = std::function<double(double x, double y)>;

function_x init_function_x(Rcpp::Function f);
function_xy init_function_xy(Rcpp::Function f);


// ==============================
// ========= DEFINE =============
// ==============================
// _______________________________________________________________ IMPORT STEP FUNCTION
inline step_function<double, double> import_stepfun(Rcpp::Function f) {
    Rcpp::Environment env = f.environment();
    Rcpp::NumericVector x = env["x"];
    Rcpp::NumericVector y = env["y"];
    double yleft = env["yleft"];
    double yright = env["yright"];
    double inf = std::numeric_limits<double>::infinity();

    step_function<double, double> sf;
    sf.push_back({ -inf, yleft });
    for (int i = 0; i < x.size(); ++i) {
        sf.push_back( { x[i], y[i] });
    }
    sf.push_back({ inf, yright });

    return sf;
};

// ____________________________________________________ IMPORT PIECEWISE LINEAR FUNCTION
inline function_x import_linfun(Rcpp::Function f) {
    Rcpp::Environment env = f.environment();
    Rcpp::NumericVector x = env["x"];
    Rcpp::NumericVector y = env["y"];
    double yleft = env["yleft"];
    double yright = env["yright"];
    double max = std::numeric_limits<double>::max();

    piecewise_linear_function<double, double> lf;
    lf.push_back({ -max,  yleft });
    for (int i = 0; i < x.size(); ++i) {
        lf.push_back( { x[i], y[i] });
    }
    lf.push_back({ max, yright });

    return lf;
};

// ___________________________________________________________ IMPORT GOMPERTZ FUNCTION
function_x import_gompertz(Rcpp::Function f) {
    Rcpp::Environment env = f.environment();
    double alpha = env["alpha"];
    double beta = env["beta"];
    double lambd = env["lambda"];
    return [=](double x) { return alpha * std::exp(beta * x) + lambd; };
};

// ____________________________________________________________ IMPORT WEIBULL FUNCTION
function_x import_weibull(Rcpp::Function f) {
    Rcpp::Environment env = f.environment();
    double shape = env["k"];
    double scale = env["lambda"];
    return [=](double x) { return (shape/scale) * std::pow(x/scale,shape-1)*std::exp(-std::pow(x/scale,shape)) ; };
};

// ____________________________________________________________ IMPORT NORMAL FUNCTION
function_x import_normal(Rcpp::Function f) {
    Rcpp::Environment env = f.environment();
    double shape = env["shape"];
    double scale = env["scale"];
    return [=](double x) { return shape/scale * std::pow(x/scale,shape-1)*std::exp(-std::pow(x/scale,shape)) ; };
};

struct piecewise_function_x : public step_function<double, function_x>
{
    using step_function<double, function_x>::step_function;
    double operator()(double x) {
        return (step_function<double, function_x>::operator()(x))(x);
    }
};

inline piecewise_function_x import_piecewise_x(Rcpp::Function f) {
    Rcpp::Environment env = f.environment();
    std::vector<double> breaks = Rcpp::as<std::vector<double> >(env["breaks"]);
    Rcpp::List funs = Rcpp::as<Rcpp::List>(env["funs"]);
    piecewise_function_x pf;
    double inf = std::numeric_limits<double>::infinity();
    pf.push_back({ -inf, init_function_x(Rcpp::as<Rcpp::Function>(funs[0])) });
    for (int i = 1; i < breaks.size(); ++i) {
        pf.push_back({ breaks[i], init_function_x(Rcpp::as<Rcpp::Function>(funs[i])) });
    }
    return pf;
}

// ______________________________________________________________________ INIT FUNCTION
inline function_x init_function_x(Rcpp::Function f) {
    std::string s = Rcpp::as<std::string>(Rcpp::as<Rcpp::StringVector>(f.attr("class"))(0));
    if (s == "stepfun") return import_stepfun(f);
    if (s == "linfun") return import_linfun(f);
    if (s == "gompertz") return import_gompertz(f);
    if (s == "weibull") return import_weibull(f);
    if (s == "normal") return import_normal(f);
    if (s == "piecewise_x") return import_piecewise_x(f);
    return [=](double x) { return Rcpp::as<double>(f(x)); };
};


// _____________________________________________________ IMPORT PIECEWISE TIME FUNCTION
struct piecewise_function_xy : public step_function<double, function_x>
{
    using step_function<double, function_x>::step_function;
    double operator()(double x, double y) {
        return (step_function<double, function_x>::operator()(x))(y);
    }
};

function_xy import_piecewise_xy(Rcpp::Function f) {
    Rcpp::Environment env = f.environment();
    std::vector<double> breaks = Rcpp::as<std::vector<double>>(env["breaks"]);
    Rcpp::List funs = Rcpp::as<Rcpp::List>(env["funs"]);
    piecewise_function_xy pf;
    double inf = std::numeric_limits<double>::infinity();
    pf.push_back({ -inf, init_function_x(Rcpp::as<Rcpp::Function>(funs[0])) });
    for (int i = 1; i < breaks.size(); ++i) {
        pf.push_back({ breaks[i], init_function_x(Rcpp::as<Rcpp::Function>(funs[i])) });
    }
    return pf; //[pf](double x, double y) -> double { return (pf(x))(y) };
};

function_xy init_function_xy(Rcpp::Function f) {
    std::string s = Rcpp::as<std::string>(Rcpp::as<Rcpp::StringVector>(f.attr("class"))(0));
    if (s == "piecewise_xy") return import_piecewise_xy(f);
    return [=](double x, double y) { return Rcpp::as<double>(f(x, y)); };
};

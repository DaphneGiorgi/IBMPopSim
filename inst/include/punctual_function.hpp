#pragma once
#include <iostream>
#include <vector>
#include <algorithm>

template <typename TX, typename TY>
struct point {
    point(TX x = 0, TY y = 0) : x(x), y(y) { }
    TX x;
    TY y;
};

template <typename TX, typename TY>
point<TX, TY> make_point(TX x, TY y) {
    return { x, y };
};

template <typename TX, typename TY>
inline bool operator<(point<TX, TY> const & p, TX const & x) {
    return p.x < x;
};

template <typename TX, typename TY>
inline bool operator<(TX const & x, point<TX, TY> const & p) {
    return x < p.x;
};

template <typename TFunction, typename TUnaryFunction>
TFunction apply(TFunction const & f, TUnaryFunction const & u) {
    TFunction result;
    for (auto const & p : f)
        result.push_back({ p.x, u(p.x) });
    return result;
};

template <typename TX, typename TY>
std::ostream & operator<<(std::ostream & o, point<TX, TY> const & p) {
    return o << "(" << p.x << "," << p.y << ")";
};

template <typename TX, typename TY>
using data_function = std::vector< point<TX, TY> >;

template <typename TX, typename TY>
std::ostream & operator<<(std::ostream & o, data_function<TX, TY> const & f) {
    for (point<TX, TY> const & p : f) o << p;
    return o << std::endl;
};

template <typename TX, typename TY>
struct discrete_function
: public data_function<TX, TY>
{
    using data_function<TX, TY>::data_function;
    template <typename TArg>
        TY operator()(TArg const & x) {
            auto it = std::lower_bound(this->begin(), this->end(), x);
            return (it != this->end() && it->x == x)
                ? it->y
                : throw std::domain_error("from discrete function");
        }
};

template <typename TX, typename TY>
struct step_function
: public data_function<TX, TY>
{
    using data_function<TX, TY>::data_function;
    TY operator()(TX const & x) {
        auto it = std::upper_bound(this->begin(), this->end(), x);
        --it;
        return (it != this->end())
            ? it->y
            : throw std::domain_error("from step function");
    }
};

template <typename TX, typename TY>
struct piecewise_linear_function
: public data_function<TX, TY>
{
    using data_function<TX, TY>::data_function;
    TY operator()(TX const & x) {
        auto itp1 = std::upper_bound(this->begin(), this->end(), x);
        auto it = itp1-1;
        if (it == this->end() || itp1 == this->end())
            throw std::domain_error("from piecewise linear function");
        TX c = (x - it->x) / (itp1->x - it->x);
        return it->y + c * (itp1->y - it->y);
    }
};

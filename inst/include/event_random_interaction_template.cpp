#ifndef __INTERACTION
#define __INTERACTION
#endif

// Event "_NAME_"
struct event_J_ : public event {
    double __lambda(unsigned _k, population const & pop, double t, context & cntxt) const {
        individual const & I = pop[_k];
        unsigned _j = pop.pick_potentially_alive(t, cntxt);

        if (pop.is_alive(_j, t)) {
            auto const & J = pop[_j];
            double result;
            _INTENSITY_CODE_
            return result;
        }
        else
            return 0;
    }
    double __lambda_bound(population const & pop, context & cntxt) const {
        double result = cntxt.__intensity_bound[_J_] * pop.nAlive * pop.nAlive;
        return result;
    }
    void __kernel(unsigned _k, population & pop, double t, context & cntxt) const {
        individual & I = pop[_k];
        _KERNEL_METHOD_
    }
    bool __apply(double t, population & pop, context & cntxt) const {
        unsigned _k = pop.pick_potentially_alive(t, cntxt);
        if (pop.is_alive(_k, t)) {
            // Make a reject strategy
            double theta = CUnif() * cntxt.__intensity_bound[_J_];
            if (theta < __lambda(_k, pop, t, cntxt)) {
                __kernel(_k, pop, t, cntxt);
                return true;
            }
        } else pop.kill(_k, t);
        return false;
    }
};

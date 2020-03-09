#ifndef __INTERACTION
#define __INTERACTION
#endif

// Event "_NAME_"
struct event_J_ : public event {
    double __lambda(unsigned k, population const & pop, double t, context & cntxt) const {
        individual const & I = pop[k];
        double sum = 0;
        for (unsigned j = 0; j < pop.individuals.size(); ++j) {
            if (pop.is_alive(j, t)) {
                individual const & J = pop[j];
                double result = 0;
                _INTENSITY_CODE_
                sum += result;
            }
        }
        return sum;
    }
    double __lambda_bound(population const & pop, context & cntxt) const {
        double result = cntxt.__intensity_bound[_J_] * pop.nAlive * pop.nAlive;
        return result;
    }
    void __kernel(unsigned k, population & pop, double t, context & cntxt) const {
        individual & I = pop[k];
        _KERNEL_METHOD_
    }
    bool __apply(double t, population & pop, context & cntxt) const {
        unsigned k = pop.pick_potentially_alive(t, cntxt);
        if (pop.is_alive(k, t)) {
            // Make a reject strategy
            double theta = CUnif() * cntxt.__intensity_bound[_J_] * pop.nAlive;
            if (theta < __lambda(k, pop, t, cntxt)) {
                __kernel(k, pop, t, cntxt);
                return true;
            }
        } else pop.kill(k, t);
        return false;
    }
};

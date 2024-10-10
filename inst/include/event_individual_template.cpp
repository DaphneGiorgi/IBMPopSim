// Event "_NAME_"
struct event_J_ : public event {
    double __lambda(unsigned _k, population & pop, double t, context & cntxt) const {
        individual & I = pop[_k];
        double result = 0;
        _INTENSITY_CODE_
        return result;
    }
    double __lambda_bound(population const & pop, context & cntxt) const {
        double result = cntxt.__intensity_bound[_J_] * pop.nAlive;
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
            double theta = CUnif() * __lambda_bound(pop, cntxt) / pop.nAlive;
            if (theta < __lambda(_k, pop, t, cntxt)) {
                __kernel(_k, pop, t, cntxt);
                return true;
            }
        } else pop.kill(_k, t);
        return false;
    }
};

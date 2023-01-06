// Event "_NAME_"
struct event_J_ : public event {
    double __lambda(unsigned _k, population const & pop, double t, context & cntxt) const {
        double result = 0;
        _INTENSITY_CODE_
        return result;
    }
    double __lambda_bound(population const & pop, context & cntxt) const {
        double result = cntxt.__intensity_bound[_J_] / cntxt.__num_threads;
        return result;
    }
    void __kernel(unsigned _k, population & pop, double t, context & cntxt) const {
        individual & I = pop[_k];
        _KERNEL_METHOD_
    }
    bool __apply(double t, population & pop, context & cntxt) const {
        // Make a reject strategy
        double theta = RUnif() * __lambda_bound(pop, cntxt);
        if (theta < __lambda(0, pop, t)) {
            unsigned _k = _PICK_IFNOT_ENTRY_;
            __kernel(_k, pop, t, cntxt);
            return true;
        }
        return false;
    }
};

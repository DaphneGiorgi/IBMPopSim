#pragma once

struct counter {
    unsigned proposedEvents;
    unsigned effectiveEvents;
    unsigned cleanAll;
    counter & operator+=(counter const & C) {
        proposedEvents += C.proposedEvents;
        effectiveEvents += C.effectiveEvents;
        cleanAll = std::max(cleanAll, C.cleanAll);
        return *this;
    }
};

std::ostream & operator<<(std::ostream & o, counter const & C) {
    return o << "proposedEvents:\t" << C.proposedEvents << "\n"
        << "effectiveEvents:\t" << C.effectiveEvents << "\n"
        << "cleanAll:\t" << C.cleanAll << "\n";
};

counter popsim_algorithm(population & pop,
        context cntxt,
        std::array<event*, NEVENTS> const & events,
        double T1, double T2, double clean_step, double clean_ratio)
{
    std::array<double, NEVENTS> _bounds;
    counter C {0, 0, 0};
    unsigned int k = 0;
    double t = T1;
    while (t < T2) {
        // Clean population following a given frequence
        if (t-T1 > (k+1) * clean_step) {
            pop.cleanAll(t);
            ++C.cleanAll;
            ++k;
        }
        if (pop.nAlive < (1-clean_ratio) * pop.size()) {
            //if ((pop.size() - pop.nAlive)/(double)(pop.size()) > clean_ratio) {
            pop.cleanAll(t);
            ++C.cleanAll;
        }
        if(pop.nAlive > 0) {
            // propose a time t
            double global_intensity_bound = 0.;
            for (int i = 0; i < NEVENTS; ++i) {
                global_intensity_bound += events[i]->__lambda_bound(pop, cntxt);
                _bounds[i] = global_intensity_bound;
            }
            t += CExp() / (global_intensity_bound);

            // propose an event
            double u = global_intensity_bound * CUnif();
            for (int i = 0; i < NEVENTS; ++i) {
                if (u < _bounds[i]) {
                    ++C.proposedEvents;
                    // apply an event
                    if (events[i]->__apply(t, pop, cntxt))
                        ++C.effectiveEvents;
                    break; // for
                }
                // here we have rejected the event...
            }
        } else
            break;
    }

    // Clean the population
    //std::cout << "final clean ... " << std::endl;
    pop.cleanAll(t);
    ++C.cleanAll;

    //std::cout << "final time:\t" << t << std::endl;
    //std::cout << C;
    return C;
};


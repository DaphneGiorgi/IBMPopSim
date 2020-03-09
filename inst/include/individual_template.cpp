struct  individual{
    individual() : death_date(NAN){}
    individual(double birth_date, _DECLARE_CARACTERISTICS_ double death_date = NAN)
        : birth_date(birth_date), _DEFINE_CARACTERISTICS_ death_date(death_date) { }
    double birth_date;

    // **** caracteristiques supplémentaires
_CARACTERISTICS_
    // ****

    double death_date; // If not dead, death_date = nan
    double age(double t) const { return t - birth_date; };
    void set_age(double a, double t) { birth_date = t-a; };
    bool is_dead() const {return !std::isnan(death_date); };
};

inline double age(individual const & I, double t) { return I.age(t); }

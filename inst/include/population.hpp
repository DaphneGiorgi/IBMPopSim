#pragma once
#include <random>

class population {
	public:
    population() = default;
		population(double a_max) : nAlive(0), nDead(0), a_max(a_max) { }

		individual operator[](unsigned k) const { return individuals[k]; }
		individual & operator[](unsigned k) { return individuals[k]; }

		unsigned size() const { return individuals.size(); };
		bool is_alive(unsigned k, double t) const;
		unsigned pick_alive(double t, context & omeg) const;
		unsigned pick_potentially_alive(double t, context & omeg) const;

    void add_init(individual const & ind);
		void add(individual const & ind);

    void kill(unsigned int k, double t, bool moveToCemetery = false);
		/* void cleanDead(); */
		/* void cleanTooOld(double t); */
		void cleanAll(double t);

		unsigned nAlive;
		unsigned nDead;
		double a_max;

    void set_individuals(std::vector<individual> const & v) {
			individuals = v;
			nAlive = v.size();
		}
		void set_dead_individuals(std::vector<individual> const & v) {
			dead_individuals = v;
			nDead = v.size();
		}

		std::vector<individual> individuals;
		std::vector<individual> dead_individuals;
	private:
		//std::vector<individual> immigrated_individuals;
};

inline bool population::is_alive(unsigned k, double t) const {
	if ((age(individuals[k], t) < a_max) && !individuals[k].is_dead()) {
		return true;
	} else {
		return false;
	}
}

// Pick an alive individual !! if no alive individual in the population, it will loop endlessly
inline unsigned population::pick_alive(double t, context & cntxt) const {
	unsigned k;
	do {
		k = std::floor(CUnif() * individuals.size());
	} while (individuals[k].is_dead() && age(individuals[k], t) > a_max);
	return k;
}

// Pick potentially alive (but maybe too old) individual
inline unsigned population::pick_potentially_alive(double t, context & cntxt) const {
	unsigned k;
	do {
		k = std::floor(CUnif() * individuals.size());
	} while (individuals[k].is_dead());
	return k;
}

// Add individual to population (initialization)
inline void population::add_init(individual const & ind){
	if (!ind.is_dead()){
		nAlive++;
		individuals.push_back(std::move(ind));
	}
	else{
		nDead++;
		dead_individuals.push_back(std::move(ind));
	}
}

// Add individual to population (during algorithm)
inline void population::add(individual const & ind){
    nAlive++;
    individuals.push_back(std::move(ind));
#ifdef WITH_ID
    individuals.back().id = NA_INTEGER;
#endif
}

inline void population::kill(unsigned int k, double t, bool moveToCemetery) {
	if (!individuals[k].is_dead()){
		individuals[k].death_date = t;
		nAlive--;
		nDead++;
	}
}

// Clean too old individuals and  bury dead individuals in the population
void population::cleanAll(double t){
	std::vector<individual> new_individuals = std::vector<individual>();
	auto it = individuals.begin();
	while ( it != individuals.end()) {
		if ( (age(*it, t) > a_max) && !(*it).is_dead() ){
			(*it).death_date = (*it).birth_date+this->a_max;
			nAlive--;
			nDead++;
		}

		if ( (*it).is_dead() )
			dead_individuals.push_back(std::move(*it));
		else
			new_individuals.push_back(std::move(*it));

		++it;
	}
	individuals.clear();
	individuals = new_individuals;
}

// split a std::vector<T>
template <typename T>
std::vector<std::vector<T>> split_vector(std::vector<T> const & v, unsigned n) {
	std::vector<std::vector<T>> result(n);
	for (int i = 0; i < v.size(); ++i) {
		result[i % n].push_back(v[i]);
	}
	return result;
}

std::vector<population> split(population const & pop, unsigned n) {
	std::vector<population> result(n);
	auto individuals = split_vector(pop.individuals, n);
	auto dead_individuals = split_vector(pop.dead_individuals, n);
	for (int i = 0; i < n; ++i) {
		result[i].set_individuals(individuals[i]);
		result[i].set_dead_individuals(dead_individuals[i]);
		/* result[i].set_events(pop.events); */
		result[i].a_max = pop.a_max;
	}
	return result;
}

population merge(std::vector<population> const & pops) {
	auto pop = pops[0];
	for (int i = 1; i < pops.size(); ++i) {
		std::move(pops[i].individuals.begin(),
				pops[i].individuals.end(),
				std::back_inserter(pop.individuals));
		std::move(pops[i].dead_individuals.begin(),
				pops[i].dead_individuals.end(),
				std::back_inserter(pop.dead_individuals));
	}
	pop.nAlive = pop.individuals.size();
	pop.nDead = pop.dead_individuals.size();
	return pop;
}

IBMPopSim
=========

The IBMPopSim package aims at simulating the random evolution of heterogeneous populations, called stochastic Individual Based Models (IBMs). 

The package allows users to simulate population evolution in which individuals are characterized by their age and some characteristics, and where the population is modified by different types of events including births/arrivals, death/exit events, or changes of characteristics. The frequency at which an event can occur to an individual can depend on his age and characteristics, but also on time and on the rest of the population (interactions).
Such models have a wide range of applications in various fields including actuarial sciences, biology, demography, or ecology. For instance, IBMs can be used for simulating the evolution of an heterogeneous insurance portfolio, assessing the basis and demographic risk, or validating mortality forecasts. In this presentation, we propose an illustration of such applications based
on two examples.
IBMPopSim overcomes the limitations of time consuming IBMs simulations. This is done by implementing new efficient algorithms based on thinning methods, which are compiled using the Rcpp library. The package allows a wide range of IMBs to be simulated, while being user-friendly thanks to its structure based on simple build blocks. In addition, we provide tools for analyzing outputs, such a age-pyramids or life tables obtained from the simulated data, consistent with the data format of packages for mortality modeling such as StMoMo.

The IBMPopSim R package is conceived to simulate the random evolution of structured population dynamics, called stochastic Individual Based Models (IBMs).

The package allows users to simulate the random evolution of  a population in which  individuals are characterised by their date of birth, a set of (discrete or continuous) attributes, and their potential date of death.
The population is modified due to different types of events  defined by the user, and occuring at random dates. These events include the birth/arrival or death/exit of an individual in the population, an individual changing caracteristics, or custom events defined by the user.
Once the events modifiying the population have been defined, the last step before simulating the population evolution is to specify the so-called events intensities, describing the frequency at which the different types of events occur in the population.

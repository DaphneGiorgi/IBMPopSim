IBMPopSim
=========

The IBMPopSim package aims at simulating the random evolution of heterogeneous populations, called stochastic Individual Based Models (IBMs). Such models have a wide range of applications in various fields including actuarial sciences, biology, demography, or ecology. For instance, IBMs can be used for simulating the evolution of an heterogeneous insurance portfolio, of an spatial ecological population with interacting individuals, or for validation mortality forecasts. 

The package allows users to simulate population evolution in which individuals are characterized by their age and some characteristics, and where the population is modified by different types of events including births/arrivals, death/exit events, or changes of characteristics. The frequency at which an event can occur to an individual can depend on his age and characteristics, but also on time and on the rest of the population (interactions).

IBMPopSim overcomes the limitations of time consuming IBMs simulations. This is done by implementing new efficient algorithms based on thinning methods, which are compiled using the Rcpp library. The package allows a wide range of IMBs to be simulated, while being user-friendly thanks to its structure based on simple build blocks. In addition, we provide tools for analyzing outputs, such a age-pyramids or life tables obtained from the simulated data, consistent with the data format of packages for mortality modeling such as StMoMo.

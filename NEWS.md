# IBMPopSim 1.0.0

## New features

- Introduce classes and methods for data structures returned by main functions : population, pyramid, logs, simulation_output
- Functions with a 'population' argument are now methods of 'population' class
- Return all prompts at the begging of the line
- Changed argument from 'population' to 'initial_population' in the call to main function 'popsim', initial_population shall inherit from 'population' class
- Add compatibility functions to check compatibility between a given model and population and between characteristics and events
- Replace 'with_id' flag in 'mk_model' fct by a search of 'id' among individual type names

# IBMPopSim 0.4.4

## Bug fixes
- Set the arguments of 'popsim' function available from all levels of R environment 

## New features
- Add verbose flag to pop_sim function
- Vignettes update

# IBMPopSim 0.4.3

## Bug fixes
- Add figures in interaction vignette
- Fix run test in mk_model and plot_pyramid functions
- Add reserved names for characteristics and parameters : I, J, t, pop, newI, id, entry, out
- Hide plot commands from vignettes
- Add default value for "out" column for entry events 
- Add default value for "entry" column for birth event
- Add characteristics tracking to merge_pop_withid

## New features
- Convert exposure_table and death_table to Rcpp for more speed
- Reduce dependencies


## Scenario 4: totally random testing, with `exact' SA test numbers

source('./scenarios/sa_testing.R')

#make number of timesteps equal to number of dates with testing data

tests_conducted = dd_test$daily_tests  ### or choose dd_test_raw$daily_tests_raw

timestep_reduction = 1 # don't change
tstart = 1
tend = length(tests_conducted)
timesteps = seq(tstart, tend, 1/timestep_reduction)


max_daily_test_supply = rep(2e5, length(timesteps))
testing_demand_feedback_strength = 0 # no feedback here
testing_demand_lag = 4

compartments_to_test = c("S", "E", "A", "P", "M", "C", "R_P", "R_N", "daily_deaths")

tested = array(dim = c(length(timesteps),length(compartments_to_test)))

colnames(tested) = compartments_to_test

tested <- tested %>% data.frame()

relHaz = matrix(nrow = length(timesteps), ncol = length(compartments_to_test))
colnames(relHaz) = c("S", "E", "A", "P", "M", "C", "R_P", "R_N", "daily_deaths")
relHaz = relHaz %>% data.frame()

relHaz[,] = 1 # totally random testing

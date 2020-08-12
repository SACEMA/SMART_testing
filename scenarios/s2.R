## Scenario 2: only symptomatic people get tested, with linear approximation to SA test numbers

source('scenarios/tests_conducted_linearSA.R')

# relative hazards 1 for M, 10 for C, 0.1 for the rest

compartments_to_test = c("S", "E", "A", "P", "M", "C", "R_P", "R_N", "daily_deaths")
tested = array(dim = c(length(timesteps),length(compartments_to_test)))
colnames(tested) = compartments_to_test
tested <- tested %>% data.frame()

relHaz = matrix(nrow = length(timesteps), ncol = length(compartments_to_test))
colnames(relHaz) = c("S", "E", "A", "P", "M", "C", "R_P", "R_N", "daily_deaths")
relHaz = relHaz %>% data.frame()

# passive testing only; non-covid only tested when presenting similar sypmtoms
for(row in 1:nrow(relHaz)){
  relHaz[row,c("S", "E", "A", "P", "M", "C", "R_P", "R_N", "daily_deaths")] = 
  c(0.01, 0.01, 0.01, 0.01, 1, 10, 0.01, 0.01, 0.01) } 

# can we get info on this from test positivity rate?

# brainstorm

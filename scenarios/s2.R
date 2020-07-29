## Scenario 2: only symptomatic people get tested, with linear approximation to SA test numbers

source('scenarios/tests_conducted_linearSA.R')

# relative hazards 1 for M, 10 for C, 0.1 for the rest?

relHaz = matrix(nrow = length(timesteps), ncol = length(compartments_to_test))
colnames(relHaz) = c("S", "E", "A", "P", "M", "C", "R_P", "R_N", "daily_deaths")
relHaz = relHaz %>% data.frame()
relHaz[,c("S", "E", "A", "P", "M", "C", "R_P", "R_N", "daily_deaths")] = 
  rep(c(0.1, 0.1, 0.1, 0.1, 1, 10, 0.1, 0.1, 0.1), nrow(relHaz)) # totally random testing
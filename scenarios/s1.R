## Scenario 1: completely random testing with linear approximation to SA test numbers

source('./scenarios/tests_conducted_linearSA.R')
# source('scenarios/tests_conducted_constant.R')

# relative hazards 1 for everybody

compartments_to_test = c("S", "E", "A", "P", "M", "C", "R_P", "R_N", "daily_deaths")
tested = array(dim = c(length(timesteps),length(compartments_to_test)))
colnames(tested) = compartments_to_test
tested <- tested %>% data.frame()


relHaz = matrix(nrow = length(timesteps), ncol = length(compartments_to_test))
colnames(relHaz) = c("S", "E", "A", "P", "M", "C", "R_P", "R_N", "daily_deaths")
relHaz = relHaz %>% data.frame()

relHaz[,] = 1 # totally random testing

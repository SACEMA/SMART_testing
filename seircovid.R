##Discrete time model
##covid testing

# rmvhyper(1, n = c(1,2,3), k = 2)

# rules
# can't have k > sum(n)
# all groupsizes must be non-negative

# random note
# want to later encode not all infecteds available for testing

rm(list=ls(all=TRUE))
library(tidyverse)
library(extraDistr)

tstart=0
tend=100
timestep_reduction = 1
timesteps = seq(tstart, tend, 1/timestep_reduction)

variables = 14 
SEIR = array(dim = c(length(timesteps),variables))
colnames(SEIR) = c("S", "E", "I_a", "A_a","I_p", "A_p", "I_m", "A_m", "I_c", "A_c", "R", "R_a", "D", "D_a")

#initial population sizes
S0 = 9700
E0 = 300
I_a0 = 0
I_p0 = 0
I_m0 =  0
I_c0 =  0
R0 = 0
D0 = 0
#ascertained compartments initial population sizes
A_a0 = 0
A_p0 = 0
A_m0 = 0
A_c0 = 0
R_a0 = 0
D_a0 = 0

SEIR[1,] = c(S0, E0, I_a0, A_a0, I_p0, A_p0, I_m0, A_m0, I_c0, A_c0, R0, R_a0, D0, D_a0 )

N0 = sum(SEIR[1,])

#####parameter values#####
# sigma = progression rates between infectious compartments 
#gamma = recovery rates
#psi = ascertainments rates
#mu = death rates

# time in I_P ~ 2 days
# time in I_M ~ 7 days
# time in I_C ~ 7 days
# 15% of E goes to I_A
# 10% of I_M goes to critical

# tests_in_class = size_of_class / eligible_pop_size * total_tests
# carrying capacity for getting tested
# (weight we are assigning * n_critical) / total_eligible

# P(S) = S/N
# P(I) = I*w_I/(S+R+I*w_I)
# check that elibigible pop is larger than n_tests (and adjust if necessary)

## testing/ascertainment parameters

tests_conducted = rep(100, length(timesteps))
max_daily_test_supply = rep(500, length(timesteps)) # these supply parameters shouldn't be constant

# they should be based on real (or realistic) data on test availability
compartments_to_test = c("S", "E", "I_a","I_p",  "I_m", "I_c", "R" )
tested = array(dim = c(length(timesteps),length(compartments_to_test)))
colnames(tested) = compartments_to_test
pos<-c()
neg<-c()

testing_demand_feedback_strength = 0
testing_demand_lag = 4 #days

# Notes on "demand-driven" testing roll-out
# - don't want total numbers of tests wanted to get too out of hand?
# - demand parameter can also be thought of as (1/total_pop) * max_realistic_test_supply
# - could also do something like A_total/I_total * some_parameter. I.e. govt has
# some idea how many infections there really are, as well as how many infections are detected,
# and will scale up testing to try and close that gap as much as they can
# - when planning scenarios (e.g. low resources, only test critical cases;
# high resources, widespread random testing etc) we must take care to choose the 
# max_daily_supply and test criteria parameters in a sensible and consistent way
# (e.g. if we want widescale random testing the max_daily_supply would need to
# be sufficiently large)

beta = 0.8 
alpha = .25  #2,7 proportion asymptomatic
m = 1/3.69     #2,7 1/latency duration

sigma_p = 1/1.75   #3,11 presymptomatic to mild
sigma_m = 1/7    #4,12 mild to critical
gamma_m = 1/7  #5,10 mild to recovery 
gamma_c = 1/11.5  #6,13 critical to recovery
gamma_a = 1/7  #8,9 asymptomatic to recovered   
mu_c = 1/6   #20,14 critical to death

r = 1 #reduction in "infectiousness" due to ascertainment (i.e. some form of quarantine or self-isolation)

for(t_index in seq(2,nrow(SEIR))){
  
  N = sum(SEIR[t_index-1,1:(ncol(SEIR)-2)])
  
  S = SEIR[t_index - 1, "S"]
  E = SEIR[t_index - 1, "E"]
  I_a = SEIR[t_index - 1, "I_a"]
  A_a = SEIR[t_index - 1, "A_a"]
  I_p = SEIR[t_index - 1, "I_p"]
  A_p = SEIR[t_index - 1, "A_p"]
  I_m = SEIR[t_index - 1, "I_m"]
  A_m = SEIR[t_index - 1, "A_m"]
  I_c = SEIR[t_index - 1, "I_c"]
  A_c = SEIR[t_index - 1, "A_c"]
  R = SEIR[t_index - 1, "R"]
  R_a = SEIR[t_index - 1, "R_a"]
  D = SEIR[t_index - 1, "D"]
  D_a = SEIR[t_index - 1, "D_a"]
  
  #lambda is a fucntion of time
  lambda = beta*(((I_a + I_p + I_m + I_c)+r*(A_a + A_p + A_m + A_c))/N)
  
  # moving between compartments due to disease process
  change_S = - lambda*S
  change_E = lambda*S - m*E
  change_I_a = (alpha*m)*E - (gamma_a)*I_a
  change_A_a = - gamma_a*A_a
  change_I_p = ((1-alpha)*m)*E - (sigma_p)*I_p
  change_A_p = - sigma_p*A_p
  change_I_m = sigma_p*I_p - (sigma_m + gamma_m)*I_m
  change_A_m = sigma_p*A_p - (sigma_m + gamma_m)*A_m
  change_I_c = sigma_m*I_m - (gamma_c  + mu_c)*I_c
  change_A_c = sigma_m*A_m - (gamma_c + mu_c)*A_c
  change_R = gamma_a*I_a + gamma_m*I_m + gamma_c*I_c
  change_R_a = gamma_a * A_a + gamma_m*A_m + gamma_c*A_c
  change_D = mu_c*I_c
  change_D_a = mu_c*A_c
  
  rateofchange_disease_processes <- c(change_S, change_E, change_I_a, change_A_a, change_I_p, change_A_p, change_I_m,
                  change_A_m, change_I_c,change_A_c, change_R, change_R_a, change_D,change_D_a)
  
  SEIR[t_index,] = SEIR[t_index-1,] + rateofchange_disease_processes*1/timestep_reduction
  
  # Moving between compartments due to testing:
  
  test_result_lag = 0
  test_collection_date = max(t_index - test_result_lag, 1)
  
  sampling_weights = c(0.1, 0.1, 1, 1, 1, 1, 0.1)
  
  groupsizes = floor(SEIR[test_collection_date, c("S", "E", "I_a", "I_p", "I_m", "I_c", "R")] 
                    * sampling_weights) #weighted group sizes; equivalent to group members eligible for testing
  
  # number of tests
  # tests_per_weighted_groupsize * sum(groupsizes) = n_tests 
  # tests_per_weighted_groupsize = n_tests / sum(groupsizes) #really tests allocated
  # min(groupsizes['k'], tests_per_weighted_groupsize * groupsizes['k'] ) = n_tests_k, where k is a compartment
  # function(number of tests, population_vector, relative_hazard_vector)
  
  if(tests_conducted[test_collection_date] > sum(groupsizes)){
    print(sprintf("WARNING: %s tests available, but only %s eligible individuals. Reducing tests_conducted to number of eligible individuals.", tests_conducted[test_collection_date], sum(groupsizes)))
    tests_conducted[test_collection_date] = sum(groupsizes)
  }
  
  tested_pergroup <- rmvhyper(1, n=groupsizes, k=tests_conducted[test_collection_date])
  
  tested[t_index,]<- tested_pergroup
  neg<-append(neg, sum(tested[t_index,"S"],tested[t_index,"E"],tested[t_index,"R"]))
  pos<-append(pos, sum(tested[t_index,"I_a"],tested[t_index,"I_p"],tested[t_index,"I_m"],tested[t_index,"I_c"] ))
  
  tests_wanted_in_two_weeks = pos[length(pos)] * testing_demand_feedback_strength
  tests_conducted[t_index + testing_demand_lag] =
    tests_conducted[t_index + testing_demand_lag] +
    min(tests_wanted_in_two_weeks, max_daily_test_supply[t_index + testing_demand_lag])
  
  change_test_S = 0
  change_test_E = 0
  change_test_I_a = - tested[t_index,"I_a"] 
  change_test_A_a = tested[t_index,"I_a"]
  change_test_I_p = - tested[t_index,"I_p"]
  change_test_A_p = tested[t_index,"I_p"]
  change_test_I_m = - tested[t_index,"I_m"]
  change_test_A_m = tested[t_index,"I_m"]
  change_test_I_c = - tested[t_index,"I_c"]
  change_test_A_c = tested[t_index,"I_c"]
  change_test_R = 0
  change_test_R_a = 0
  change_test_D = 0
  change_test_D_a = 0
  
  rates_change_testing= c(change_test_S, change_test_E, change_test_I_a, change_test_A_a,
                          change_test_I_p, change_test_A_p, change_test_I_m, change_test_A_m,
                          change_test_I_c, change_test_A_c, change_test_R, change_test_R_a,
                          change_test_D, change_test_D_a)
  SEIR[t_index, ] = SEIR[t_index, ] + rates_change_testing
}


plot(timesteps, SEIR[, "S"], type = 'l', col = 'blue', ylim = c(min(SEIR), max(SEIR)), xlim=c(0,130))
lines(timesteps, SEIR[, "E"], col = 'red')
lines(timesteps, SEIR[, "I_a"], col = 'green')
lines(timesteps, SEIR[, "I_p"], col = 'orange')
lines(timesteps, SEIR[, "I_m"], col = 'pink' )
lines(timesteps, SEIR[, "I_c"], col = 'turquoise' )
lines(timesteps, SEIR[, "R"], col = 'violet')
lines(timesteps, SEIR[, "D"], col = 'black')
lines(timesteps, SEIR[, "A_a"], col = 'black')
legend(105, 8000, legend=c("S", "E", "I_a", "I_p", "I_m" , "I_c", "R", "D"),
       col=c("blue", "red", "green", "orange", "pink", "turquoise","violet", "black"), lty=1, cex=0.8)



plot(timesteps, SEIR[, "A_a"], type = 'l', col = 'blue', ylim = c(min(SEIR), 2000), xlim=c(0,130))
lines(timesteps, SEIR[, "A_p"], col = 'red')
lines(timesteps, SEIR[, "A_m"], col = 'green')
lines(timesteps, SEIR[, "A_c"], col = 'orange')
legend(105, 1800, legend=c("A_a", "A_p", "A_m", "A_c"),
       col=c("blue", "red", "green", "orange"), lty=1, cex=0.8)

# 
# true_positives = SEIR %>%
#   data.frame() %>%
#   select(I_p, I_a, I_m, I_c, A_p, A_a, A_m, A_c)
# true_positives_summary = rowSums(true_positives)
# 
# 
# plot(neg)
# plot(pos, type = "l", ylim = c(0,max(true_positives_summary)))
# lines(true_positives_summary)

#lines(timesteps, cumsum(daily_tests$positive_results), lwd=3, col = 'grey')


## this next section is very very dirty code and we're not proud
## but the basic model does work as expected

hidden_positives = SEIR %>%
 data.frame() %>%
 select(I_a, I_p, I_m, I_c)
hidden_prevalence = rowSums(hidden_positives)
eligible_pop_per_time = SEIR %>%
 data.frame() %>%
 select(-c(R_a, A_a, A_p, A_m, A_c, D_a, D))
elibibility_prevalence = rowSums(eligible_pop_per_time)

plot(timesteps[1:(length(timesteps)-1)], pos/tests_conducted[1:(length(tests_conducted) - 5)], type = 'l', col = 'blue', ylab = "Proportion", xlab = "time (days)")
lines(timesteps, hidden_prevalence/elibibility_prevalence, col = 'green')
legend(x=60, y = 2, legend = c("proportion\n positive \n","prevalence \n among \n eligibles"), lty = 1, col = c("blue", "green"))

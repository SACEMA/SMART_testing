##Discrete time model
##covid testing

# rmvhyper(1, n = c(1,2,3), k = 2)

# rules
# can't have k > sum(n)
# all groupsizes must be non-negative

# random note
# want to later encode not all infecteds available for testing

rm(list=ls(all=TRUE))
source("hazard.R")
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

tests_conducted = rep(250, length(timesteps))
max_daily_test_supply = rep(1000, length(timesteps)) # these supply parameters shouldn't be constant

# they should be based on real (or realistic) data on test availability
compartments_to_test = c("S", "E", "I_a","I_p",  "I_m", "I_c", "R" )
tested = array(dim = c(length(timesteps),length(compartments_to_test)))
colnames(tested) = compartments_to_test
pos<-c(0)
neg<-c(tests_conducted[1])
tested[1,] = 0
incident_cases = c(0)
prevalent_cases = c(0)
observed_prevalent_cases = c(0)
cumulative_observed_cases = c(0)
cumulative_observed_deaths = c(0)

testing_demand_feedback_strength = 0
testing_demand_lag = 14 #days

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

  relHaz = c("S" = 1, "E" = 1, "I_a" = 1, "I_p" = 1, "I_m" = 1,"I_c" = 1, "R" = 1) # still don't know how to interpret these numbers... for example, how do we account for the number of people we think have covid-like symptoms, but no covid?
  elibible_pop = sum(SEIR[t_index, names(relHaz[relHaz > 0])]) # this feels messier than just defining relHaz to be zero for the ascertained and dead classes
  
  if(elibible_pop < tests_conducted[t_index]){
    print(sprintf("%s tests planned, but only %s individuals with nonzero hazards of being tested", tests_conducted[t_index], elibible_pop))
    tests_conducted[t_index] = elibible_pop
  }
  
  tested_per_group = assignTests(numTests = tests_conducted[t_index], state = SEIR[t_index, names(relHaz)], relHaz = relHaz)
  
  tested[t_index, ] <- tested_per_group
  neg<-append(neg, sum(tested[t_index,"S"],tested[t_index,"E"],tested[t_index,"R"]))
  pos<-append(pos, sum(tested[t_index,"I_a"],tested[t_index,"I_p"],tested[t_index,"I_m"],tested[t_index,"I_c"] ))
  incident_cases <- append(incident_cases, S*lambda)
  prevalent_cases <- append(prevalent_cases, sum(I_a, A_a, I_p, A_p, I_m, A_m, I_c, A_c))
  observed_prevalent_cases <- append(observed_prevalent_cases, sum(A_a, A_p, A_m, A_c))
  
  
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
  
  #testing demand feedback
  tests_wanted_in_two_weeks = pos[length(pos)] * testing_demand_feedback_strength
  tests_conducted[t_index + testing_demand_lag] =
    tests_conducted[t_index + testing_demand_lag] +
    min(tests_wanted_in_two_weeks, max_daily_test_supply[t_index + testing_demand_lag])
  
}

tests_conducted = tests_conducted[1:length(timesteps)] #dirty fix for demand driven testing making this vector too long

n_alive = SEIR %>%
  data.frame() %>%
  select(-D, -D_a) %>%
  rowSums()

rowSums(n_alive)

dd <- SEIR %>%
  data.frame() %>%
  mutate(positive_tests = pos, day = 1:nrow(SEIR)) %>%
  mutate(incident_cases = incident_cases) %>%
  mutate(tests_conducted = tests_conducted) %>%
  mutate(prevalent_cases = prevalent_cases) %>%
  mutate(observed_prevalent_cases = observed_prevalent_cases) %>%
  mutate(n_alive = n_alive) %>%
  mutate(prevalence_per_thousand = 1000 * prevalent_cases/n_alive) %>%
  mutate(daily_deaths = c(0, diff(D)+diff(D_a))) %>%
  mutate(daily_deaths_unobserved = c(0, diff(D))) %>%
  mutate(daily_deaths_observed = c(0, diff(D_a))) %>%
  # group_by(day) %>%
  mutate(cumulative_confirmed_cases = cumsum(positive_tests))
  
cumulative_plot <- dd %>%
  ggplot(aes(x = day, y = cumulative_confirmed_cases, color = "Cumulative confirmed cases")) +
  geom_line()+
  # geom_line(aes(x = day, y = positive_tests, color = "Daily positive tests"))+
  geom_line(aes(x = day, y = D_a, color = "Cumulative observed deaths"))+
  labs(x = "Time (days)", y = "Cumulative cases and deaths")
cumulative_plot

outbreak_plot <- dd %>%
  ggplot(aes(x = day, y = prevalence_per_thousand, color = "Prevalence"))+
  geom_line() +
  labs(x = "Time (days)", y = "Cases per thousand population")
outbreak_plot

mortality_plot <- dd %>%
  ggplot(aes(x = day, y = daily_deaths, color = "Daily deaths")) +
  geom_line() +
  labs(x = "Time (days)", y = "People")
mortality_plot

inc_plot <- dd %>% ggplot(aes(x = day, y = incident_cases, color = "Incident cases per day")) +
  geom_line() +
  geom_line(aes(x = day, y = pos, color = "Newly confirmed cases per day")) +
  geom_line(aes(x = day, y = daily_deaths, color = "Daily deaths")) +
  labs(x = "Time (days)", y = "People")
inc_plot

prev_plot <- dd %>% ggplot(aes(x = day, y = observed_prevalent_cases, color = "Confirmed prevalent cases"))+
  geom_line() +
  geom_line(aes(x = day, y = prevalent_cases, color = "True prevalent cases"))
prev_plot

prop_ascertained_plot <- dd %>% 
  ggplot(aes(x = day, y = observed_prevalent_cases/prevalent_cases, color =
               "Proportion of true positives \n which are ascertained")) +
  geom_line() +
  labs(x = "Time (days)", y = "Proportion")
prop_ascertained_plot



# pos_vs_inc <- dd %>% ggplot(aes(x = pos, y = incident_cases, color = "incident_cases")) +
#   geom_line()
# pos_vs_inc

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

# hidden_positives = SEIR %>%
#  data.frame() %>%
#  select(I_a, I_p, I_m, I_c)
# hidden_prevalent_cases = rowSums(hidden_positives)
# eligible_pop_per_time = SEIR %>%
#  data.frame() %>%
#  select(-c(R_a, A_a, A_p, A_m, A_c, D_a, D))
# elibibility_prevalent_cases = rowSums(eligible_pop_per_time)

# plot(timesteps[1:(length(timesteps))], pos/tests_conducted[1:(length(tests_conducted))], type = 'l', col = 'blue', ylab = "Proportion", xlab = "time (days)")
# lines(timesteps, hidden_prevalent_cases/elibibility_prevalent_cases, col = 'green')
# legend(x=60, y = 2, legend = c("proportion\n positive \n","prevalent_cases \n among \n eligibles"), lty = 1, col = c("blue", "green"))

# questions/tasks to resolve before tuesday
# - plot results nicely (see meeting notes)
# - why are there regular fluctuations in the demand driven testing
# - why one day lag?
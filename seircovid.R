##Discrete time model
##covid testing

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
S0 = 9997
E0 = 3
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

tests_conducted = rep(100, length(timesteps))
max_daily_test_supply = rep(1000, length(timesteps))

testing_demand_feedback_strength = 15
testing_demand_lag = 2 #days

# proportion of people being ascertained (demand) * some factor

compartments_to_test = c("S", "E", "I_a","I_p",  "I_m", "I_c", "R" )
tested = array(dim = c(length(timesteps),length(compartments_to_test)))
colnames(tested) = compartments_to_test
tested <- tested %>% data.frame()
pos<-c(0)
neg<-c(tests_conducted[1])
tested[1,] = 0
incident_cases = c(0)
incident_inf_cases = c(0)
prevalent_cases = c(0)
eligible_pop_t = c(sum(SEIR[1,compartments_to_test]))
observed_prevalent_cases = c(0)
cumulative_observed_cases = c(0)
cumulative_observed_deaths = c(0)


# sigma = progression rates between infectious compartments 
# gamma = recovery rates
# psi = ascertainments rates
# mu = death rates

beta = 0.8 
alpha = .25  #2,7 proportion asymptomatic
m = 1/3.69     #2,7 1/latency duration

sigma_p = 1/1.75   #3,11 presymptomatic to mild
sigma_m = 1/32    #4,12 mild to critical    # citing The Novel Coronavirus Pneumonia Emergency Response Epidemiology Team. The Epidemiological Characteristics of an Outbreak of 2019 Novel Coronavirus Diseases (COVID-19) — China, 2020[J]. China CDC Weekly, 2020, 2(8): 113-122. doi: 10.46234/ccdcw2020.032 shu
gamma_m = 1/8  #5,10 mild to recovery       # about 1/5 symptomatic cases become critical
gamma_c = 1/7  #6,13 critical to recovery
gamma_a = 1/14  #8,9 asymptomatic to recovered
mu_c = 1/40   #20,14 critical to death      # about a quarter of critical cases ("severe + critical" in the cited work) die

r = 1         #reduction in "infectiousness" due to ascertainment

relHaz = matrix(nrow = length(timesteps), ncol = 7)
colnames(relHaz) = c("S", "E", "I_a", "I_p", "I_m","I_c", "R")
relHaz = relHaz %>% data.frame()

# for(i in 1:101){
#   relHaz[i,c("S", "E", "I_a", "I_p", "I_m","I_c", "R")] = c(1 , 1, 1, 1, 1, 1, 1)}
 
for(i in 1:14){
  relHaz[i,c("S", "E", "I_a", "I_p", "I_m","I_c", "R")] = c(1 , 1, 10, 10, 10, 10, 1)}
for(i in 15:25){
  relHaz[i,c("S", "E", "I_a", "I_p", "I_m","I_c", "R")] = c(1 , 1, 1, 1, 1, 1, 1)}
for(i in 26:length(timesteps)){
  relHaz[i,c("S", "E", "I_a", "I_p", "I_m","I_c", "R")] = c(1 , 1, 1, 1, 2, 50, 1)}


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

  # relHaz = relHaz_df[t_index,] #c("S" = 1, "E" = 1, "I_a" = 1, "I_p" = 1, "I_m" = 1,"I_c" = 1, "R" = 1)
  elibible_pop = sum(SEIR[t_index, names(relHaz)]) # this feels messier than just defining relHaz to be zero for the ascertained and dead classes
  eligible_pop_t = c(eligible_pop_t, elibible_pop)
  
  if(elibible_pop < tests_conducted[t_index]){
    print(sprintf("%s tests planned, but only %s individuals with nonzero hazards of being tested", tests_conducted[t_index], elibible_pop))
    tests_conducted[t_index] = elibible_pop
  }
  
  tested_per_group = assignTests(numTests = tests_conducted[t_index], state = SEIR[t_index, names(relHaz)], relHaz = relHaz[t_index,])
  
  tested[t_index, ] <- tested_per_group
  neg<-append(neg, sum(tested[t_index,"S"],tested[t_index,"E"],tested[t_index,"R"]))
  pos<-append(pos, sum(tested[t_index,"I_a"],tested[t_index,"I_p"],tested[t_index,"I_m"],tested[t_index,"I_c"] ))
  incident_cases <- append(incident_cases, S*lambda)
  incident_inf_cases <- append(incident_inf_cases,  m*E )
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
  
  # testing demand feedback
  future_test_demand = pos[length(pos)] * testing_demand_feedback_strength
  tests_conducted[t_index + testing_demand_lag] =
    min(tests_conducted[t_index + testing_demand_lag] +
    future_test_demand, max_daily_test_supply[t_index + testing_demand_lag])
}

tests_conducted = tests_conducted[1:length(timesteps)] #dirty fix for demand driven testing making this vector too long

n_alive = SEIR %>%
  data.frame() %>%
  select(-D, -D_a) %>%
  rowSums()

dd <- SEIR %>%
  data.frame() %>%
  mutate(positive_tests = pos, day = 1:nrow(SEIR)) %>%
  mutate(incident_cases = incident_cases) %>%
  mutate(incident_inf_cases = incident_inf_cases) %>%
  mutate(tests_conducted = tests_conducted) %>%
  mutate(prevalent_cases = rowSums(SEIR[,c("A_p","A_a","A_m","A_c","I_p","I_a","I_m","I_c")])) %>%
  mutate(observed_prevalent_cases = rowSums(SEIR[,c("A_p","A_a","A_m","A_c")])) %>%
  mutate(eligible_pop = eligible_pop_t ) %>% #rowSums(SEIR[,c("S","E","I_p","I_a","I_m","I_c","R")])
  mutate(prevalent_cases_non_ascertained = rowSums(SEIR[,c("I_p","I_a","I_m","I_c")])) %>%
  mutate(n_alive = n_alive) %>%
  mutate(prevalence_per_thousand = 1000 * prevalent_cases/n_alive) %>%
  mutate(daily_deaths = c(0, diff(D)+diff(D_a))) %>%
  mutate(daily_deaths_unobserved = c(0, diff(D))) %>%
  mutate(daily_deaths_observed = c(0, diff(D_a))) %>%
  mutate(cumulative_confirmed_cases = cumsum(positive_tests)) %>%
  mutate(prop_positive = positive_tests/tests_conducted)%>%
  mutate(cumulative_incidence = cumsum(incident_cases)) %>%
  mutate(max_daily_test_supply = max_daily_test_supply)

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



test_efficacy_plot <- dd %>%
  ggplot(aes(x = day, y = prevalent_cases_non_ascertained, color = 'Prevalent cases, non-ascertained')) +
  geom_line() +
  geom_line(aes(x = day, y = eligible_pop, color = 'Population eligible for testing'))
test_efficacy_plot

test_efficacy_plot_relative <- dd %>%
  ggplot(aes(x = day, y = prevalent_cases_non_ascertained/eligible_pop, color = 'Prevalence among \n people eligible for testing')) +
  geom_line() + 
  geom_line(aes(x = day, y = prop_positive, color = 'Proportion positive')) +
  scale_colour_manual(values=cbPalette)
test_efficacy_plot_relative

prop_pos_plot <- dd %>%
  ggplot(aes(x = day, y = prop_positive, color = "Proportion of tests \n which come back positive")) +
  geom_line() +
  geom_line(aes(x= day, y = prevalence_per_thousand/1000, color = "Prevalence per \n million")) +
  scale_colour_manual(values=cbPalette) 
prop_pos_plot

cumulative_plot <- dd %>%
  ggplot(aes(x = day, y = cumulative_confirmed_cases, color = "Cumulative ascertained cases")) +
  geom_line()+
  # geom_line(aes(x = day, y = positive_tests, color = "Daily positive tests"))+
  geom_line(aes(x = day, y = D_a, color = "Cumulative ascertained deaths"))+
  scale_colour_manual(values=cbPalette) +
  labs(x = "Time (days)", y = "Cumulative ascertained cases and deaths")
cumulative_plot

prevalence_plot <- dd %>%
  ggplot(aes(x = day, y = prevalence_per_thousand, color = "Prevalence"))+
  geom_line() +
  scale_colour_manual(values=cbPalette) +
  labs(x = "Time (days)", y = "Cases per thousand population")
prevalence_plot


mortality_plot <- dd %>%
  ggplot(aes(x = day, y = daily_deaths, color = "Daily deaths")) +
  geom_line() +
  scale_colour_manual(values=cbPalette) +
  labs(x = "Time (days)", y = "People")
mortality_plot

inc_plot <- dd %>% ggplot(aes(x = day, y = incident_inf_cases, color = "Incident infectious \n cases per day")) +
  geom_line() +
  geom_line(aes(x = day, y = pos, color = "Newly confirmed cases per day")) +
  geom_line(aes(x = day, y = daily_deaths, color = "Daily deaths")) +
  geom_line(aes(x = day, y = daily_deaths_observed, color = "COVID-confirmed \n daily deaths"))+
  # geom_line(aes(x = day, y = prevalence_per_thousand, color = "Prevalence per thousand")) +
  scale_colour_manual(values=cbPalette) +
  labs(x = "Time (days)", y = "People")
inc_plot

prop_ascertained_plot <- dd %>%
  ggplot(aes(x = day, y = observed_prevalent_cases/prevalent_cases, color =
               "Proportion of true positives \n which are ascertained")) +
  geom_line() +
  scale_colour_manual(values=cbPalette) +
  labs(x = "Time (days)", y = "Proportion")
prop_ascertained_plot


prev_plot <- dd %>% ggplot(aes(x = day, y = observed_prevalent_cases, color = "Confirmed (Ascertained) \n prevalent cases"))+
  geom_line() +
  geom_line(aes(x = day, y = prevalent_cases, color = "True prevalent cases")) +
  scale_colour_manual(values=cbPalette) +
  geom_line(aes(x = day, y = tests_conducted, color = "Tests conducted")) +
  geom_line() +
  geom_line(aes( x = day, y = positive_tests, color = "Positive test results")) +
  geom_line(aes(x = day, y= max_daily_test_supply, color = "Maximum daily \n test supply")) +
  scale_y_continuous(trans = "log1p")+
  labs(x = "Time (days)", y = "People")
prev_plot

outbreak_plot <- dd %>%
  ggplot(aes(x = day, y = E, color = "Exposed")) +
  geom_line() +
  # geom_line(aes(x = day, y = S, color = "S")) +
  geom_line(aes(x = day, y = I_p + A_p, color = "Pre-symptomatic")) +
  geom_line(aes(x = day, y = I_a + A_a, color = "Asymptomatic")) +
  geom_line(aes(x = day, y = I_m + A_m, color = "Mildly symptomatic")) +
  geom_line(aes(x = day, y = I_c + A_c, color = "Critical")) +
  # geom_line(aes(x = day, y = R + R_a, color = "R")) +
  geom_line(aes(x = day, y = D_a + D, color = "Dead")) +
  scale_colour_manual(values=cbPalette) +
  labs(y = "Prevalent Cases", x = "Time (days)")
outbreak_plot

testing_plot <- dd %>%
  ggplot(aes(x = day, y = tests_conducted, color = "Tests conducted")) +
  geom_line() +
  geom_line(aes( x = day, y = positive_tests, color = "Positive test results")) +
  geom_line(aes(x = day, y= max_daily_test_supply, color = "Maximum daily \n test supply")) +
  scale_colour_manual(values=cbPalette) +
  labs(title = "Impact of positive tests on test demand", x = 'Time (days)', y = "Tests") 
testing_plot



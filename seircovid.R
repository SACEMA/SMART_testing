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

variables = 25 
SEIR = array(dim = c(length(timesteps),variables))
colnames(SEIR) = c("S", "E", "A", "P", "M", "C", "R_P", "R_N", "D", 
                   "S_w", "E_w", "A_w", "P_w", "M_w", "C_w", "R_Pw", "R_Nw", "D_w", 
                   "A_a", "P_a", "M_a", "C_a", "R_Pa", "R_Na", "D_a")

#####initial population sizes######
S0 = 9997
E0 = 3
A0 = 0
P0 = 0
M0 =  0
C0 =  0
R_P0 = 0
R_N0 = 0
D0 = 0
#waiting compartment initial popualtion sizes
S_w0 = 0
E_w0 = 0
A_w0 = 0
P_w0 = 0
M_w0 =  0
C_w0 =  0
R_Pw0 = 0
R_Nw0 = 0
D_w0 = 0
#ascertained compartments initial population sizes
A_a0 = 0
P_a0 = 0
M_a0 =  0
C_a0 =  0
R_Pa0 = 0
R_Na0 = 0
D_a0 = 0

SEIR[1,] = c(S0, E0, A0, P0, M0, C0, R_P0, R_N0, D0, S_w0, E_w0, A_w0, P_w0, M_w0, C_w0, R_Pw0, R_Nw0, D_w0, A_a0, P_a0, M_a0, C_a0, R_Pa0, R_Na0, D_a0)
N0 = sum(SEIR[1,])


########disease parameters########

# sigma = progression rates between infectious compartments 
# gamma = recovery rates
# psi = ascertainments rates
# mu = death rates
# omega = 1/ waiting time for results 

beta = 0.8 
alpha = .25  #2,7 proportion asymptomatic
m = 1/3.69     #2,7 1/latency duration

sigma_p = 1/1.75   #3,11 presymptomatic to mild
sigma_m = 1/32  #4,12 mild to critical    # citing The Novel Coronavirus Pneumonia Emergency Response Epidemiology Team. The Epidemiological Characteristics of an Outbreak of 2019 Novel Coronavirus Diseases (COVID-19) — China, 2020[J]. China CDC Weekly, 2020, 2(8): 113-122. doi: 10.46234/ccdcw2020.032 shu

gamma_m = 1/8  #5,10 mild to recovery positive     # about 1/5 symptomatic cases become critical
gamma_c = 1/7  #6,13 critical to recovery positive
gamma_a = 1/14  #8,9 asymptomatic to recovered pos sitive
gamma_r = 1/3  #recover to negative 
  
  
mu_c = 1/40   #20,14 critical to death      # about a quarter of critical cases ("severe + critical" in the cited work) die

omega = 1/4 #1/waiting time for results

r = 1         #reduction in "infectiousness" due to ascertainment




########### testing ##################
tests_conducted = rep(100, length(timesteps))
max_daily_test_supply = rep(1000, length(timesteps))

testing_demand_feedback_strength = 5
testing_demand_lag = 4 #number of days

# proportion of people being ascertained (demand) * some factor

compartments_to_test = c("S", "E", "A", "P", "M", "C", "R_P", "R_N")
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

relHaz = matrix(nrow = length(timesteps), ncol = 8)
colnames(relHaz) = c("S", "E", "A", "P", "M", "C", "R_N", "R_P")
relHaz = relHaz %>% data.frame()

for(i in 1:101){
  relHaz[i,c("S", "E", "A", "P", "M", "C", "R_N", "R_P")] = c(1 , 1, 1, 1, 1, 1, 1, 1)}





for(t_index in seq(2,nrow(SEIR))){
  
  N = sum(SEIR[t_index-1,1:(ncol(SEIR)-2)])
  
  S = SEIR[t_index - 1, "S"]
  E = SEIR[t_index - 1, "E"]
  A = SEIR[t_index - 1, "A"]
  P = SEIR[t_index - 1, "P"]
  M = SEIR[t_index - 1, "M"]
  C = SEIR[t_index - 1, "C"]
  R_P = SEIR[t_index - 1, "R_P"]
  R_N = SEIR[t_index - 1, "R_N"]
  D = SEIR[t_index - 1, "D"]
  
  S_w = SEIR[t_index - 1, "S_w"]
  E_w = SEIR[t_index - 1, "E_w"]
  A_w = SEIR[t_index - 1, "A_w"]
  P_w = SEIR[t_index - 1, "P_w"]
  M_w = SEIR[t_index - 1, "M_w"]
  C_w = SEIR[t_index - 1, "C_w"]
  R_Pw = SEIR[t_index - 1, "R_Pw"]
  R_Nw = SEIR[t_index - 1, "R_Nw"]
  D_w = SEIR[t_index - 1, "D_w"]
  
  A_a = SEIR[t_index - 1, "A_a"]
  P_a = SEIR[t_index - 1, "P_a"]
  M_a = SEIR[t_index - 1, "M_a"]
  C_a = SEIR[t_index - 1, "C_a"]
  R_Pa = SEIR[t_index - 1, "R_Pa"]
  R_Na = SEIR[t_index - 1, "R_Na"]
  D_a = SEIR[t_index - 1, "D_a"]
  
  
  lambda = beta*(((A + P + M + C)+r*(A_a + P_a + M_a + C_a))/N)
  lambda_w = 0   #????????
  
  #####model equations ####

  ####change in disease process and waiting for results ####
  change_S = -lambda*S + omega*S_w
  change_E = lambda*S - alpha*m*E - (1-alpha)*m*E + omega*E_w
  change_A = alpha*m*E + alpha*m*E_w - gamma_a*A
  change_P = (1-alpha)*m*E+ (1-alpha)*m*E_w - sigma_p*P
  change_M = sigma_p*P - sigma_m*M - gamma_m*M
  change_C = sigma_m*M -mu_c*C -gamma_c*C
  change_R_P = gamma_c*C + gamma_m*M + gamma_a*A - gamma_r*R_P
  change_R_N = gamma_r*R_P
  change_D = mu_c*C
  
  change_S_w = -lambda_w*S_w -omega*S_w
  change_E_w = -alpha*m*E_w - (1-alpha)*m*E_w + lambda_w*S_w -omega*E_w
  change_A_w = -gamma_a*A_w -omega*A_w
  change_P_w = -sigma*p*P_w  -omega*P_w
  change_M_w = sigma_p*P_w- sigma_m*M_w - gamma_m*M_w  -omega*M_w
  change_C_w = sigma_m*M_w - gamma_c*C_w - mu_c*C_w  -omega*C_w
  change_R_Pw = gamma_m*M_w + gamma_a*A_w + gamma_c*C_w  -omega*R_Pw
  change_R_Nw = -omega*R_Nw
  change_D_w =  mu_c*C_w -omega*D_w
    
  change_A_a =  omega*A_w
  change_P_a = -sigma_p*P_a - gamma_a*P_a + omega*P_w
  change_M_a = -sigma_m*M_a - gamma_m*M_a + sigma_p*P_a + omega*M_w
  change_C_a = sigma_m*M_a -gamma_c*C_a - mu_c*C_a + omega*C_w
  change_R_P_a = gamma_c*C_a + gamma_m*M_a + gamma_a*P_a + omega*R_Pw
  change_R_N_a = omega*R_Nw
  change_D_a = mu_c*C_a + omega*D_w
  
  
  rateofchange_diseaseandwaiting <- c(change_S, change_E, change_A, change_P, change_M, change_C, change_R_P, change_R_N, change_D,
                                      change_S_w, change_E_w, change_A_w, change_P_w, change_M_w, change_C_w, change_R_Pw, change_R_Nw, change_D_w,
                                                              change_A_a, change_P_a, change_M_a, change_C_a, change_R_Pa, change_R_Na, change_D_a)
  
  SEIR[t_index,] = SEIR[t_index-1,] + rateofchange_diseaseandwaiting *1/timestep_reduction
  
  
  
  
  
  ###### calc number of samples to be collected ######
  elibible_pop = sum(SEIR[t_index, names(relHaz)]) # this feels messier than just defining relHaz to be zero for the ascertained and dead classes
  eligible_pop_t = c(eligible_pop_t, elibible_pop)
  
  if(elibible_pop < tests_conducted[t_index]){
    print(sprintf("%s tests planned, but only %s individuals with nonzero hazards of being tested", tests_conducted[t_index], elibible_pop))
    tests_conducted[t_index] = elibible_pop
  }
  tested_per_group = assignTests(numTests = tests_conducted[t_index], state = SEIR[t_index, names(relHaz)], relHaz = relHaz[t_index,])
  
  tested[t_index, ] <- tested_per_group
  neg<-append(neg, sum(tested[t_index,"S"],tested[t_index,"E"],tested[t_index,"R_N"]))
  pos<-append(pos, sum(tested[t_index,"A"],tested[t_index,"P"],tested[t_index,"M"],tested[t_index,"C"],tested[t_index,"R_P"] ))
  incident_cases <- append(incident_cases, S*lambda)
  incident_inf_cases <- append(incident_inf_cases,  m*E)
  prevalent_cases <- append(prevalent_cases, sum(A, A_a, P, P_a, M, M_a, C, C_a))
  observed_prevalent_cases <- append(observed_prevalent_cases, sum(A, P, M, C))
  
  
  
  ###### test samples collected changes ####
  change_sample_S = -tested[t_index,"S"] 
  change_sample_E = -tested[t_index,"E"] 
  change_sample_A = -tested[t_index,"A"] 
  change_sample_P = -tested[t_index,"P"] 
  change_sample_M = -tested[t_index,"M"] 
  change_sample_C = -tested[t_index,"C"] 
  change_sample_R_P = -tested[t_index,"R_P"] 
  change_sample_R_N = -tested[t_index,"R_N"] 
  change_sample_D = 0
  
  change_sample_S_w = tested[t_index,"S"] 
  change_sample_E_w = tested[t_index,"E"] 
  change_sample_A_w = tested[t_index,"A"] 
  change_sample_P_w = tested[t_index,"P"] 
  change_sample_M_w = tested[t_index,"M"] 
  change_sample_C_w = tested[t_index,"C"] 
  change_sample_R_Pw = tested[t_index,"R_P"] 
  change_sample_R_Nw = tested[t_index,"R_N"] 
  change_sample_D_w =  0
  
  change_sample_A_a = 0
  change_sample_P_a = 0
  change_sample_M_a = 0
  change_sample_C_a = 0
  change_sample_R_P_a = 0
  change_sample_R_N_a = 0
  change_sample_D_a = 0

  rates_change_samples <- c(change_sample_S, change_sample_E, change_sample_A, change_sample_P, change_sample_M, change_sample_C, change_sample_R_P, change_sample_R_N, change_sample_D,
       change_sample_S_w, change_sample_E_w, change_sample_A_w, change_sample_P_w, change_sample_M_w, change_sample_C_w, change_sample_R_Pw, change_sample_R_Nw, change_sample_D_w,
       change_sample_A_a, change_sample_P_a, change_sample_M_a, change_sample_C_a, change_sample_R_Pa, change_sample_R_Na, change_sample_D_a)

  SEIR[t_index, ] = SEIR[t_index, ] + rates_change_samples
  
  
  
  #### sample collection demand feedback ######
  future_test_demand = pos[length(pos)] * testing_demand_feedback_strength
  tests_conducted[t_index + testing_demand_lag] =
    min(tests_conducted[t_index + testing_demand_lag] +
          future_test_demand, max_daily_test_supply[t_index + testing_demand_lag])
  

}

tests_conducted = tests_conducted[1:length(timesteps)] #dirty fix for demand driven testing making this vector too long

# n_alive = SEIR %>%
#   data.frame() %>%
#   select(-D, -D_a) %>%
#   rowSums()
# 
# dd <- SEIR %>%
#   data.frame() %>%
#   mutate(positive_tests = pos, day = 1:nrow(SEIR)) %>%
#   mutate(incident_cases = incident_cases) %>%
#   mutate(incident_inf_cases = incident_inf_cases) %>%
#   mutate(tests_conducted = tests_conducted) %>%
#   mutate(prevalent_cases = rowSums(SEIR[,c("A_p","A_a","A_m","A_c","I_p","I_a","I_m","I_c")])) %>%
#   mutate(observed_prevalent_cases = rowSums(SEIR[,c("A_p","A_a","A_m","A_c")])) %>%
#   mutate(eligible_pop = eligible_pop_t ) %>% #rowSums(SEIR[,c("S","E","I_p","I_a","I_m","I_c","R")])
#   mutate(prevalent_cases_non_ascertained = rowSums(SEIR[,c("I_p","I_a","I_m","I_c")])) %>%
#   mutate(n_alive = n_alive) %>%
#   mutate(prevalence_per_thousand = 1000 * prevalent_cases/n_alive) %>%
#   mutate(daily_deaths = c(0, diff(D)+diff(D_a))) %>%
#   mutate(daily_deaths_unobserved = c(0, diff(D))) %>%
#   mutate(daily_deaths_observed = c(0, diff(D_a))) %>%
#   mutate(cumulative_confirmed_cases = cumsum(positive_tests)) %>%
#   mutate(prop_positive = positive_tests/tests_conducted)%>%
#   mutate(cumulative_incidence = cumsum(incident_cases)) %>%
#   mutate(max_daily_test_supply = max_daily_test_supply)
# 
# testing_plot <- dd %>%
#   ggplot(aes(x = day, y = tests_conducted, color = "Tests conducted")) +
#   geom_line() +
#   geom_line(aes( x = day, y = positive_tests, color = "Positive test results")) +
#   geom_line(aes(x = day, y= max_daily_test_supply, color = "Maximum daily \n test supply")) +
#   labs(title = "Impact of positive tests on test demand", x = 'Time (days)', y = "Tests")
# testing_plot
# 
# test_efficacy_plot <- dd %>%
#   ggplot(aes(x = day, y = prevalent_cases_non_ascertained, color = 'Prevalent cases, non-ascertained')) +
#   geom_line() +
#   geom_line(aes(x = day, y = eligible_pop, color = 'Population eligible for testing'))
# test_efficacy_plot
# 
# test_efficacy_plot_relative <- dd %>%
#   ggplot(aes(x = day, y = prevalent_cases_non_ascertained/eligible_pop, color = 'Prevalence among \n people eligible for testing')) +
#   geom_line() + 
#   geom_line(aes(x = day, y = prop_positive, color = 'Proportion positive'))
# test_efficacy_plot_relative
# 
# prop_pos_plot <- dd %>%
#   ggplot(aes(x = day, y = prop_positive, color = "Proportion of tests \n which come back positive")) +
#   geom_line() +
#   geom_line(aes(x= day, y = prevalence_per_thousand/1000, color = "Prevalence per \n million")) 
# prop_pos_plot
# 
# cumulative_plot <- dd %>%
#   ggplot(aes(x = day, y = cumulative_confirmed_cases, color = "Cumulative ascertained cases")) +
#   geom_line()+
#   # geom_line(aes(x = day, y = positive_tests, color = "Daily positive tests"))+
#   geom_line(aes(x = day, y = D_a, color = "Cumulative ascertained deaths"))+
#   labs(x = "Time (days)", y = "Cumulative ascertained cases and deaths")
# cumulative_plot
# 
# prevalence_plot <- dd %>%
#   ggplot(aes(x = day, y = prevalence_per_thousand, color = "Prevalence"))+
#   geom_line() +
#   labs(x = "Time (days)", y = "Cases per thousand population")
# prevalence_plot
# 
# outbreak_plot <- dd %>%
#   ggplot(aes(x = day, y = E, color = "Exposed")) +
#   geom_line() +
#   # geom_line(aes(x = day, y = S, color = "S")) +
#   geom_line(aes(x = day, y = I_p + A_p, color = "Pre-symptomatic")) +
#   geom_line(aes(x = day, y = I_a + A_a, color = "Asymptomatic")) +
#   geom_line(aes(x = day, y = I_m + A_m, color = "Mildly symptomatic")) +
#   geom_line(aes(x = day, y = I_c + A_c, color = "Critical")) +
#   # geom_line(aes(x = day, y = R + R_a, color = "R")) +
#   geom_line(aes(x = day, y = D_a + D, color = "Dead")) +
#   labs(y = "Prevalent Cases", x = "Time (days)")
# outbreak_plot
# 
# mortality_plot <- dd %>%
#   ggplot(aes(x = day, y = daily_deaths, color = "Daily deaths")) +
#   geom_line() +
#   labs(x = "Time (days)", y = "People")
# mortality_plot
# 
# inc_plot <- dd %>% ggplot(aes(x = day, y = incident_inf_cases, color = "Incident infectious \n cases per day")) +
#   geom_line() +
#   geom_line(aes(x = day, y = pos, color = "Newly confirmed cases per day")) +
#   geom_line(aes(x = day, y = daily_deaths, color = "Daily deaths")) +
#   geom_line(aes(x = day, y = daily_deaths_observed, color = "COVID-confirmed \n daily deaths"))+
#   # geom_line(aes(x = day, y = prevalence_per_thousand, color = "Prevalence per thousand")) +
#   labs(x = "Time (days)", y = "People")
# inc_plot
# 
# prev_plot <- dd %>% ggplot(aes(x = day, y = observed_prevalent_cases, color = "Confirmed (Ascertained) \n prevalent cases"))+
#   geom_line() +
#   geom_line(aes(x = day, y = prevalent_cases, color = "True prevalent cases")) +
#   labs(x = "Time (days)", y = "People")
# prev_plot

# prop_ascertained_plot <- dd %>% 
#   ggplot(aes(x = day, y = observed_prevalent_cases/prevalent_cases, color =
#                "Proportion of true positives \n which are ascertained")) +
#   geom_line() +
#   labs(x = "Time (days)", y = "Proportion")
# prop_ascertained_plot
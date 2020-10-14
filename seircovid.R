##Discrete time model
##covid testing

rm(list=ls(all=TRUE))
source("hazard.R")
library(tidyverse)
library(extraDistr)
scenario_name = "s2"
scenario_file <- sprintf("./scenarios/%s.R", scenario_name)

if(scenario_name %in% c('s3','s4')){
  source(scenario_file)
  # timesteps defined in scenario_file
  }else{
  tstart = 1
  tend = 250
  timestep_reduction = 1
  timesteps = seq(tstart, tend, 1/timestep_reduction)
  source(scenario_file)
  }


###how can we add parameter to either choose
#tests_conducted = dd_test$daily_tests  
### or dd_test_raw$daily_tests_raw



variables = 25
SEIR = array(dim = c(length(timesteps),variables))
colnames(SEIR) = c("S", "E", "A", "P", "M", "C", "R_P", "R_N", "D", 
                   "S_w", "E_w", "A_w", "P_w", "M_w", "C_w", "R_Pw", "R_Nw", "D_w", 
                   "A_a", "P_a", "M_a", "C_a", "R_Pa", "D_a", "daily_deaths")


#####initial population sizes######
S0 = 58000000 - 50
E0 = 50
A0 = 0
P0 = 0
M0 =  0
C0 =  0
R_P0 = 0
R_N0 = 0
D0 = 0
dailydeaths0 = 0
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
D_a0 = 0

SEIR[1,] = c(S0, E0, A0, P0, M0, C0, R_P0, R_N0, D0, S_w0, E_w0, A_w0, P_w0, M_w0, C_w0, R_Pw0, R_Nw0, D_w0, A_a0, P_a0, M_a0, C_a0, R_Pa0, D_a0, dailydeaths0)
N0 = sum(SEIR[1,])


########disease parameters########

# sigma = progression rates between infectious compartments 
# gamma = recovery rates
# psi = ascertainments rates
# mu = death rates
# omega = 1/ waiting time for results 

beta = 0.3
alpha = .25  #2,7 proportion asymptomatic
m = 0.25 # 1/3.69     #2,7 1/latency duration
m_w = 0.25
omega_E = 0.25

sigma_p = 1/1.75   #3,11 presymptomatic to mild
sigma_m = 1/32  #4,12 mild to critical    # citing The Novel Coronavirus Pneumonia Emergency Response Epidemiology Team. The Epidemiological Characteristics of an Outbreak of 2019 Novel Coronavirus Diseases (COVID-19) — China, 2020[J]. China CDC Weekly, 2020, 2(8): 113-122. doi: 10.46234/ccdcw2020.032 shu

gamma_m = 1/7  #5,10 mild to recovery positive     # about 1/5 symptomatic cases become critical
gamma_c = 1/7  #6,13 critical to recovery positive
gamma_a = 1/14  #8,9 asymptomatic to recovered pos sitive 
gamma_r = 1/10  #recover to negative

zeta =  4# strength of phenomenological heterogeneity

mu_c = 1/40 #20,14 critical to death      # about a quarter of critical cases ("severe + critical" in the cited work) die

omega = 1/4 #1/waiting time for results

r = 1  #reduction in "infectiousness" due to ascertainment

rateofchange_diseaseandwaiting = c()
#to track the number of people moving into the dead compartment each day 
people_died_eachday = c(0)


########### testing ##################

# proportion of people being ascertained (demand) * some factor

####Creating arrays/ lists for outputs####

pos <- c(0)
neg <- c(tests_conducted[1]) #initilaize the first day with the base of 100 tests???

tested[1,] = 0 #no-one tetsed on the first day 
eligible_pop_t = c(sum(SEIR[1,compartments_to_test]))

incident_infections = c(0)
incident_in_cases = c(0)

total_prevalent_cases = c(0)
waiting_prevalent_cases  = c(0)
ascertained_prevalent_cases =c(0)
non_ascertained_prevalent_cases =c(0)

cumulative_observed_cases = c(0)
cumulative_observed_deaths = c(0)

waitingcompartments <- c("S_w", "E_w", "A_w", "P_w", "M_w", "C_w", "R_Pw", "R_Nw", "D_w")
test_results_returned <- array(dim = c(length(timesteps), length(waitingcompartments)))
test_results_returned[1,] <- 0
colnames(test_results_returned) = waitingcompartments
colnames(test_results_returned)<- c("S_w", "E_w", "A_w", "P_w", "M_w", "C_w", "R_Pw", "R_Nw", "D_w")

n_tmp <- c(N0,rep(0,length(timesteps) - 1))

for(t_index in seq(2,nrow(SEIR))){
  N = sum(SEIR[t_index-1,]) - SEIR[t_index-1, "D_a"] - SEIR[t_index-1, "D_w"] - SEIR[t_index - 1, "D"]
  n_tmp[t_index] = sum(SEIR[t_index - 1,1:24])
  colnames(SEIR)
  

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
  D_a = SEIR[t_index - 1, "D_a"]
  
  
  lambda = beta*(((A + P + M + C)+r*(A_a + P_a + M_a + C_a))) 
  # N is shifted into the equations below for simplicity in implementing phenomenological heterogeneity
  lambda_w = lambda / r
  
  #####model equations ####

  ####change in disease process and arrival of test results ####
  change_S = - lambda*(S/N)^(1 + zeta) + omega*S_w
  change_E = lambda*(S/N)^(1 + zeta) - alpha*m*E - (1-alpha)*m*E + omega_E*E_w
  change_A = - gamma_a*A + alpha*m*E + alpha*m_w*E_w 
  change_P = (1-alpha)*m*E  - sigma_p*P + (1-alpha)*m_w*E_w
  change_M = sigma_p*P - sigma_m*M - gamma_m*M
  change_C = sigma_m*M - mu_c*C - gamma_c*C
  change_R_P = gamma_c*C + gamma_m*M + gamma_a*A - gamma_r*R_P
  change_R_N = gamma_r*R_P + omega*R_Nw
  change_D = mu_c*C
  
  change_S_w = - lambda_w*(S_w/N)^(1 + zeta) - omega*S_w
  change_E_w = + lambda_w*(S_w/N)^(1 + zeta) - omega_E*E_w - alpha*m_w*E_w - (1-alpha)*m_w*E_w
  change_A_w = - gamma_a*A_w - omega*A_w
  change_P_w = -sigma_p*P_w  - omega*P_w
  change_M_w = sigma_p*P_w - sigma_m*M_w - gamma_m*M_w  - omega*M_w
  change_C_w = sigma_m*M_w - gamma_c*C_w - mu_c*C_w  - omega*C_w
  change_R_Pw = gamma_m*M_w + gamma_a*A_w + gamma_c*C_w  - omega*R_Pw
  change_R_Nw = - omega*R_Nw
  change_D_w =  mu_c*C_w - omega*D_w
    
  change_A_a =  omega*A_w - gamma_a*A_a
  change_P_a = -sigma_p*P_a  + omega*P_w
  change_M_a = -sigma_m*M_a - gamma_m*M_a + sigma_p*P_a + omega*M_w
  change_C_a = sigma_m*M_a -gamma_c*C_a - mu_c*C_a + omega*C_w
  change_R_Pa = gamma_c*C_a + gamma_m*M_a  + omega*R_Pw + gamma_a*A_a
  change_D_a = mu_c*C_a + omega*D_w
  
  test_results_returned[t_index, ] <- c(omega*S_w, omega_E*E_w + m_w*E_w, omega*A_w, 
                                        omega*P_w, omega*M_w, omega*C_w, 
                                        omega*R_Pw, omega*R_Nw, omega*D_w)
  
  rateofchange_diseaseandwaiting <- c(change_S[[1]], change_E[[1]], change_A[[1]], change_P[[1]],
                                      change_M[[1]], change_C[[1]], change_R_P[[1]],
                                      change_R_N[[1]], change_D[[1]], change_S_w[[1]],
                                      change_E_w[[1]], change_A_w[[1]], change_P_w[[1]],
                                      change_M_w[[1]], change_C_w[[1]], change_R_Pw[[1]],
                                      change_R_Nw[[1]], change_D_w[[1]], change_A_a[[1]],
                                      change_P_a[[1]], change_M_a[[1]], change_C_a[[1]],
                                      change_R_Pa[[1]], change_D_a[[1]])
  

  SEIR[t_index, 1:24] = SEIR[t_index-1,1:24] + rateofchange_diseaseandwaiting *1/timestep_reduction
  SEIR[t_index, 25] = SEIR[t_index,9] - SEIR[t_index-1,9]
  
  ###### calc number of samples to be collected ######
  elibible_pop = sum(SEIR[t_index, names(relHaz)] * as.numeric( relHaz[t_index,] > 0)) # this feels messier than just defining relHaz to be zero for the ascertained and dead classes
  eligible_pop_t = c(eligible_pop_t, elibible_pop)
  if(elibible_pop < tests_conducted[t_index]){
    print(sprintf("%s tests planned, but only %s individuals with nonzero hazards of being tested", tests_conducted[t_index], elibible_pop))
    tests_conducted[t_index] = elibible_pop - 0.01
  }
  
  tested_per_group = assignTests(numTests = tests_conducted[t_index], state = SEIR[t_index, names(relHaz)], relHaz = relHaz[t_index,])
  tested[t_index, ] <- tested_per_group
  neg <- append(neg, sum(tested[t_index,"S"],tested[t_index,"E"],tested[t_index,"R_N"]))
  pos <- append(pos, sum(tested[t_index,"A"],tested[t_index,"P"],tested[t_index,"M"],tested[t_index,"C"],tested[t_index,"R_P"] ))
  
  ###### test samples collected changes ####
  change_sample_S = -tested[t_index,"S"] 
  change_sample_E = -tested[t_index,"E"] 
  change_sample_A = -tested[t_index,"A"] 
  change_sample_P = -tested[t_index,"P"] 
  change_sample_M = -tested[t_index,"M"] 
  change_sample_C = -tested[t_index,"C"] 
  change_sample_R_P = -tested[t_index,"R_P"] 
  change_sample_R_N = -tested[t_index,"R_N"] 
  change_sample_D = - tested[t_index,"daily_deaths"]
  
  change_sample_S_w = tested[t_index,"S"] 
  change_sample_E_w = tested[t_index,"E"] 
  change_sample_A_w = tested[t_index,"A"] 
  change_sample_P_w = tested[t_index,"P"] 
  change_sample_M_w = tested[t_index,"M"] 
  change_sample_C_w = tested[t_index,"C"] 
  change_sample_R_Pw = tested[t_index,"R_P"] 
  change_sample_R_Nw = tested[t_index,"R_N"] 
  change_sample_D_w =  tested[t_index,"daily_deaths"] 
  
  change_sample_A_a = 0
  change_sample_P_a = 0
  change_sample_M_a = 0
  change_sample_C_a = 0
  change_sample_R_Pa = 0
  change_sample_D_a = 0

  rates_change_samples <- c(change_sample_S, change_sample_E, change_sample_A, change_sample_P, change_sample_M, change_sample_C, change_sample_R_P, change_sample_R_N, change_sample_D,
       change_sample_S_w, change_sample_E_w, change_sample_A_w, change_sample_P_w, change_sample_M_w, change_sample_C_w, change_sample_R_Pw, change_sample_R_Nw, change_sample_D_w,
       change_sample_A_a, change_sample_P_a, change_sample_M_a, change_sample_C_a, change_sample_R_Pa, change_sample_D_a,0)

  SEIR[t_index,] = SEIR[t_index,] + rates_change_samples
  
  #### sample collection demand feedback ######
  future_test_demand = pos[length(pos)] * testing_demand_feedback_strength
  tests_conducted[t_index + testing_demand_lag] =
    min(tests_conducted[t_index + testing_demand_lag] +
          future_test_demand, max_daily_test_supply[t_index + testing_demand_lag])
  
  #### some outputs to save #####
  incident_infections <- append(incident_infections,
                           lambda * (S / N)^(1 + zeta) +
                             lambda_w *(S_w / N)^(1 + zeta)) #what about S_w ?
  # incident_inf_cases <- append(incident_inf_cases,  m*E) #what about E_w ?
  
  total_prevalent_cases <- append(total_prevalent_cases, sum(A, A_w, A_a, P, P_w, P_a, M, M_w, M_a, C, C_w, C_a))
  waiting_prevalent_cases <- append(waiting_prevalent_cases, sum(A_w, P_w, M_w, C_w))
  ascertained_prevalent_cases <- append(ascertained_prevalent_cases, sum(A_a, P_a, M_a, C_a))
  non_ascertained_prevalent_cases <-append(non_ascertained_prevalent_cases, sum (A,P, M, C))
  
  
  #### sample accumulator ####
  #move into waiting compartments
  #rate of ascertainment = psi
  
  # prevalence of waiting compartments
  
  #how to plot the backlog?
  
  ####results accumulator#####
  #move into ascertained compartments
  #rate of test reults = omega
}

tests_conducted = tests_conducted[1:length(timesteps)] #dirty fix for demand driven testing making this vector too long


n_alive = SEIR %>%
   data.frame() %>%
   select(-D, -D_a, -D_w) %>%
   rowSums()
 
dd <- SEIR %>%
  data.frame() %>%
  mutate(positive_samples_collected = pos, day = 1:nrow(SEIR)) %>%
  mutate(incident_infections = incident_infections) %>%  #S*lambda
  # mutate(incident_inf_cases = incident_inf_cases) %>% #m*E
  mutate(tests_conducted = tests_conducted) %>%
  mutate(total_prevalent_cases = total_prevalent_cases) %>%
  mutate(waiting_prevalent_cases = waiting_prevalent_cases) %>%
  mutate(ascertained_prevalent_cases = ascertained_prevalent_cases) %>%
  mutate(non_ascertained_prevalent_cases = non_ascertained_prevalent_cases) %>%
  mutate(eligible_pop = eligible_pop_t ) %>% 
  mutate(n_alive = n_alive) %>%
  mutate(prevalence_per_thousand = 1000 * total_prevalent_cases/n_alive) %>%
  mutate(daily_deaths = c(0, diff(D) + diff(D_a) + diff(D_w))) %>%
  mutate(daily_deaths_unobserved = c(0, diff(D))) %>%
  mutate(daily_deaths_observed = c(0, diff(D_a)+diff(D_w))) %>%
  mutate(cumulative_confirmed_cases = cumsum(positive_samples_collected)) %>%
  mutate(prop_positive = positive_samples_collected/tests_conducted)%>%
  mutate(cumulative_incidence = cumsum(incident_infections)) %>%
  mutate(max_daily_test_supply = max_daily_test_supply) %>%
  mutate(positive_test_results_returned = 
           rowSums(test_results_returned[,c("A_w", "P_w", "M_w", "C_w", "R_Pw", "D_w")])) %>%
  mutate(test_results_returned = rowSums(test_results_returned))
  

colnames(test_results_returned)

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# test_efficacy_plot <- dd %>%
#   ggplot(aes(x = day, y = non_ascertained_prevalent_cases, color = 'Prevalent cases, non-ascertained')) +
#    geom_line() +
#    geom_line(aes(x = day, y = eligible_pop, color = 'Population eligible for testing'))
# test_efficacy_plot

# test_efficacy_plot_relative <- dd %>%
#   ggplot(aes(x = day, y = prevalent_cases_non_ascertained/eligible_pop, color = 'Prevalence among \n people eligible for testing')) +
#   geom_line() +
#   geom_line(aes(x = day, y = prop_positive, color = 'Proportion positive'))
# test_efficacy_plot_relative
testing_plot <- dd %>%
  ggplot(aes(x = day, y = tests_conducted, color = "Samples collected")) +
  geom_line() +
  geom_line(aes( x = day, y = positive_samples_collected, color = "Positive samples collected")) +
  # geom_line(aes(x = day, y= max_daily_test_supply, color = "Maximum daily \n test supply")) +
  geom_line(aes(x = day, y = test_results_returned, color = "Test results returned"))+
  geom_line(aes(x = day, y = positive_test_results_returned, color = "Positive test results")) + 
  labs(title = "Tests conducted", x = 'Time (days)', y = "Tests") +
  theme(legend.justification = c(0,1),
        legend.position = c(0,1),
        legend.title = element_blank()) +
  coord_cartesian(xlim = c(0, 190)) +
  scale_colour_manual(values=cbbPalette)
testing_plot

observed_prevalence_plot <- dd %>%
  ggplot(aes(x = day, y = ascertained_prevalent_cases/total_prevalent_cases, color = "(Confirmed prevalence)/(true prevalence")) +
  geom_line() +
  theme(legend.position = "none", legend.title = element_blank()) +
  coord_cartesian(xlim = c(0, 190)) +
  labs(x = "Day", y = "Proportion of prevalent cases \n which are confirmed positive") +
  scale_colour_manual(values=cbbPalette)
observed_prevalence_plot

# ascertainment_plot <- dd %>%
#   ggplot() + 
#   geom_line(aes( x = , y = , color = ))

prop_pos_plot <- dd %>%
  ggplot(aes(x = day, y = prop_positive, color = "Proportion of tests \n which come back positive")) +
  geom_line() +
  geom_line(aes(x= day, y = prevalence_per_thousand/1000, color = "Prevalence")) +
  geom_line(aes(x = day, y = ascertained_prevalent_cases/n_alive, color = "Ascertained prevalence")) +
  labs(y = 'Proportion', x = "Day") +
  scale_colour_manual(values=cbbPalette)+
  coord_cartesian(xlim = c(0, 190)) +
  theme(legend.justification = c(0,1),
        legend.position = c(0,1),
        legend.title = element_blank())
prop_pos_plot

cumulative_plot <- dd %>%
  ggplot(aes(x = day, y = cumulative_confirmed_cases,
            color = "Cumulative ascertained cases")) +
  geom_line()+
  # geom_line(aes(x = day, y = positive_samples_collected, color = "Daily positive tests"))+
  geom_line(aes(x = day, y = D_a, color = "Cumulative ascertained deaths"))+
  labs(x = "Time (days)", y = "Cumulative ascertained cases and deaths") +
  scale_colour_manual(values=cbbPalette)+
  coord_cartesian(xlim = c(0, 190))+
  theme(legend.justification = c(0,1),
        legend.position = c(0,1),
        legend.title = element_blank())
cumulative_plot


prevalence_plot <- dd %>%
   ggplot(aes(x = day, y = prevalence_per_thousand, color = "Prevalence")) +
   geom_line() +
   labs(x = "Time (days)", y = "Cases per thousand population",
        title = "Prevalence per thousand population")+
  theme(legend.title = element_blank(), legend.position = 'none') +
  coord_cartesian(xlim = c(0, 190)) +
  scale_colour_manual(values=cbbPalette)
prevalence_plot

 
outbreak_plot <- dd %>%
  ggplot(aes(x = day, y = E, color = "Exposed")) +
  geom_line() +
  geom_line(aes(x = day, y = P + P_w + P_a, color = "Pre-symptomatic")) +
  geom_line(aes(x = day, y = A + A_w + A_a, color = "Asymptomatic")) +
  geom_line(aes(x = day, y = M + M_w + M_a, color = "Mildly symptomatic")) +
  geom_line(aes(x = day, y = C + C_w + C_a, color = "Critical")) +
  geom_line(aes(x = day, y = D + D_w + D_a, color = "Dead")) +
  labs(y = "Prevalent Cases", x = "Time (days)", title = "Overview") +
  scale_colour_manual(values=cbbPalette)+
  coord_cartesian(xlim = c(0, 190)) +
  theme(legend.justification = c(0,1),
        legend.position = c(0,1),
        legend.title = element_blank())
outbreak_plot

mortality_plot <- dd %>%
  ggplot(aes(x = day, y = daily_deaths, color = "Daily deaths")) +
  geom_line() +
  labs(x = "Time (days)", y = "People", title = "Mortality") +
  theme(legend.title = element_blank()) +
  scale_colour_manual(values=cbbPalette)+
  coord_cartesian(xlim = c(0, 190)) +
  theme(legend.justification = c(0,1),
        legend.position = c(0,1))
mortality_plot


# names(dd)

inc_plot <- dd %>% ggplot() +
  geom_line(aes(x = day, y = incident_infections, color = "Incident cases")) +
  geom_line(aes(x = day, y = positive_samples_collected, color = "Newly confirmed cases")) +
  geom_line(aes(x = day, y = daily_deaths, color = "Deaths")) +
  geom_line(aes(x = day, y = daily_deaths_observed, color = "COVID-confirmed deaths"))+
  # geom_line(aes(x = day, y = prevalence_per_thousand, color = "Prevalence per thousand")) +
  labs(x = "Day", y = "People") +
  coord_cartesian(xlim = c(0, 190)) +
  theme(legend.justification = c(0,1),
        legend.position = c(0,1))
inc_plot

ascertainment_cases_vs_deaths <- dd %>% ggplot() +
  geom_line(aes(x = day, y = positive_samples_collected / (incident_infections + 0.01), color = "Positive samples collected\nper incident case")) +
  geom_line(aes(x = day, y = daily_deaths_observed  / daily_deaths, color = "Daily proportion of deaths\nwith covid confirmation"))+ 
  # geom_line(aes(x = day, y = daily_deaths_unobserved / daily_deaths, color = "Daily proportion of deaths\nwithout covid confirmation"))+
  # geom_line(aes(x = day, y = (daily_deaths_observed + daily_deaths_unobserved) / daily_deaths, color = 'Sanity check')) +
  coord_cartesian(xlim = c(0, 190)) +
  labs(x = "Day", y = "Proportion") +
  theme(legend.justification = c(0,1),
        legend.position = c(0,1),
        legend.title = element_blank(),
        legend.background = element_rect(fill = 'transparent'))
  # scale_y_log10()
ascertainment_cases_vs_deaths

#prop_pos_plot
all_plot <- cowplot::plot_grid(outbreak_plot, ascertainment_cases_vs_deaths,cumulative_plot, observed_prevalence_plot, testing_plot, prevalence_plot, ncol = 2)
all_plot

ggsave( sprintf("./plots/scenario_%s_r%s_zeta%s_beta%s.pdf", scenario_name, r, zeta, beta), 
        plot = all_plot, 
        scale = 1,
        width = 20,
        height = 28,
        units = "in",
        device = "pdf")


# dd$positive_samples_collected / (dd$incident_infections + 1)







positives_percompartment_plot <- dd %>%
  ggplot(aes(x = day, y = P_a+P_w, color = "Pre-symptomatic")) +
  geom_line() +
  geom_line(aes(x = day, y = A_w + A_a, color = "Asymptomatic")) +
  geom_line(aes(x = day, y = M_w + M_a, color = "Mildly symptomatic")) +
  geom_line(aes(x = day, y = C_w + C_a, color = "Critical")) +
  geom_line(aes(x = day, y = D_w + D_a, color = "Dead")) +
  labs(y = "prevalent cases", x = "Time (days)", title = "Confirmed positive prevalent cases") +
  scale_colour_manual(values=cbbPalette)+
  coord_cartesian(xlim = c(0, 250)) +
  theme(legend.justification = c(0,1),
        legend.position = c(0,1),
        legend.title = element_blank())
positives_percompartment_plot



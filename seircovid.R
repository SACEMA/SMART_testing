##Discrete time model
##covid testing

#matrix setup
#rows=time
#columns=variables(disease compartments)

tstart=0
tend=100
timestep_reduction = 20 #basic timestep is days looking at days
timesteps = seq(tstart, tend, 1/timestep_reduction)

variables = 14 
SEIR = array(dim = c(length(timesteps),variables))
colnames(SEIR) = c("S", "E", "I_a", "I_aa","I_p", "I_ap", "I_m", "I_am", "I_c", "I_ac", "R", "R_a", "D", "D_a")

#initial population sizes
S0 = 970
E0 = 20
I_a0 = 0
I_p0 = 0
I_m0 =  0
I_c0 =  0
R0 = 0
D0 = 0

#ascertained compartments initial population sizes
I_aa0 = 0
I_ap0 = 0
I_am0 = 0
I_ac0 = 0
R_a0 = 0
D_a0 = 0

SEIR[1,] = c(S0, E0, I_a0, I_aa0, I_p0, I_ap0, I_m0, I_am0, I_c0, I_ac0, R0, R_a0, D0, D_a0 )


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



#lambda =  #1 this is a function of time so appears in loop
beta = 1 / timestep_reduction
alpha = .1  #2,7 exposed to asymptomatic
m = 0.1     #2,7

sigma_p = 1/4 * (1/timestep_reduction)   #3 presymptomatic to mild
sigma_m = 1/8 * (1/timestep_reduction)   #4 mild to critical symptoms = 10% of I_M
gamma_m = 1/9 * (1/timestep_reduction)   #5 mild to recovery 
gamma_c = 1/10 * (1/timestep_reduction)  #6 critical to recovery
gamma_a = 1/7 * (1/timestep_reduction)   #8 asymptomatic to recovered   ##psi_a + gamma_a = 1
mu_c = 1/6    * (1/timestep_reduction)   #20 critical to death

#####ascertained parameters
r = 0
gamma_til_a = 0#9
gamma_til_m = 0#10
sigma_til_p = 0#11
sigma_til_m = 0#12
gamma_til_c = 0#13
mu_til_c = 0#14
psi_p = 0#15
psi_m = 0#16
psi_c = 0 #17
#18
psi_a = 0#19   ##psi_a + gamma_a = 1

for(t_index in seq(2,nrow(SEIR))){
  N = sum(SEIR[t_index-1,])
  S = SEIR[t_index - 1, "S"]
  E = SEIR[t_index - 1, "E"]
  I_a = SEIR[t_index - 1, "I_a"]
  I_aa = SEIR[t_index - 1, "I_aa"]
  I_p = SEIR[t_index - 1, "I_p"]
  I_ap = SEIR[t_index - 1, "I_ap"]
  I_m = SEIR[t_index - 1, "I_m"]
  I_am = SEIR[t_index - 1, "I_am"]
  I_c = SEIR[t_index - 1, "I_c"]
  I_ac = SEIR[t_index - 1, "I_ac"]
  R = SEIR[t_index - 1, "R"]
  R_a = SEIR[t_index - 1, "R_a"]
  D = SEIR[t_index - 1, "D"]
  D_a = SEIR[t_index - 1, "D_a"]
  
  #lambda is a fucntion of time
  lambda = beta*(((I_a + I_p + I_m + I_c)+r*(I_aa + I_ap + I_am + I_ac))/N)
  
  #time=t+1
  SEIR[t_index, "S"] = S - lambda*S
  SEIR[t_index, "E"] = E + lambda*S - m*E
  SEIR[t_index, "I_a"] = I_a + (alpha*m)*E - (psi_a + gamma_a)*I_a
  SEIR[t_index, "I_aa"] = I_aa + psi_a*I_a - gamma_til_a*I_aa
  SEIR[t_index, "I_p"] = I_p + ((1-alpha)*m)*E - (sigma_p + psi_p)*I_p
  SEIR[t_index, "I_ap"] = I_ap + psi_p*I_p - sigma_til_p*I_ap
  SEIR[t_index, "I_m"] = I_m + sigma_p*I_p - (sigma_m + psi_m + gamma_m)*I_m
  SEIR[t_index, "I_am"] = I_am + sigma_til_p*I_ap + psi_m*I_m - (sigma_til_m + gamma_til_m)*I_am
  SEIR[t_index, "I_c"] = I_c + sigma_m*I_m - (gamma_c + psi_c + mu_c)*I_c
  SEIR[t_index, "I_ac"] = I_ac + psi_c*I_c + sigma_til_m*I_am - (gamma_til_c + mu_til_c)*I_ac
  SEIR[t_index, "R"] = R + gamma_a*I_a + gamma_m*I_m + gamma_c*I_c
  SEIR[t_index, "R_a"] = R_a + gamma_til_a * I_aa + gamma_til_m*I_am + gamma_til_c*I_ac
  SEIR[t_index, "D"] = D + mu_c*I_c
  SEIR[t_index, "D_a"] = D_a + mu_til_c*I_ac

}


plot(timesteps, SEIR[, "S"], type = 'l', col = 'blue', ylim = c(min(SEIR), max(SEIR)), xlim=c(0,130))
lines(timesteps, SEIR[, "E"], col = 'red')
lines(timesteps, SEIR[, "I_a"], col = 'green')
lines(timesteps, SEIR[, "I_p"], col = 'orange')
lines(timesteps, SEIR[, "I_m"], col = 'pink' )
lines(timesteps, SEIR[, "R"], col = 'violet')
lines(timesteps, SEIR[, "D"])

# legend(x = tmax- tmax/4, y = max(SEIR)*0.8)

legend(85, 900, legend=c("S", "E", "I_a", "I_p", "I_m" , "R"),
       col=c("blue", "red", "green", "orange", "pink", "violet"), lty=1, cex=0.8)

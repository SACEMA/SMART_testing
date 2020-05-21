##Discrete time model
##covid testing

#matrix setup
#rows=discrete time
#columns=variables (disease compartments)

tstart=0
tend=100
timestep_reduction = 2
timesteps = seq(tstart, tend, 1/timestep_reduction)

variables = 14 
SEIR = array(dim = c(length(timesteps),variables))
colnames(SEIR) = c("S", "E", "I_a", "A_a","I_p", "A_p", "I_m", "A_m", "I_c", "A_c", "R", "R_a", "D", "D_a")

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
A_a0 = 0
A_p0 = 0
A_m0 = 0
A_c0 = 0
R_a0 = 0
D_a0 = 0

SEIR[1,] = c(S0, E0, I_a0, A_a0, I_p0, A_p0, I_m0, A_m0, I_c0, A_c0, R0, R_a0, D0, D_a0 )


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
beta = 1 
alpha = .1  #2,7 exposed to asymptomatic
m = 0.1     #2,7

sigma_p = 1/4   #3,11 presymptomatic to mild
sigma_m = 1/8    #4,12 mild to critical symptoms = 10% of I_M
gamma_m = 1/9  #5,10 mild to recovery 
gamma_c = 1/10  #6,13 critical to recovery
gamma_a = 1/7  #8,9 asymptomatic to recovered   
mu_c = 1/6   #20,14 critical to death

#####ascertainment parameters
r = 0
psi_p = 0#15
psi_m = 0#16
psi_c = 0 #17
psi_a = 0#19   

for(t_index in seq(2,nrow(SEIR))){
  N = sum(SEIR[t_index-1,])
  
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
  
  change_S = - lambda*S
  change_E = lambda*S - m*E
  change_I_a = (alpha*m)*E - (psi_a + gamma_a)*I_a
  change_A_a = psi_a*I_a - gamma_a*A_a
  change_I_p = ((1-alpha)*m)*E - (sigma_p + psi_p)*I_p
  change_A_p = psi_p*I_p - sigma_p*A_p
  change_I_m = sigma_p*I_p - (sigma_m + psi_m + gamma_m)*I_m
  change_A_m = sigma_p*A_p + psi_m*I_m - (sigma_m + gamma_m)*A_m
  change_I_c = sigma_m*I_m - (gamma_c + psi_c + mu_c)*I_c
  change_A_c = psi_c*I_c + sigma_m*A_m - (gamma_c + mu_c)*A_c
  change_R = gamma_a*I_a + gamma_m*I_m + gamma_c*I_c
  change_R_a = gamma_a * A_a + gamma_m*A_m + gamma_c*A_c
  change_D = mu_c*I_c
  change_D_a = mu_c*A_c
  
  rateofchange<-c(change_S, change_E, change_I_a, change_A_a, change_I_p, change_A_p, change_I_m,
                  change_A_m, change_I_c,change_A_c, change_R, change_R_a, change_D,change_D_a)
  
  SEIR[t_index,]= SEIR[t_index-1,] + rateofchange*1/timestep_reduction
 
}


plot(timesteps, SEIR[, "S"], type = 'l', col = 'blue', ylim = c(min(SEIR), max(SEIR)), xlim=c(0,130))
lines(timesteps, SEIR[, "E"], col = 'red')
lines(timesteps, SEIR[, "I_a"], col = 'green')
lines(timesteps, SEIR[, "I_p"], col = 'orange')
lines(timesteps, SEIR[, "I_m"], col = 'pink' )
lines(timesteps, SEIR[, "I_c"], col = 'turquoise' )
lines(timesteps, SEIR[, "R"], col = 'violet')
lines(timesteps, SEIR[, "D"], col = 'black')
legend(105, 900, legend=c("S", "E", "I_a", "I_p", "I_m" , "I_c", "R", "D"),
       col=c("blue", "red", "green", "orange", "pink", "turquoise","violet", "black"), lty=1, cex=0.8)


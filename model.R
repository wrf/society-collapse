# based on Human And Nature DYnamics (HANDY) model by:
# Motesharrei et al (2012) A Minimal Model for Human and Nature Interaction
# https://www.sciencedirect.com/science/article/pii/S0921800914000615
#
# code made by WRF 2024-09-01

library(ggplot2)
library(gridExtra)

# min and max death factors
alpha_min = 0.01
alpha_max = 0.07

# birth rates for commoners and elites
BETA_Comm = 0.03
BETA_Elite = 0.03

# nature regeneration factor
GAMMA_REGROW = 0.01

# multiplier of consumption between commoners and elites
K_SALARY = 1

# max capacity of Nature
lambda_MAX_FOREST = 100

SUBSIST = 0.0005
rho_MIN_CONSUM = 0.005
X_Comm_start = 100
X_Elite_start = 0
y_start = lambda_MAX_FOREST
w_start = 0
BASE_DEPLETION = 6.67e-6
depletion = BASE_DEPLETION*1

max_time = 1000

### TRACKERS AND STARTING CONDITIONS
X_Comm_t = c(X_Comm_start)
X_Elite_t = c(X_Elite_start)
y_t = c(y_start)
w_t = c(w_start)
t = c(0)

### MODEL LOOP
for (i in 1:max_time){
  # calculate some dependent values
  w_thres = rho_MIN_CONSUM * X_Comm_t[i]  +  K_SALARY * rho_MIN_CONSUM * X_Elite_t[i]

  cons_Comm = min(1, (w_t[i]/w_thres) ) * SUBSIST * X_Comm_t[i]
  cons_Comm
  cons_Elite = min(1, (w_t[i]/w_thres) ) * SUBSIST * X_Elite_t[i] * K_SALARY
  cons_Elite
  alpha_Comm = alpha_min + max(0, 1-(cons_Comm/(SUBSIST * X_Comm_t[i]))) * (alpha_max-alpha_min)

  starv_Elite = cons_Elite/(SUBSIST * X_Elite_t[i]) # set to 0 if Na, from divide by 0
  alpha_Elite = alpha_min + max(0, 1-(ifelse(is.na(starv_Elite),0,starv_Elite))) * (alpha_max-alpha_min)
  alpha_Comm
  alpha_Elite
  
  # iterate 4 major equations in model
  X_Comm_new = X_Comm_t[i]  +  X_Comm_t[i] * BETA_Comm  -  
               X_Comm_t[i] * alpha_Comm
  X_Comm_new
  X_Elite_new = X_Elite_t[i]  +  X_Elite_t[i] * BETA_Elite  -  
                X_Elite_t[i] * ifelse(is.na(alpha_Elite),0,alpha_Elite)
  X_Elite_new
  y_new = y_t[i]  +  GAMMA_REGROW * y_t[i] * (lambda_MAX_FOREST-y_t[i])  -  
          depletion * X_Comm_t[i] * y_t[i]
  y_new
  w_new = w_t[i]  +  depletion*X_Comm_t[i]*y_t[i] - cons_Comm - cons_Elite
  w_new

  # track new values
  t = c(t, i)
  X_Comm_t = c(X_Comm_t, X_Comm_new)
  X_Elite_t = c(X_Elite_t, X_Elite_new)
  y_t = c(y_t, y_new)
  w_t = c(w_t, w_new)
}


# make plot
# plot(t, X_Comm_t, 
#      type='l', lwd=5, col="#0c81e0ff")
# lines(t, X_Elite_t, lwd=5, col="#7e0ce0ff")
# lines(t, y_t*500, lwd=5, col="#2e831eff")
# lines(t, w_t*200, lwd=5, col="#e08e0cff")


pop_counts = data.frame(time=t, commoners=X_Comm_t, elites=X_Elite_t, nature=y_t, wealth=w_t)
g1 = ggplot(pop_counts, aes(x=time, y=commoners)) +
  scale_x_continuous(expand=c(0,0)) +
  geom_line(linewidth = 3, colour="#0c81e0ff") + 
  geom_line(aes(y=elites), linewidth = 3, colour="#7e0ce0ff")
g2 = ggplot(pop_counts, aes(x=time, y=nature)) +
  scale_x_continuous(expand=c(0,0)) +
  geom_line(linewidth = 3, colour="#2e831eff") + 
  geom_line(aes(y=wealth), linewidth = 3, colour="#e08e0cff")
grid.arrange(g1, g2, ncol=1)

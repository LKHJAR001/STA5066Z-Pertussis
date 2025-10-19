setwd("C:/Users/Jared/OneDrive - University of Cape Town/Documents/MMID/Assignment")

#========================================================================
#                             Data
#========================================================================
rm(list = ls())
library(readr)
library(dplyr)
pertussis <- read_csv("NNDSS_Weekly_Data_20250826.csv")
pertussis <- pertussis %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))

pertussis <- pertussis %>%
  mutate(`Reporting Area` = toupper(`Reporting Area`),
         `Reporting Area` = if_else(`Reporting Area` == "NEW YORK CITY", 
                                    "NEW YORK", `Reporting Area`))

valid_states <- toupper(c(state.name, "District of Columbia"))

state_data <- pertussis %>%
  filter(Label == "Pertussis",
         `Reporting Area` %in% valid_states) %>%
  transmute(state = `Reporting Area`,
            year  = `Current MMWR Year`,
            week  = `MMWR WEEK`,
            cases = `Current week`)

northeast <- c("CONNECTICUT","MAINE","MASSACHUSETTS","NEW HAMPSHIRE","RHODE ISLAND","VERMONT",
               "NEW JERSEY","NEW YORK","PENNSYLVANIA")
midwest   <- c("ILLINOIS","INDIANA","MICHIGAN","OHIO","WISCONSIN",
               "IOWA","KANSAS","MINNESOTA","MISSOURI","NEBRASKA","NORTH DAKOTA","SOUTH DAKOTA")
south     <- c("DELAWARE","DISTRICT OF COLUMBIA","FLORIDA","GEORGIA","MARYLAND",
               "NORTH CAROLINA","SOUTH CAROLINA","VIRGINIA","WEST VIRGINIA",
               "ALABAMA","KENTUCKY","MISSISSIPPI","TENNESSEE",
               "ARKANSAS","LOUISIANA","OKLAHOMA","TEXAS")
west      <- c("ARIZONA","COLORADO","IDAHO","MONTANA","NEVADA","NEW MEXICO","UTAH","WYOMING",
               "ALASKA","CALIFORNIA","HAWAII","OREGON","WASHINGTON")

region_map <- tibble(
  state = c(northeast, midwest, west, south),
  region = c(rep("Northeast", length(northeast)),
             rep("Midwest",   length(midwest)),
             rep("West",      length(west)), 
             rep("South",     length(south)))
)

region_data <- state_data %>%
  left_join(region_map, by = "state") %>%
  group_by(region, year, week) %>%
  summarise(cases = sum(cases, na.rm = TRUE), .groups = "drop") %>%
  arrange(region, year, week)

Incidence_data <- matrix(NA, 188, 4)
colnames(Incidence_data) <- c("Northeast","Midwest","West", "South")
regions <- c("Northeast","Midwest","West", "South")

for (i in seq_along(regions)) {
  reg <- regions[i]
  temp_cases <- region_data %>%
    filter(region == reg) %>%
    select(year, week, cases) %>%
    arrange(year, week)
  Incidence_data[ , i] <- temp_cases$cases
}

plot(Incidence_data[, 1], pch = 'o')
plot.ts((temp_cases$cases))
plot.ts(cumsum(temp_cases$cases))

A <- 3; P <- 4
#========================================================================
#                         Pop Matrix
#========================================================================
pop1 = 57832935 
pop2 = 69596584 
pop3 = 80015776
pop4 = 132665693
pop18  = 73100000
Ptot =pop1+pop2+pop3+pop4

Pop = matrix(NA, A, P)
col = 1
for (pop in c(pop1, pop2, pop3, pop4)){
  Pop[1, col] = Ptot* pop/(pop1+pop2+pop3+pop4) * 1/18 * pop18/(pop1+pop2+pop3+pop4) 
  col = col+1
}
col = 1
for (pop in c(pop1, pop2, pop3, pop4)){
  Pop[2, col] = Ptot* pop/(pop1+pop2+pop3+pop4) * 10/18 * pop18/(pop1+pop2+pop3+pop4)  
  col = col+1
}
col = 1
for (pop in c(pop1, pop2, pop3, pop4)){
  Pop[3, col] = Ptot* pop/(pop1+pop2+pop3+pop4) * ((pop1+pop2+pop3+pop4) - 11/18 * pop18)/(pop1+pop2+pop3+pop4)  
  col = col+1
}


#========================================================================
#               Migration Matrix (not a function of age)
#========================================================================
M <-  matrix(
  c(
    0,           192109/3, 192109/3, 192109/3,   # Northeast outflow
    49214/3,     0,        49214/3,  49214/3,    # Midwest outflow
    169681/3,    169681/3, 0,        169681/3,   # West outflow
    0,           0,        0,        0           # South has net inflow
  ),
  nrow = 4, byrow = TRUE
)/ 52
M/3

M = M/matrix(c(rep(pop1, P), rep(pop2, P), rep(pop3, P), rep(pop4, P)), P, P, byrow = TRUE)/3


#========================================================================
#                         Births
#========================================================================
Bus = 3622673
pop1 = 57832935 
pop2 = 69596584 
pop3 = 80015776
pop4 = 132665693

b0 = c(Bus* pop1/(pop1+pop2+pop3+pop4)/52, Bus* pop2/(pop1+pop2+pop3+pop4)/52, 
       Bus* pop3/(pop1+pop2+pop3+pop4)/52, Bus* pop4/(pop1+pop2+pop3+pop4)/52)

b0 = b0/Pop[1, ]
#========================================================================
#                         Mortality
#========================================================================
Dus = 3287000

mus_rec = matrix(NA, A, P)
col = 1
for (pop in c(pop1, pop2, pop3, pop4)){
  mus_rec[1, col] = Dus* pop/(pop1+pop2+pop3+pop4) * 1/18 * pop18/(pop1+pop2+pop3+pop4)  / 52
  col = col+1
}
col = 1
for (pop in c(pop1, pop2, pop3, pop4)){
  mus_rec[2, col] = Dus* pop/(pop1+pop2+pop3+pop4) * 10/18 * pop18/(pop1+pop2+pop3+pop4)  / 52
  col = col+1
}
col = 1
for (pop in c(pop1, pop2, pop3, pop4)){
  mus_rec[3, col] = Dus* pop/(pop1+pop2+pop3+pop4) * ((pop1+pop2+pop3+pop4) - 11/18 * pop18)/(pop1+pop2+pop3+pop4)  / 52
  col = col+1
}
mus_rec/Pop
mus = mus_rec/Pop
# Dus/Ptot/52


#========================================================================
#                         Vaccination Rates
#========================================================================

V_tottot = 503068145
V_tot = 1/12 *V_tottot
V_tot_week = V_tot/52


v = matrix(NA, A, P)
col = 1
for (pop in c(pop1, pop2, pop3, pop4)){
  v[1, col] = V_tot_week* pop/(pop1+pop2+pop3+pop4) * 1/18 * pop18/(pop1+pop2+pop3+pop4)
  col = col+1
}
col = 1
for (pop in c(pop1, pop2, pop3, pop4)){
  v[2, col] = V_tot_week* pop/(pop1+pop2+pop3+pop4) * 10/18 * pop18/(pop1+pop2+pop3+pop4)
  col = col+1
}
col = 1
for (pop in c(pop1, pop2, pop3, pop4)){
  v[3, col] = V_tot_week* pop/(pop1+pop2+pop3+pop4) * ((pop1+pop2+pop3+pop4) - 11/18 * pop18)/(pop1+pop2+pop3+pop4)
  col = col+1
}
v
v = v/Pop
# V_tot/Ptot/52


#========================================================================
#                         Model Setup
#========================================================================
library(deSolve)
betat_store  = matrix(NA, 1, P)
Lam_store  = matrix(NA, 1, P)

rhs_vec <- function(t, y, parms) {
  with(parms, {
    index = 1:P
    Mmat = matrix(y[index], 1, P)
    # index = (max(index) + 1):(max(index) + A*P)
    index = max(index)+1:(A*P)
    Smat = matrix(y[index], A, P)
    index = max(index)+1:(A*P)
    Vmat = matrix(y[index], A, P)
    index = max(index)+1:(A*P)
    Emat = matrix(y[index], A, P)
    index = max(index)+1:(A*P)
    Amat = matrix(y[index], A, P)
    index = max(index)+1:(A*P)
    CnTmat = matrix(y[index], A, P)
    index = max(index)+1:(A*P)
    CTmat = matrix(y[index], A, P)
    index = max(index)+1:(A*P)
    Tmmat = matrix(y[index], A, P)
    index = max(index)+1:(A*P)
    Rmat = matrix(y[index], A, P)
    index = max(index)+1:(A*P)
    CIncmat = matrix(y[index], A, P)
    

    Maug <- rbind(Mmat, matrix(0, A-1, P))
    Nmat <- Maug + Smat + Vmat + Emat + Amat + CnTmat + CTmat + Tmmat + Rmat
    
    Icell <- zetaA*Amat + zetanT*CnTmat + zetaT*CTmat
    Lam <- beta0 * (1 + beta1*exp(-1/2*((t - phi)/sig)^2))   * (Icell / Nmat)   # A x P

    
    dM  <- matrix(0, 1, P)
    dS  <- matrix(0, A, P)
    dV  <- matrix(0, A, P)
    dE  <- matrix(0, A, P)
    dAs <- matrix(0, A, P)
    dCnT<- matrix(0, A, P)
    dCT <- matrix(0, A, P)
    dTm <- matrix(0, A, P)
    dR  <- matrix(0, A, P)
    dCInc <- matrix(0, A, P)

    dS  <- dS  + omegaV*Vmat + omegaR*Rmat - Lam*Smat - v*Smat

    dV  <- dV  + v*Smat - omegaV*Vmat - (1-eps)*Lam*Vmat
    dE  <- dE  + Lam*Smat + (1-eps)*Lam*Vmat
    dE  <- dE  - ( pA*sigma + (1-pA)*(1-pT)*sigma + (1-pA)*pT*sigma )*Emat
    dAs <- dAs + pA*sigma*Emat + delta*CnTmat - gammaI*Amat
    dCnT<- dCnT+ (1-pA)*(1-pT)*sigma*Emat - delta*CnTmat
    dCT <- dCT + (1-pA)*pT*sigma*Emat - tau*CTmat
    dTm <- dTm + tau*CTmat - gammaT*Tmmat
    dR  <- dR  + gammaI*Amat + gammaT*Tmmat - omegaR*Rmat
    dCInc <- (1-pA)*pT*sigma*Emat  # incidence tracker
    
    #  Births, infant vaccine waning to S, and M dynamics
    Ntot <- colSums(Nmat)               
    dM[1, ] <- dM[1, ] + b0*pi0*Ntot - omegaM[1, ]*Mmat[1, ]
    # Newborn S and M->S aging contribution
    dS[1, ] <- dS[1, ] + b0*(1-pi0)*Ntot + omegaM[1, ]*Mmat[1, ]
    if (A >= 2) {dS[2, ] <- dS[2, ] + alphas[1, ]*Mmat[1, ]}
    
    # Aging
    # a = 1
    dM[1, ]   = dM[1, ]   - alphas[1, ] * Mmat[1, ]
    dS[1, ]   = dS[1, ]   - alphas[1, ] * Smat[1, ]
    dV[1, ]   = dV[1, ]   - alphas[1, ] * Vmat[1, ]
    dE[1, ]   = dE[1, ]   - alphas[1, ] * Emat[1, ]
    dAs[1, ]  = dAs[1, ]  - alphas[1, ] * Amat[1, ]
    dCnT[1, ] = dCnT[1, ] - alphas[1, ] * CnTmat[1, ]
    dCT[1, ]  = dCT[1, ]  - alphas[1, ] * CTmat[1, ]
    dTm[1, ]  = dTm[1, ]  - alphas[1, ] * Tmmat[1, ]
    dR[1, ]   = dR[1, ]   - alphas[1, ] * Rmat[1, ]

    # a != 1 && a!=A
    if (A> 2){
    dS[c(2:(A-1)), ]   = dS[c(2:(A-1)), ]   - alphas[c(2:(A-1)), ] * Smat[c(2:(A-1)), ]   + alphas[c(1:(A-2)), ] * Smat[c(1:(A-2)), ]
    dV[c(2:(A-1)), ]   = dV[c(2:(A-1)), ]   - alphas[c(2:(A-1)), ] * Vmat[c(2:(A-1)), ]   + alphas[c(1:(A-2)), ] * Vmat[c(1:(A-2)), ]
    dE[c(2:(A-1)), ]   = dE[c(2:(A-1)), ]   - alphas[c(2:(A-1)), ] * Emat[c(2:(A-1)), ]   + alphas[c(1:(A-2)), ] * Emat[c(1:(A-2)), ]
    dAs[c(2:(A-1)), ]  = dAs[c(2:(A-1)), ]  - alphas[c(2:(A-1)), ] * Amat[c(2:(A-1)), ]   + alphas[c(1:(A-2)), ] * Amat[c(1:(A-2)), ]
    dCnT[c(2:(A-1)), ] = dCnT[c(2:(A-1)), ] - alphas[c(2:(A-1)), ] * CnTmat[c(2:(A-1)), ] + alphas[c(1:(A-2)), ] * CnTmat[c(1:(A-2)), ]
    dCT[c(2:(A-1)), ]  = dCT[c(2:(A-1)), ]  - alphas[c(2:(A-1)), ] * CTmat[c(2:(A-1)), ]  + alphas[c(1:(A-2)), ] * CTmat[c(1:(A-2)), ]
    dTm[c(2:(A-1)), ]  = dTm[c(2:(A-1)), ]  - alphas[c(2:(A-1)), ] * Tmmat[c(2:(A-1)), ]  + alphas[c(1:(A-2)), ] * Tmmat[c(1:(A-2)), ]
    dR[c(2:(A-1)), ]   = dR[c(2:(A-1)), ]   - alphas[c(2:(A-1)), ] * Rmat[c(2:(A-1)), ]   + alphas[c(1:(A-2)), ] * Rmat[c(1:(A-2)), ]
    }

    # a==A
    dS[A, ]   = dS[A, ]   + alphas[A-1, ] * Smat[A-1, ]
    dV[A, ]   = dV[A, ]   + alphas[A-1, ] * Vmat[A-1, ]
    dE[A, ]   = dE[A, ]   + alphas[A-1, ] * Emat[A-1, ]
    dAs[A, ]  = dAs[A, ]  + alphas[A-1, ] * Amat[A-1, ]
    dCnT[A, ] = dCnT[A, ] + alphas[A-1, ] * CnTmat[A-1, ]
    dCT[A, ]  = dCT[A, ]  + alphas[A-1, ] * CTmat[A-1, ]
    dTm[A, ]  = dTm[A, ]  + alphas[A-1, ] * Tmmat[A-1, ]
    dR[A, ]   = dR[A, ]   + alphas[A-1, ] * Rmat[A-1, ]
    
  
    
    # Migration
    B <- Marr[, , 1]                     # P x P
    diag(B) <- -rowSums(Marr[, ,1 ]) 
    
    dS  <- dS  + Smat  %*% B
    dV  <- dV  + Vmat  %*% B
    dE  <- dE  + Emat  %*% B
    dAs <- dAs + Amat  %*% B
    dCnT<- dCnT+ CnTmat%*% B
    dCT <- dCT + CTmat %*% B
    dTm <- dTm + Tmmat %*% B
    dR  <- dR  + Rmat  %*% B
    dM[1, ] <- dM[1, ] + Mmat[1, ] %*% B

    # Mortality
    dM[1, ] <- dM[1, ] - mus[1, ]*Mmat[1, ]
    dS  <- dS  - mus*Smat
    dV  <- dV  - mus*Vmat
    dE  <- dE  - mus*Emat
    dAs <- dAs - mus*Amat
    dCnT<- dCnT- mus*CnTmat
    dCT <- dCT - mus*CTmat
    dTm <- dTm - mus*Tmmat
    dR  <- dR  - mus*Rmat

    list(c(dM, dS, dV, dE, dAs, dCnT, dCT, dTm, dR, dCInc))
  })
}



M0    <- rep(1, P)
V0    <- 0.25*Pop
E0    <-  matrix(1, A, P)
A0    <-  matrix(1, A, P)
CnT0  <-  matrix(1, A, P)
CT0   <- matrix(rep(Incidence_data[1, ]/A, A), A, P, byrow = TRUE)
Tm0   <- matrix(1, A, P)
R0    <- 0.25*Pop
CInc0 <- matrix(0, A, P)
S0    <- (Pop - V0-R0)


y0 <- c(
  M0,
  as.vector(S0),
  as.vector(V0),
  as.vector(E0),
  as.vector(A0),
  as.vector(CnT0),
  as.vector(CT0),
  as.vector(Tm0),
  as.vector(R0), 
  as.vector(CInc0)
)



parms <- list(
  A = A, P = P,
  Marr = array(M, dim = c(P,P,A)), # migration not a function of age
  zetaA   = matrix(0.7, A, P),
  zetanT  = matrix(1, A, P),
  zetaT   = matrix(1, A, P),
  v       = v,
  eps     = matrix(c(rep(0.8, P), rep(0.8, P), rep(0.5, P)), A, P, byrow = TRUE),
  pA      = matrix(c(rep(0.1, P), rep(0.3, P), rep(0.6, P)),A, P, byrow = TRUE),
  pT      = matrix(c(rep(0.8, P), rep(0.4, P), rep(0.4, P)),A, P, byrow = TRUE),
  sigma   = matrix(7/7, A, P),
  delta   = matrix(0.5, A, P),
  alphas  = matrix(c(rep(1, 4), rep(.1, 4), rep(0, 4)), A, P, byrow = TRUE)/52,
  gammaI  = matrix(7/21, A, P),
  tau     = matrix(c(rep(7/5.6, P), rep(7/13.8, P),rep(7/13.8, P)), A, P, byrow = TRUE) ,
  gammaT  = matrix(7/5, A, P),
  omegaM  = matrix(7*log(2)/30, 1, P),
  omegaV  = matrix(1/4/52, A, P),
  omegaR  = matrix(1/30/52, A, P),
  mus     = mus,
  b0      = b0,
  pi0     = rep(0.55, P),
  beta0 = matrix(c(rep(0.29, A), rep(0.247, A),rep(0.268, A), rep(0.263, A)), A, P, byrow = FALSE),
  beta1 =  matrix(c(rep(7.96, A), rep(7.24, A),rep(6.64, A), rep(6.29, A)), A, P, byrow = FALSE),
  sig = matrix(c(rep(6.34, A), rep(10.04, A),rep(8.55, A), rep(9.34, A)), A, P, byrow = FALSE),
  phi = matrix(c(rep(116, A), rep(127, A),rep(129, A), rep(130.31, A)), A, P, byrow = FALSE)
)


t_period = nrow(temp_cases)
times = seq(0, nrow(temp_cases), by = 1)
run = ode(y = y0,times = times,func = rhs_vec,parms = parms,method = "lsoda" )

#========================================================================
#                         Model Fitting
#========================================================================

M0    <- rep(1, P)         
V0    <- 0.25*Pop
E0    <-  matrix(1, A, P)
A0    <-  matrix(1, A, P)
CnT0  <-  matrix(1, A, P)
CT0   <- matrix(rep(Incidence_data[1, ]/A, A), A, P, byrow = TRUE)
Tm0   <- matrix(1, A, P)
R0    <- 0.25*Pop
CInc0 <- matrix(0, A, P)
S0    <- Pop - V0 - R0   


parms_fixed <- list(
  A = A, P = P,
  Marr = array(M, dim = c(P,P,A)), # migration not a function of age
  zetaA   = matrix(0.7, A, P),
  zetanT  = matrix(1, A, P),
  zetaT   = matrix(1, A, P),
  v       = v,
  eps     = matrix(c(rep(0.8, P), rep(0.8, P), rep(0.5, P)), A, P, byrow = TRUE),
  pA      = matrix(c(rep(0.1, P), rep(0.3, P), rep(0.6, P)),A, P, byrow = TRUE),
  pT      = matrix(c(rep(0.8, P), rep(0.4, P), rep(0.4, P)),A, P, byrow = TRUE),
  sigma   = matrix(7/7, A, P),
  delta   = matrix(0.5, A, P),
  alphas  = matrix(c(rep(1, 4), rep(.1, 4), rep(0, 4)), A, P, byrow = TRUE)/52,
  gammaI  = matrix(7/21, A, P),
  tau     = matrix(c(rep(7/5.6, P), rep(7/13.8, P),rep(7/13.8, P)), A, P, byrow = TRUE) ,
  gammaT  = matrix(7/5, A, P),
  omegaM  = matrix(7*log(2)/30, 1, P),
  omegaV  = matrix(1/4/52, A, P),
  omegaR  = matrix(1/30/52, A, P),
  mus     = mus,
  b0      = b0,
  pi0     = rep(0.55, P)
  # beta0 = matrix(1.2, A, P),
  # beta1   = matrix(120,A, P),
  # sig = rep(1, P),
  # phi    = rep(150, P)
)

t_period = nrow(temp_cases)
times = seq(0, nrow(temp_cases), by = 1)


CIncmat = function(y0, times, parms, func= rhs_vec) {
  run <- deSolve::ode(y=y0, times=times, func=func, parms=parms, method="lsoda")
  CInc_indices = 1+(P + 8*(A*P)) + 1:(A*P)
  CIncmat = matrix(NA,t_period, P)
  index = 0
  for (p in 1:P){
    index = max(index) + 1:A
    CIncmat[, p] = diff(rowSums(run[, CInc_indices[index]]))
  }
  return (CIncmat)
}



obj_sse = function(parms_est, parms_fixed, times = times, func= rhs_vec, data = Incidence_data)  {
  parms = parms_fixed
  dummies1 = parms_est[1:P]
  dummies2 = parms_est[(P+1):(2*P)]
  dummies3 = parms_est[(2*P+1):(3*P)]
  dummies4 = parms_est[(3*P+1):(4*P)]
  rho = 1/(1+exp(-parms_est[(4*P+1):(5*P)]))
  rho =   matrix(rho, nrow = t_period, ncol = P, byrow = TRUE)
  parms$beta0 = matrix(exp(rep(dummies1, A)), A, P, byrow = TRUE)
  parms$beta1 = matrix(exp(rep(dummies2, A)), A, P, byrow = TRUE)
  parms$sig = matrix(exp(rep(dummies3, A)), A, P, byrow = TRUE)
  parms$phi = matrix(exp(rep(dummies4, A)), A, P, byrow = TRUE)
  pred  = CIncmat(y0, times, parms, func= rhs_vec)
  mu = rho*pred
  sum((mu - Incidence_data)^2)
}

obj_poisson = function(parms_est, parms_fixed, times = times, func= rhs_vec, data = Incidence_data)  {
  parms = parms_fixed
  dummies1 = parms_est[1:P]
  dummies2 = parms_est[(P+1):(2*P)]
  dummies3 = parms_est[(2*P+1):(3*P)]
  dummies4 = parms_est[(3*P+1):(4*P)]
  rho = 1/(1+exp(-parms_est[(4*P+1):(5*P)]))
  rho =   matrix(rho, nrow = t_period, ncol = P, byrow = TRUE)
  parms$beta0 = matrix(exp(rep(dummies1, A)), A, P, byrow = TRUE)
  parms$beta1 = matrix(exp(rep(dummies2, A)), A, P, byrow = TRUE)
  parms$sig = matrix(exp(rep(dummies3, A)), A, P, byrow = TRUE)
  parms$phi = matrix(exp(rep(dummies4, A)), A, P, byrow = TRUE)
  yhat  = CIncmat(y0, times, parms, func= rhs_vec)
  -sum(Incidence_data*log(rho*yhat) - rho*yhat)
}

obj_negbin =function(parms_est, parms_fixed, times = times, func= rhs_vec, data = Incidence_data)  {
  parms = parms_fixed
  dummies1 = parms_est[1:P]
  dummies2 = parms_est[(P+1):(2*P)]
  dummies3 = parms_est[(2*P+1):(3*P)]
  dummies4 = parms_est[(3*P+1):(4*P)]
  parms$beta0 = matrix(exp(rep(dummies1, A)), A, P, byrow = TRUE)
  parms$beta1 = matrix(exp(rep(dummies2, A)), A, P, byrow = TRUE)
  parms$sig = matrix(exp(rep(dummies3, A)), A, P, byrow = TRUE)
  parms$phi = matrix(exp(rep(dummies4, A)), A, P, byrow = TRUE)
  yhat  = CIncmat(y0, times, parms, func= rhs_vec)
  
  rho = 1/(1+exp(-parms_est[(4*P+1):(5*P)]))
  rho =   matrix(rho, nrow = t_period, ncol = P, byrow = TRUE)
  kappa = exp(parms_est[(5*P+1): (6*P)])
  kappa = matrix(kappa, nrow = t_period, ncol = P, byrow = TRUE)
  -sum(lgamma(Incidence_data+kappa) - lgamma(kappa) - lgamma(Incidence_data+1) + kappa*log(kappa) - kappa*log(kappa+rho*yhat)
       + Incidence_data*log(rho*yhat) - Incidence_data*log(kappa+rho*yhat))
}




fitfun <- function(parms_est) obj_sse(parms_est, parms_fixed = parms_fixed, times = times, data = Incidence_data)
# theta0 = c(log(c(0.26, .25, 0.25, 0.24)), log(c(9.2, 7.2, 8.1, 8)), log(c(7, 10, 9, 10)), log(c(120, 130, 130, 130)), -log(1/c(0.99, 0.99, 0.99, 0.99) - 1),  )
# Negbin
# theta0 = c(log(c(0.4, .4, 0.42, 0.42)), log(c(1.21, 0.89, 1.34, 0.73)), log(c(10, 10, 10, 10)), log(c(20, 25, 10, 1)),  log(c(2,2, 2, 2)),  -log(1/c(0.99, 0.99, 0.99, 0.99) - 1) )

fit <- optim(
  par     = theta0,
  fn      = function(parms_est) obj_poisson(parms_est, parms_fixed = parms_fixed, times = times, data = Incidence_data),
  method  = "L-BFGS-B",
  lower   = rep(-Inf,length(theta0)),
  upper   = rep(Inf, length(theta0)),
  control = list(maxit = 200, trace = 1)
)


# SSE
par_hat = fit$par
b0_hat   <- exp(par_hat[1:P])
b1_hat = exp(par_hat[(P+1):(2*P)])
sig_hat= exp(par_hat[(2*P+1):(3*P)])
phi_hat = exp(par_hat[(3*P+1):(4*P)])

parms_hat <- parms_fixed
parms_hat$beta0 <- matrix(rep(b0_hat, each = A), nrow = A, ncol = P)
parms_hat$beta1 <- matrix(rep(b1_hat, each = A), nrow = A, ncol = P)
parms_hat$sig <- matrix(rep(sig_hat, each = A), nrow = A, ncol = P)
parms_hat$phi <- matrix(rep(phi_hat, each = A), nrow = A, ncol = P)

yhat <- CIncmat(y0, times, parms_hat, func = rhs_vec)   
rho_hat = 1/(1+exp(-par_hat[(4*P+1):(5*P)]))
mu =  matrix(rho_hat, nrow = t_period, ncol = P, byrow = TRUE)*yhat

op <- par(mfrow = c(2, 2), mar = c(4,5,3,1), oma = c(0,0,4,0)) 
patch_names <- c("Northeast","Midwest","West","South") 
for (p in 1:P) {
  plot(Incidence_data[,p], type="p", pch=16, col="#19263B",
       main = patch_names[p], xlab="Time (weeks)", ylab=bquote(Y[.(p)](t)), cex.lab = 1.5)
  lines(mu[,p], col="#ff2800", lwd=4)              
  legend("topleft", bty="n",
         legend=c("Observed","Fitted mean"),
         col=c("#19263B","#ff2800"),
         pch=c(16, NA), lwd=c(NA,4))
}
mtext(expression(hat(bold(theta))^{SSE}), side=3, outer=TRUE, line=0.001, cex=2)
par(op)




# NegBin
par_hat = fit$par
b0_hat   <- exp(par_hat[1:P])
b1_hat = exp(par_hat[(P+1):(2*P)])
sig_hat= exp(par_hat[(2*P+1):(3*P)])
phi_hat = exp(par_hat[(3*P+1):(4*P)])

parms_hat <- parms_fixed
parms_hat$beta0 <- matrix(rep(b0_hat, each = A), nrow = A, ncol = P)
parms_hat$beta1 <- matrix(rep(b1_hat, each = A), nrow = A, ncol = P)
parms_hat$sig <- matrix(rep(sig_hat, each = A), nrow = A, ncol = P)
parms_hat$phi <- matrix(rep(phi_hat, each = A), nrow = A, ncol = P)


yhat <- CIncmat(y0, times, parms_hat, func = rhs_vec)  
rho_hat = 1/(1+exp(-par_hat[(4*P+1):(5*P)]))
mu =  matrix(rho_hat, nrow = t_period, ncol = P, byrow = TRUE)*yhat

k_hat = exp(par_hat[(5*P+1): (6*P)])
Kmat <- matrix(k_hat, nrow = t_period, ncol = P, byrow = TRUE)
lo <- matrix(qnbinom(0.025, size = Kmat, mu = mu), nrow = t_period)
hi <- matrix(qnbinom(0.975, size = Kmat, mu = mu), nrow = t_period)

op <- par(mfrow = c(2, 2), mar = c(4,5,3,1), oma = c(0,0,4,0)) 
patch_names <- c("Northeast","Midwest","West","South") 
for (p in 1:P) {
  plot(Incidence_data[,p], type="p", pch=16, col="#19263B",
       main = patch_names[p], xlab="Time (weeks)", ylab=bquote(Y[.(p)](t)), cex.lab = 1.5)
  
  lines(mu[,p], col="#ff2800", lwd=4)               # fitted mean
  lines(lo[,p], col="grey70", lwd=2)              # 95% PI
  lines(hi[,p], col="grey70", lwd=2)
  legend("topleft", bty="n",
         legend=c("Observed","Fitted mean","95% PI"),
         col=c("#19263B","#ff2800","grey70"), lwd=c(NA,2,2), pch=c(16, NA, NA))
}
mtext(expression(hat(bold(theta))^{MLE[NB]}), side=3, outer=TRUE, line=0.001, cex=2)
par(op)



# Poisson
par_hat = fit$par
b0_hat   <- exp(par_hat[1:P])
b1_hat = exp(par_hat[(P+1):(2*P)])
sig_hat= exp(par_hat[(2*P+1):(3*P)])
phi_hat = exp(par_hat[(3*P+1):(4*P)])

parms_hat <- parms_fixed
parms_hat$beta0 <- matrix(rep(b0_hat, each = A), nrow = A, ncol = P)
parms_hat$beta1 <- matrix(rep(b1_hat, each = A), nrow = A, ncol = P)
parms_hat$sig <- matrix(rep(sig_hat, each = A), nrow = A, ncol = P)
parms_hat$phi <- matrix(rep(phi_hat, each = A), nrow = A, ncol = P)


yhat <- CIncmat(y0, times, parms_hat, func = rhs_vec)   
rho_hat = 1/(1+exp(-par_hat[(4*P+1):(5*P)]))
mu =  matrix(rho_hat, nrow = t_period, ncol = P, byrow = TRUE)*yhat

lo <- matrix(qpois(0.025, lambda = mu), nrow = t_period)
hi <- matrix(qpois(0.975, lambda = mu), nrow = t_period)
op <- par(mfrow = c(2, 2), mar = c(4,5,3,1), oma = c(0,0,4,0)) 
patch_names <- c("Northeast","Midwest","West","South") 
for (p in 1:P) {
  plot(Incidence_data[,p], type="p", pch=16, col="#19263B",
       main = patch_names[p], xlab="Time (weeks)", ylab=bquote(Y[.(p)](t)), cex.lab = 1.5)
  
  lines(mu[,p], col="#ff2800", lwd=4)               # fitted mean
  lines(lo[,p], col="grey70", lwd=2)              # 95% PI
  lines(hi[,p], col="grey70", lwd=2)
  
  legend("topleft", bty="n",
         legend=c("Observed","Fitted mean","95% PI"),
         col=c("#19263B","#ff2800","grey70"), lwd=c(NA,2,2), pch=c(16, NA, NA))
}
mtext(expression(hat(bold(theta))^{MLE[Poi]}), side=3, outer=TRUE, line=0.001, cex=2)
par(op)




#========================================================================
#                         Sensitivity Analysis
#========================================================================

# V0 and R0
parms <- list(
  A = A, P = P,
  Marr = array(M, dim = c(P,P,A)), # migration not a function of age
  zetaA   = matrix(0.7, A, P),
  zetanT  = matrix(1, A, P),
  zetaT   = matrix(1, A, P),
  v       = v,
  eps     = matrix(c(rep(0.8, P), rep(0.8, P), rep(0.5, P)), A, P, byrow = TRUE),
  pA      = matrix(c(rep(0.1, P), rep(0.3, P), rep(0.6, P)),A, P, byrow = TRUE),
  pT      = matrix(c(rep(0.8, P), rep(0.4, P), rep(0.4, P)),A, P, byrow = TRUE),
  sigma   = matrix(7/7, A, P),
  delta   = matrix(0.5, A, P),
  alphas  = matrix(c(rep(1, 4), rep(.1, 4), rep(0, 4)), A, P, byrow = TRUE)/52,
  gammaI  = matrix(7/21, A, P),
  tau     = matrix(c(rep(7/5.6, P), rep(7/13.8, P),rep(7/13.8, P)), A, P, byrow = TRUE) ,
  gammaT  = matrix(7/5, A, P),
  omegaM  = matrix(7*log(2)/30, 1, P),
  omegaV  = matrix(1/4/52, A, P),
  omegaR  = matrix(1/30/52, A, P),
  mus     = mus,
  b0      = b0,
  pi0     = rep(0.55, P)
  # beta0 = matrix(c(rep(0.63, A), rep(0.63, A),rep(0.63, A), rep(0.63, A)), A, P, byrow = FALSE)
  # beta0 = matrix(c(rep(0.26, A), rep(0.25, A),rep(0.25, A), rep(0.24, A)), A, P, byrow = FALSE),
  # beta1 =  matrix(c(rep(9.2, A), rep(7.2, A),rep(8.1, A), rep(8, A)), A, P, byrow = FALSE),
  # sig = matrix(c(rep(7, A), rep(10, A),rep(9, A), rep(10, A)), A, P, byrow = FALSE),
  # phi = matrix(c(rep(120, A), rep(130, A),rep(130, A), rep(130, A)), A, P, byrow = FALSE)
)

parms$beta0 <- matrix(rep(b0_hat, each = A), nrow = A, ncol = P)
parms$beta1 <- matrix(rep(b1_hat, each = A), nrow = A, ncol = P)
parms$sig <- matrix(rep(sig_hat, each = A), nrow = A, ncol = P)
parms$phi <- matrix(rep(phi_hat, each = A), nrow = A, ncol = P)


CIncmat = function(y0, times, parms, func= rhs_vec) {
  run <- deSolve::ode(y=y0, times=times, func=func, parms=parms, method="lsoda")
  CInc_indices = 1+(P + 8*(A*P)) + 1:(A*P)
  CIncmat = matrix(NA,t_period, P)
  index = 0
  for (p in 1:P){
    index = max(index) + 1:A
    CIncmat[, p] = diff(rowSums(run[, CInc_indices[index]]))
  }
  return (CIncmat)
}


base <- 0.9
vals <- seq(0, 0.9, 0.05)
n <- length(vals)

NE_store <- matrix(NA_real_, n, n)
MW_store <- matrix(NA_real_, n, n)
W_store  <- matrix(NA_real_, n, n)
S_store  <- matrix(NA_real_, n, n)
NEt_store <- matrix(NA_real_, n, n)
MWt_store <- matrix(NA_real_, n, n)
Wt_store  <- matrix(NA_real_, n, n)
St_store  <- matrix(NA_real_, n, n)

for (i in seq_len(n)) {
  Vval <- vals[i]
  for (j in seq_len(n)) {
    Rval <- vals[j]
    if (Vval + Rval > base) next
    
    M0    <- rep(1, P)
    V0    <- Vval * Pop
    E0    <- matrix(1, A, P)
    A0    <- matrix(1, A, P)
    CnT0  <- matrix(1, A, P)
    CT0   <- matrix(rep(Incidence_data[1, ] / A, A), A, P, byrow = TRUE)
    Tm0   <- matrix(1, A, P)
    R0    <- Rval * Pop
    CInc0 <- matrix(0, A, P)
    S0    <- Pop - V0 - R0
    
    y0 <- c(
      M0,
      as.vector(S0),
      as.vector(V0),
      as.vector(E0),
      as.vector(A0),
      as.vector(CnT0),
      as.vector(CT0),
      as.vector(Tm0),
      as.vector(R0),
      as.vector(CInc0)
    )
    
    mu_new  <- CIncmat(y0, times, parms, func = rhs_vec) *
      matrix(rho_hat, nrow = t_period, ncol = P, byrow = TRUE)
    
    maxes <- apply(mu_new, 2, max, na.rm = TRUE)
    maxtimes = apply(mu_new, 2, which.max)
    NE_store[i, j] <- maxes[1]
    MW_store[i, j] <- maxes[2]
    W_store[i, j]  <- maxes[3]
    S_store[i, j]  <- maxes[4]
    NEt_store[i, j] <- maxtimes[1]
    MWt_store[i, j] <- maxtimes[2]
    Wt_store[i, j]  <- maxtimes[3]
    St_store[i, j]  <- maxtimes[4]
  }
}


library(fields)
par(mfrow = c(2,2), mar = c(5, 5, 4, 2))
pal_NE <- colorRampPalette(c("#c6dbef", "#08306b"))   
pal_MW <- colorRampPalette(c("#fcbba1", "#67000d"))  
pal_W  <- colorRampPalette(c("#fee6ce", "#7f2704"))   
pal_S  <- colorRampPalette(c("#fff7bc", "#7a0177"))  

# Northeast
image.plot(vals, vals, log(NE_store/apply(mu, 2, max)[1]),
           col = pal_NE(200),
           xlab = expression(rho^V), 
           ylab = expression(rho^R),
           main = "Northeast")

# Midwest
image.plot(vals, vals, log(MW_store/apply(mu, 2, max)[2]),
           col = pal_MW(200),
           xlab = expression(rho^V), 
           ylab = expression(rho^R),
           main = "Midwest")

# West
image.plot(vals, vals, log(W_store/apply(mu, 2, max)[3]),
           col = pal_W(200),
           xlab = expression(rho^V), 
           ylab = expression(rho^R),
           main = "West")

# South
image.plot(vals, vals, log(S_store/apply(mu, 2, max)[4]),
           col = pal_S(200),
           xlab = expression(rho^V), 
           ylab = expression(rho^R),
           main = "South")






# beta0s
M0    <- rep(1, P)
V0    <-0.25* Pop
E0    <- matrix(1, A, P)
A0    <- matrix(1, A, P)
CnT0  <- matrix(1, A, P)
CT0   <- matrix(rep(Incidence_data[1, ] / A, A), A, P, byrow = TRUE)
Tm0   <- matrix(1, A, P)
R0    <- 0.25 * Pop
CInc0 <- matrix(0, A, P)
S0    <- Pop - V0 - R0

y0 <- c(
  M0,
  as.vector(S0),
  as.vector(V0),
  as.vector(E0),
  as.vector(A0),
  as.vector(CnT0),
  as.vector(CT0),
  as.vector(Tm0),
  as.vector(R0),
  as.vector(CInc0)
)


parms <- list(
  A = A, P = P,
  Marr = array(M, dim = c(P,P,A)), # migration not a function of age
  zetaA   = matrix(0.7, A, P),
  zetanT  = matrix(1, A, P),
  zetaT   = matrix(1, A, P),
  v       = v,
  eps     = matrix(c(rep(0.8, P), rep(0.8, P), rep(0.5, P)), A, P, byrow = TRUE),
  pA      = matrix(c(rep(0.1, P), rep(0.3, P), rep(0.6, P)),A, P, byrow = TRUE),
  pT      = matrix(c(rep(0.8, P), rep(0.4, P), rep(0.4, P)),A, P, byrow = TRUE),
  sigma   = matrix(7/7, A, P),
  delta   = matrix(0.5, A, P),
  alphas  = matrix(c(rep(1, 4), rep(.1, 4), rep(0, 4)), A, P, byrow = TRUE)/52,
  gammaI  = matrix(7/21, A, P),
  tau     = matrix(c(rep(7/5.6, P), rep(7/13.8, P),rep(7/13.8, P)), A, P, byrow = TRUE) ,
  gammaT  = matrix(7/5, A, P),
  omegaM  = matrix(7*log(2)/30, 1, P),
  omegaV  = matrix(1/4/52, A, P),
  omegaR  = matrix(1/30/52, A, P),
  mus     = mus,
  b0      = b0,
  pi0     = rep(0.55, P)
  # beta0 = matrix(c(rep(0.63, A), rep(0.63, A),rep(0.63, A), rep(0.63, A)), A, P, byrow = FALSE)
  # beta0 = matrix(c(rep(0.26, A), rep(0.25, A),rep(0.25, A), rep(0.24, A)), A, P, byrow = FALSE),
  # beta1 =  matrix(c(rep(9.2, A), rep(7.2, A),rep(8.1, A), rep(8, A)), A, P, byrow = FALSE),
  # sig = matrix(c(rep(7, A), rep(10, A),rep(9, A), rep(10, A)), A, P, byrow = FALSE),
  # phi = matrix(c(rep(120, A), rep(130, A),rep(130, A), rep(130, A)), A, P, byrow = FALSE)
)
parms$beta1 <- matrix(rep(b1_hat, each = A), nrow = A, ncol = P)
parms$sig <- matrix(rep(sig_hat, each = A), nrow = A, ncol = P)
parms$phi <- matrix(rep(phi_hat, each = A), nrow = A, ncol = P)



CIncmat = function(y0, times, parms, func= rhs_vec) {
  run <- deSolve::ode(y=y0, times=times, func=func, parms=parms, method="lsoda")
  CInc_indices = 1+(P + 8*(A*P)) + 1:(A*P)
  CIncmat = matrix(NA,t_period, P)
  index = 0
  for (p in 1:P){
    index = max(index) + 1:A
    CIncmat[, p] = diff(rowSums(run[, CInc_indices[index]]))
  }
  return (CIncmat)
}



vals <- seq(.5, 1.5, 0.05)
n <- length(vals)

NE_store <- numeric(n)
MW_store <- numeric(n)
W_store  <- numeric(n)
S_store  <- numeric(n)
NEt_store <- numeric(n)
MWt_store <- numeric(n)
Wt_store  <-numeric(n)
St_store  <- numeric(n)

for (i in seq_len(n)) {
  # b0_hat_new = c( vals[i]*b0_hat[1], b0_hat[c(2, 3, 4)])
  # b0_hat_new = c(b0_hat[c(1, 2)], vals[i]*b0_hat[3], b0_hat[c(4)])
  b0_hat_new = c(b0_hat[1:3], vals[i]*b0_hat[4])
  
  parms$beta0 <- matrix(rep(b0_hat_new, each = A), nrow = A, ncol = P)
  mu_new  <- CIncmat(y0, times, parms, func = rhs_vec) *matrix(rho_hat, nrow = t_period, ncol = P, byrow = TRUE)
  maxes <- apply(mu_new, 2, max, na.rm = TRUE)
  maxtimes = apply(mu_new, 2, which.max)
  NE_store[i] <- maxes[1];MW_store[i] <- maxes[2];W_store[i]  <- maxes[3];S_store[i]  <- maxes[4]
  NEt_store[i] <- maxtimes[1];MWt_store[i] <- maxtimes[2];Wt_store[i]  <- maxtimes[3];St_store[i]  <- maxtimes[4]
}


par(mfrow = c(2,2), mar = c(5, 8, 4, 2))
log_NE <- log(NE_store/apply(mu, 2, max)[1])
log_MW <- log(MW_store/apply(mu, 2, max)[2])
log_W  <- log(W_store/apply(mu, 2, max)[3])
log_S  <- log(S_store/apply(mu, 2, max)[4])
ylim_all <- range(c(log_NE, log_MW, log_W, log_S), na.rm = TRUE)
plot(vals, log_NE, type = "o", lwd = 2, col = "#2323ff", pch = 16, cex = 1.5,
     xlab = expression(rho^{hat(beta)[4]^0}),
     ylab = expression(
       log ~ bgroup("(", frac( max*group("{", hat(mu)[p], "}"),
                               max*group("{", hat(mu)[p]^"*", "}") ), ")")
     ),
     ylim = ylim_all, lty = 1, xaxt = 'n', main =  'South', cex.main = 1.7, cex.lab = 1.4)
axis(1, at = seq(0.5, 1.5, by = 0.1), labels = seq(0.5, 1.5, by = 0.1))

lines(vals, log_MW, type = "o", col = "#ff2800",   lwd = 2, lty = 2, pch = 17, cex = 1.5)
lines(vals, log_W,  type = "o", col = "#FD5E0F", lwd = 2, lty = 3, pch = 15, cex = 1.5)
lines(vals, log_S,  type = "o", col = "#FFC107", lwd = 2, lty = 4, pch = 18, cex = 1.5)

legend("topleft",
       legend = c("NE", "MW", "W", "S"),
       col = c("#2323ff", "#ff2800", "#FD5E0F", "#FFC107"),
       lwd = 2, lty = 1:4, pch = c(16,17,15,18), pt.cex = 1.2, bty = "n", cex = 0.9)




# phis
M0    <- rep(1, P)
V0    <-0.25* Pop
E0    <- matrix(1, A, P)
A0    <- matrix(1, A, P)
CnT0  <- matrix(1, A, P)
CT0   <- matrix(rep(Incidence_data[1, ] / A, A), A, P, byrow = TRUE)
Tm0   <- matrix(1, A, P)
R0    <- 0.25 * Pop
CInc0 <- matrix(0, A, P)
S0    <- Pop - V0 - R0

y0 <- c(
  M0,
  as.vector(S0),
  as.vector(V0),
  as.vector(E0),
  as.vector(A0),
  as.vector(CnT0),
  as.vector(CT0),
  as.vector(Tm0),
  as.vector(R0),
  as.vector(CInc0)
)


parms <- list(
  A = A, P = P,
  Marr = array(M, dim = c(P,P,A)), # migration not a function of age
  zetaA   = matrix(0.7, A, P),
  zetanT  = matrix(1, A, P),
  zetaT   = matrix(1, A, P),
  v       = v,
  eps     = matrix(c(rep(0.8, P), rep(0.8, P), rep(0.5, P)), A, P, byrow = TRUE),
  pA      = matrix(c(rep(0.1, P), rep(0.3, P), rep(0.6, P)),A, P, byrow = TRUE),
  pT      = matrix(c(rep(0.8, P), rep(0.4, P), rep(0.4, P)),A, P, byrow = TRUE),
  sigma   = matrix(7/7, A, P),
  delta   = matrix(0.5, A, P),
  alphas  = matrix(c(rep(1, 4), rep(.1, 4), rep(0, 4)), A, P, byrow = TRUE)/52,
  gammaI  = matrix(7/21, A, P),
  tau     = matrix(c(rep(7/5.6, P), rep(7/13.8, P),rep(7/13.8, P)), A, P, byrow = TRUE) ,
  gammaT  = matrix(7/5, A, P),
  omegaM  = matrix(7*log(2)/30, 1, P),
  omegaV  = matrix(1/4/52, A, P),
  omegaR  = matrix(1/30/52, A, P),
  mus     = mus,
  b0      = b0,
  pi0     = rep(0.55, P)
  # beta0 = matrix(c(rep(0.63, A), rep(0.63, A),rep(0.63, A), rep(0.63, A)), A, P, byrow = FALSE)
  # beta0 = matrix(c(rep(0.26, A), rep(0.25, A),rep(0.25, A), rep(0.24, A)), A, P, byrow = FALSE),
  # beta1 =  matrix(c(rep(9.2, A), rep(7.2, A),rep(8.1, A), rep(8, A)), A, P, byrow = FALSE),
  # sig = matrix(c(rep(7, A), rep(10, A),rep(9, A), rep(10, A)), A, P, byrow = FALSE),
  # phi = matrix(c(rep(120, A), rep(130, A),rep(130, A), rep(130, A)), A, P, byrow = FALSE)
)
parms$beta0 <- matrix(rep(b0_hat, each = A), nrow = A, ncol = P)
parms$beta1 <- matrix(rep(b1_hat, each = A), nrow = A, ncol = P)
parms$sig <- matrix(rep(sig_hat, each = A), nrow = A, ncol = P)

CIncmat = function(y0, times, parms, func= rhs_vec) {
  run <- deSolve::ode(y=y0, times=times, func=func, parms=parms, method="lsoda")
  CInc_indices = 1+(P + 8*(A*P)) + 1:(A*P)
  CIncmat = matrix(NA,t_period, P)
  index = 0
  for (p in 1:P){
    index = max(index) + 1:A
    CIncmat[, p] = diff(rowSums(run[, CInc_indices[index]]))
  }
  return (CIncmat)
}



vals <- seq(.5, 1.3, 0.05)
n <- length(vals)

NE_store <- numeric(n)
MW_store <- numeric(n)
W_store  <- numeric(n)
S_store  <- numeric(n)
NEt_store <- numeric(n)
MWt_store <- numeric(n)
Wt_store  <-numeric(n)
St_store  <- numeric(n)

for (i in seq_len(n)) {
  # phi_hat_new = c(vals[i]*phi_hat[1], phi_hat[c(2, 3, 4)])
  # phi_hat_new = c(phi_hat[c(1, 2)], vals[i]*phi_hat[3], phi_hat[c(4)])
  phi_hat_new = c(phi_hat[c(1,2,3)], vals[i]*phi_hat[4])

  parms$phi <- matrix(rep(phi_hat_new, each = A), nrow = A, ncol = P)
  mu_new  <- CIncmat(y0, times, parms, func = rhs_vec) *matrix(rho_hat, nrow = t_period, ncol = P, byrow = TRUE)
  maxes <- apply(mu_new, 2, max, na.rm = TRUE)
  maxtimes = apply(mu_new, 2, which.max)
  NE_store[i] <- maxes[1];MW_store[i] <- maxes[2];W_store[i]  <- maxes[3];S_store[i]  <- maxes[4]
  NEt_store[i] <- maxtimes[1];MWt_store[i] <- maxtimes[2];Wt_store[i]  <- maxtimes[3];St_store[i]  <- maxtimes[4]
}



par(mfrow = c(2,2), mar = c(5, 8, 4, 2))
log_NE <- log(NE_store)
log_MW <- log(MW_store)
log_W  <- log(W_store)
log_S  <- log(S_store)
ylim_all <- range(c(log_NE, log_MW, log_W, log_S), na.rm = TRUE)
plot(vals, log_NE, type = "o", lwd = 2, col = "#2323ff", pch = 16, cex = 1.5,
     xlab = expression(rho^{hat(phi)[4]}),
     ylab = expression(
       log ~ bgroup("(", frac( max*group("{", hat(mu)[p], "}"),
                               max*group("{", hat(mu)[p]^"*", "}") ), ")")
     ),
     ylim = ylim_all, lty = 1, xaxt = 'n', main = 'South', cex.main = 1.7, cex.lab = 1.4)
axis(1, at = seq(0.5, 1.5, by = 0.1), labels = seq(0.5, 1.5, by = 0.1))

lines(vals, log_MW, type = "o", col = "#ff2800",   lwd = 2, lty = 2, pch = 17, cex = 1.5)
lines(vals, log_W,  type = "o", col = "#FD5E0F", lwd = 2, lty = 3, pch = 15, cex = 1.5)
lines(vals, log_S,  type = "o", col = "#FFC107", lwd = 2, lty = 4, pch = 18, cex = 1.5)

legend("bottomright",
       legend = c("NE", "MW", "W", "S"),
       col = c("#2323ff", "#ff2800", "#FD5E0F", "#FFC107"),
       lwd = 2, lty = 1:4, pch = c(16,17,15,18), pt.cex = 1.2, bty = "n", cex = 0.9)



#========================================================================
#                         Drug resistance
#========================================================================
library(deSolve)
rhs_vec_res <- function(t, y, parms) {
  with(parms, {
    index = 1:P
    Mmat = matrix(y[index], 1, P)
    # index = (max(index) + 1):(max(index) + A*P)
    index = max(index)+1:(A*P)
    Smat = matrix(y[index], A, P)
    index = max(index)+1:(A*P)
    Vmat = matrix(y[index], A, P)
    index = max(index)+1:(A*P)
    Emat = matrix(y[index], A, P)
    index = max(index)+1:(A*P)
    Amat = matrix(y[index], A, P)
    index = max(index)+1:(A*P)
    CnTmat = matrix(y[index], A, P)
    index = max(index)+1:(A*P)
    CTmat = matrix(y[index], A, P)
    index = max(index)+1:(A*P)
    Tmmat = matrix(y[index], A, P)
    index = max(index)+1:(A*P)
    Rmat = matrix(y[index], A, P)
    index = max(index)+1:(A*P)
    CIncmat = matrix(y[index], A, P)
    index = max(index)+1:(A*P)
    CTRmat = matrix(y[index], A, P)

    Maug <- rbind(Mmat, matrix(0, A-1, P))
    Nmat <- Maug + Smat + Vmat + Emat + Amat + CnTmat + CTmat + Tmmat + Rmat
    Icell <- zetaA*Amat + zetanT*CnTmat + zetaT*CTmat + zetaR*CTRmat

    Lam <- beta0 * (1 + beta1*exp(-1/2*((t - phi)/sig)^2))   * (Icell / Nmat)   # A x P

    
    
    dM  <- matrix(0, 1, P)
    dS  <- matrix(0, A, P)
    dV  <- matrix(0, A, P)
    dE  <- matrix(0, A, P)
    dAs <- matrix(0, A, P)
    dCnT<- matrix(0, A, P)
    dCT <- matrix(0, A, P)
    dTm <- matrix(0, A, P)
    dR  <- matrix(0, A, P)
    dCInc <- matrix(0, A, P)
    dCTR  <- matrix(0, A, P)
    
    
    dS  <- dS  + omegaV*Vmat + omegaR*Rmat - Lam*Smat - v*Smat

    dV  <- dV  + v*Smat - omegaV*Vmat - (1-eps)*Lam*Vmat
    dE  <- dE  + Lam*Smat + (1-eps)*Lam*Vmat
    dE  <- dE  - ( pA*sigma + (1-pA)*(1-pT)*sigma + (1-pA)*pT*sigma )*Emat
    dAs <- dAs + pA*sigma*Emat + delta*CnTmat - gammaI*Amat
    dCnT<- dCnT+ (1-pA)*(1-pT)*sigma*Emat - delta*CnTmat
    dCT <- dCT + (1-pA)*pT*sigma*Emat - (1-pR)*tau*CTmat - pR*tau*CTmat
    dTm <- dTm + (1- pR)*tau*CTmat - gammaT*Tmmat - eta*Tmmat
    dCTR <- dCTR + pR*tau*CTmat +  eta*Tmmat - gammaR*CTRmat
    dR  <- dR  + gammaI*Amat + gammaT*Tmmat + gammaR*CTRmat - omegaR*Rmat
    dCInc <- (1-pA)*pT*sigma*Emat  # incidence tracker
    
    #  Births, infant vaccine waning to S, and M dynamics
    Ntot <- colSums(Nmat)               
    dM[1, ] <- dM[1, ] + b0*pi0*Ntot - omegaM[1, ]*Mmat[1, ]
    # Newborn S and M->S aging contribution
    dS[1, ] <- dS[1, ] + b0*(1-pi0)*Ntot + omegaM[1, ]*Mmat[1, ]
    if (A >= 2) {dS[2, ] <- dS[2, ] + alphas[1, ]*Mmat[1, ]}
    
    # Aging
    # a = 1
    dM[1, ]   = dM[1, ]   - alphas[1, ] * Mmat[1, ]
    dS[1, ]   = dS[1, ]   - alphas[1, ] * Smat[1, ]
    dV[1, ]   = dV[1, ]   - alphas[1, ] * Vmat[1, ]
    dE[1, ]   = dE[1, ]   - alphas[1, ] * Emat[1, ]
    dAs[1, ]  = dAs[1, ]  - alphas[1, ] * Amat[1, ]
    dCnT[1, ] = dCnT[1, ] - alphas[1, ] * CnTmat[1, ]
    dCT[1, ]  = dCT[1, ]  - alphas[1, ] * CTmat[1, ]
    dTm[1, ]  = dTm[1, ]  - alphas[1, ] * Tmmat[1, ]
    dR[1, ]   = dR[1, ]   - alphas[1, ] * Rmat[1, ]
    dCTR[1, ]   = dCTR[1, ]   - alphas[1, ] * CTRmat[1, ]
    
    # a != 1 && a!=A
    if (A> 2){
      dS[c(2:(A-1)), ]   = dS[c(2:(A-1)), ]   - alphas[c(2:(A-1)), ] * Smat[c(2:(A-1)), ]   + alphas[c(1:(A-2)), ] * Smat[c(1:(A-2)), ]
      dV[c(2:(A-1)), ]   = dV[c(2:(A-1)), ]   - alphas[c(2:(A-1)), ] * Vmat[c(2:(A-1)), ]   + alphas[c(1:(A-2)), ] * Vmat[c(1:(A-2)), ]
      dE[c(2:(A-1)), ]   = dE[c(2:(A-1)), ]   - alphas[c(2:(A-1)), ] * Emat[c(2:(A-1)), ]   + alphas[c(1:(A-2)), ] * Emat[c(1:(A-2)), ]
      dAs[c(2:(A-1)), ]  = dAs[c(2:(A-1)), ]  - alphas[c(2:(A-1)), ] * Amat[c(2:(A-1)), ]   + alphas[c(1:(A-2)), ] * Amat[c(1:(A-2)), ]
      dCnT[c(2:(A-1)), ] = dCnT[c(2:(A-1)), ] - alphas[c(2:(A-1)), ] * CnTmat[c(2:(A-1)), ] + alphas[c(1:(A-2)), ] * CnTmat[c(1:(A-2)), ]
      dCT[c(2:(A-1)), ]  = dCT[c(2:(A-1)), ]  - alphas[c(2:(A-1)), ] * CTmat[c(2:(A-1)), ]  + alphas[c(1:(A-2)), ] * CTmat[c(1:(A-2)), ]
      dTm[c(2:(A-1)), ]  = dTm[c(2:(A-1)), ]  - alphas[c(2:(A-1)), ] * Tmmat[c(2:(A-1)), ]  + alphas[c(1:(A-2)), ] * Tmmat[c(1:(A-2)), ]
      dR[c(2:(A-1)), ]   = dR[c(2:(A-1)), ]   - alphas[c(2:(A-1)), ] * Rmat[c(2:(A-1)), ]   + alphas[c(1:(A-2)), ] * Rmat[c(1:(A-2)), ]
      dCTR[c(2:(A-1)), ]   = dCTR[c(2:(A-1)), ]   - alphas[c(2:(A-1)), ] * CTRmat[c(2:(A-1)), ]   + alphas[c(1:(A-2)), ] * CTRmat[c(1:(A-2)), ]
    }
    
    # a==A
    dS[A, ]   = dS[A, ]   + alphas[A-1, ] * Smat[A-1, ]
    dV[A, ]   = dV[A, ]   + alphas[A-1, ] * Vmat[A-1, ]
    dE[A, ]   = dE[A, ]   + alphas[A-1, ] * Emat[A-1, ]
    dAs[A, ]  = dAs[A, ]  + alphas[A-1, ] * Amat[A-1, ]
    dCnT[A, ] = dCnT[A, ] + alphas[A-1, ] * CnTmat[A-1, ]
    dCT[A, ]  = dCT[A, ]  + alphas[A-1, ] * CTmat[A-1, ]
    dTm[A, ]  = dTm[A, ]  + alphas[A-1, ] * Tmmat[A-1, ]
    dR[A, ]   = dR[A, ]   + alphas[A-1, ] * Rmat[A-1, ]
    dCTR[A, ]   = dCTR[A, ]   + alphas[A-1, ] * CTRmat[A-1, ]
    
    
    
    # Migration
    B <- Marr[, , 1]                     # P x P
    diag(B) <- -rowSums(Marr[, ,1 ]) 
    
    dS  <- dS  + Smat  %*% B
    dV  <- dV  + Vmat  %*% B
    dE  <- dE  + Emat  %*% B
    dAs <- dAs + Amat  %*% B
    dCnT<- dCnT+ CnTmat%*% B
    dCT <- dCT + CTmat %*% B
    dTm <- dTm + Tmmat %*% B
    dR  <- dR  + Rmat  %*% B
    dCTR  <- dCTR  + CTRmat  %*% B
    dM[1, ] <- dM[1, ] + Mmat[1, ] %*% B
    
    # Mortality
    dM[1, ] <- dM[1, ] - mus[1, ]*Mmat[1, ]
    dS  <- dS  - mus*Smat
    dV  <- dV  - mus*Vmat
    dE  <- dE  - mus*Emat
    dAs <- dAs - mus*Amat
    dCnT<- dCnT- mus*CnTmat
    dCT <- dCT - mus*CTmat
    dTm <- dTm - mus*Tmmat
    dR  <- dR  - mus*Rmat
    dCTR  <- dCTR  - mus*CTRmat
    
    list(c(dM, dS, dV, dE, dAs, dCnT, dCT, dTm, dR, dCInc, dCTR))
  })
}

M0    <- rep(1, P)
V0    <- 0.25*Pop
E0    <-  matrix(1, A, P)
A0    <-  matrix(1, A, P)
CnT0  <-  matrix(1, A, P)
CT0   <- matrix(rep(Incidence_data[1, ]/A, A), A, P, byrow = TRUE)
Tm0   <- matrix(1, A, P)
R0    <- 0.25*Pop
CInc0 <- matrix(0, A, P)
S0    <- (Pop - V0-R0)
CTR   <- matrix(1, A, P)


y0 <- c(
  M0,
  as.vector(S0),
  as.vector(V0),
  as.vector(E0),
  as.vector(A0),
  as.vector(CnT0),
  as.vector(CT0),
  as.vector(Tm0),
  as.vector(R0), 
  as.vector(CInc0), 
  as.vector(CTR)
)



parms <- list(
  A = A, P = P,
  Marr = array(M, dim = c(P,P,A)), # migration not a function of age
  zetaA   = matrix(0.7, A, P),
  zetanT  = matrix(1, A, P),
  zetaT   = matrix(1, A, P),
  v       = v,
  eps     = matrix(c(rep(0.8, P), rep(0.8, P), rep(0.5, P)), A, P, byrow = TRUE),
  pA      = matrix(c(rep(0.1, P), rep(0.3, P), rep(0.6, P)),A, P, byrow = TRUE),
  pT      = matrix(c(rep(0.8, P), rep(0.4, P), rep(0.4, P)),A, P, byrow = TRUE),
  sigma   = matrix(7/7, A, P),
  delta   = matrix(0.5, A, P),
  alphas  = matrix(c(rep(1, 4), rep(.1, 4), rep(0, 4)), A, P, byrow = TRUE)/52,
  gammaI  = matrix(7/21, A, P),
  tau     = matrix(c(rep(7/5.6, P), rep(7/13.8, P),rep(7/13.8, P)), A, P, byrow = TRUE) ,
  gammaT  = matrix(7/5, A, P),
  omegaM  = matrix(7*log(2)/30, 1, P),
  omegaV  = matrix(1/4/52, A, P),
  omegaR  = matrix(1/30/52, A, P),
  mus     = mus,
  b0      = b0,
  pi0     = rep(0.55, P),
  pR      = 0.001,
  eta     = matrix(7/50, A, P),
  gammaR = matrix(7/10, A, P), 
  zetaR =  matrix(1, A, P)
)

parms$beta0 <- matrix(rep(b0_hat, each = A), nrow = A, ncol = P)
parms$beta1 <- matrix(rep(b1_hat, each = A), nrow = A, ncol = P)
parms$sig <- matrix(rep(sig_hat, each = A), nrow = A, ncol = P)
parms$phi <- matrix(rep(phi_hat, each = A), nrow = A, ncol = P)


run <- deSolve::ode(y=y0, times=times, func=rhs_vec_res, parms=parms, method="lsoda")
run


CIncmat = function(y0, times, parms, func= rhs_vec_res) {
  run <- deSolve::ode(y=y0, times=times, func=func, parms=parms, method="lsoda")
  CInc_indices = 1+(P + 8*(A*P)) + 1:(A*P)
  CIncmat = matrix(NA,t_period, P)
  index = 0
  for (p in 1:P){
    index = max(index) + 1:A
    CIncmat[, p] = diff(rowSums(run[, CInc_indices[index]]))
  }
  return (CIncmat)
}


mu_res  = CIncmat(y0, times, parms, func= rhs_vec_res)* matrix(rho_hat, nrow = t_period, ncol = P, byrow = TRUE) 

op <- par(mfrow = c(2, 2), mar = c(4,5,2,1), oma = c(0,0,2,0)) 
patch_names <- c("Northeast","Midwest","West","South") 
for (p in 1:P) {
  if (p == 1) {
    ylim_vals <- c(0, 130)
  } else if (p == 2) {
    ylim_vals <- c(0, 240)
  } else if (p == 3) {
    ylim_vals <- c(0, 130)
  } else {
    ylim_vals <- range(Incidence_data[,p], na.rm = TRUE)  
  }
  plot(Incidence_data[,p], type="p", pch=16, col="#19263B",
       main = patch_names[p], xlab="Time (weeks)", ylab=bquote(Y[.(p)](t)), cex.lab = 1.2, ylim = ylim_vals, cex.main = 1.4)
  
  lines(mu_res[,p], col="#ff2800", lwd=4)               # fitted mean
  lines(mu[,p], col="#F37022", lwd=2)               # fitted mean

  legend("topleft", bty="n",
         legend=c("Observed","Fitted mean (Resistance)", "Fitted mean (No Resistance)"),
         col=c("#19263B","#ff2800", "#F37022"),
         pch=c(16, NA, NA), lwd=c(NA,4, 2))
}
par(op)


#========================================================================
#                 Drug resistance: Sensitivity Analysis
#========================================================================

parms <- list(
  A = A, P = P,
  Marr = array(M, dim = c(P,P,A)), # migration not a function of age
  zetaA   = matrix(0.7, A, P),
  zetanT  = matrix(1, A, P),
  zetaT   = matrix(1, A, P),
  v       = v,
  eps     = matrix(c(rep(0.8, P), rep(0.8, P), rep(0.5, P)), A, P, byrow = TRUE),
  pA      = matrix(c(rep(0.1, P), rep(0.3, P), rep(0.6, P)),A, P, byrow = TRUE),
  pT      = matrix(c(rep(0.8, P), rep(0.4, P), rep(0.4, P)),A, P, byrow = TRUE),
  sigma   = matrix(7/7, A, P),
  delta   = matrix(0.5, A, P),
  alphas  = matrix(c(rep(1, 4), rep(.1, 4), rep(0, 4)), A, P, byrow = TRUE)/52,
  gammaI  = matrix(7/21, A, P),
  tau     = matrix(c(rep(7/5.6, P), rep(7/13.8, P),rep(7/13.8, P)), A, P, byrow = TRUE) ,
  gammaT  = matrix(7/5, A, P),
  omegaM  = matrix(7*log(2)/30, 1, P),
  omegaV  = matrix(1/4/52, A, P),
  omegaR  = matrix(1/30/52, A, P),
  mus     = mus,
  b0      = b0,
  pi0     = rep(0.55, P),
  # pR      = 0.001,
  # eta     = matrix(7/100, A, P),
  gammaR = matrix(7/10, A, P), 
  zetaR =  matrix(1, A, P)
)

parms$beta0 <- matrix(rep(b0_hat, each = A), nrow = A, ncol = P)
parms$beta1 <- matrix(rep(b1_hat, each = A), nrow = A, ncol = P)
parms$sig <- matrix(rep(sig_hat, each = A), nrow = A, ncol = P)
parms$phi <- matrix(rep(phi_hat, each = A), nrow = A, ncol = P)



CIncmat = function(y0, times, parms, func= rhs_vec_res) {
  run <- deSolve::ode(y=y0, times=times, func=func, parms=parms, method="lsoda")
  CInc_indices = 1+(P + 8*(A*P)) + 1:(A*P)
  CIncmat = matrix(NA,t_period, P)
  index = 0
  for (p in 1:P){
    index = max(index) + 1:A
    CIncmat[, p] = diff(rowSums(run[, CInc_indices[index]]))
  }
  return (CIncmat)
}



pRvals <- seq(.001, 0.2, .01)
etavals <- seq(10, 200,10 )
npR <- length(pRvals)
neta <- length(etavals)


NE_store <- matrix(NA_real_, npR, neta)
MW_store <- matrix(NA_real_, npR, neta)
W_store  <- matrix(NA_real_, npR, neta)
S_store  <- matrix(NA_real_, npR, neta)
NEt_store <- matrix(NA_real_, npR, neta)
MWt_store <- matrix(NA_real_, npR, neta)
Wt_store  <- matrix(NA_real_, npR, neta)
St_store  <- matrix(NA_real_, npR, neta)

for (i in seq_len(npR)){
  for (j in seq_len(neta)){
    parms$pR <- pRvals[i]
    parms$eta <- matrix(7/ etavals[j], A, P)
    mu_new  <- CIncmat(y0, times, parms, func = rhs_vec_res) *matrix(rho_hat, nrow = t_period, ncol = P, byrow = TRUE)
    maxes <- apply(mu_new, 2, max, na.rm = TRUE)
    maxtimes = apply(mu_new, 2, which.max)
    NE_store[i, j] <- maxes[1]
    MW_store[i, j] <- maxes[2]
    W_store[i, j]  <- maxes[3]
    S_store[i, j]  <- maxes[4]
    NEt_store[i, j] <- maxtimes[1]
    MWt_store[i, j] <- maxtimes[2]
    Wt_store[i, j]  <- maxtimes[3]
    St_store[i, j]  <- maxtimes[4]
  }
}





library(fields)
par(mfrow = c(2,2), mar = c(5, 5, 4, 2))
pal_NE <- colorRampPalette(c("#c6dbef", "#08306b"))   
pal_MW <- colorRampPalette(c("#fcbba1", "#67000d"))   
pal_W  <- colorRampPalette(c("#fee6ce", "#7f2704"))   
pal_S  <- colorRampPalette(c("#fff7bc", "#7a0177")) 

# Northeast
image.plot(pRvals, etavals, log(NE_store[,]/apply(mu, 2, max)[1]),
           col = pal_NE(15),
           ylab = expression(7 / eta[list(a,p)]), 
           xlab = expression(rho^{plain("Res")}),
           main = "Northeast", cex.lab = 1.5)

# Midwest
image.plot(pRvals, etavals, log(MW_store/apply(mu, 2, max)[2]),
           col = pal_MW(15),
           ylab = expression(7 / eta[list(a,p)]), 
           xlab = expression(rho^{plain("Res")}),
           main = "Midwest", cex.lab = 1.5)

# West
image.plot(pRvals, etavals, log(W_store/apply(mu, 2, max)[3]),
           col = pal_W(15),
           ylab = expression(7 / eta[list(a,p)]), 
           xlab = expression(rho^{plain("Res")}),
           main = "West", cex.lab = 1.5)

# South
image.plot(pRvals, etavals,log(S_store/apply(mu, 2, max)[4]),
           col = pal_S(20),
           ylab = expression(7 / eta[list(a,p)]), 
           xlab = expression(rho^{plain("Res")}),
           main = "South", cex.lab = 1.5)




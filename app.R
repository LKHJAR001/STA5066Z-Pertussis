rm(list = ls())
library(deSolve)
library(shiny)
library(dplyr)

#========================================================================
#                       Hard-coded Incidence Data
#========================================================================
Incidence_data <- as.data.frame(matrix(c(
  0,1,0,4, 0,0,0,2, 2,0,2,3, 3,0,0,1, 2,0,1,5, 6,0,8,0, 2,1,1,0, 0,1,2,1, 1,0,1,1, 1,0,0,0,
  0,2,0,1, 1,9,5,1, 5,1,0,3, 1,2,8,1, 1,0,5,0, 1,0,3,2, 1,0,8,1, 4,0,7,2, 1,4,5,1, 2,2,12,4,
  4,0,5,3, 1,4,4,4, 2,1,4,10, 1,0,9,4, 5,1,13,5, 8,0,6,1, 1,0,1,4, 4,1,3,4, 4,0,2,1, 6,0,7,4,
  11,1,13,4, 11,0,11,2, 12,0,12,1, 5,0,11,4, 9,1,14,5, 6,1,10,1, 6,0,0,5, 3,0,13,1, 0,0,12,1, 11,0,8,0,
  10,4,12,2, 6,2,14,1, 13,0,14,1, 12,2,18,4, 14,0,21,5, 15,0,26,5, 16,1,8,3, 10,0,8,3, 20,0,17,3, 9,1,19,3,
  20,0,12,5, 1,0,0,2, 1,6,7,4, 6,0,4,5, 5,5,18,8, 13,5,3,8, 13,11,8,3, 14,2,8,4, 13,6,9,5, 13,13,10,5,
  15,21,19,3, 24,13,13,2, 15,8,13,8, 8,20,10,5, 18,14,14,7, 1,9,6,10, 7,12,10,4, 6,10,4,3, 3,15,14,4, 9,34,11,5,
  3,17,7,4, 8,18,9,6, 2,19,15,2, 14,23,10,2, 10,41,15,6, 13,25,11,2, 12,16,3,3, 3,14,7,7, 11,14,8,7, 16,10,9,21,
  11,9,10,8, 14,24,13,7, 12,19,7,2, 14,6,12,2, 6,4,9,8, 15,11,18,4, 13,9,12,4, 1,14,8,5, 6,11,21,4, 11,9,17,3,
  14,4,24,4, 10,14,6,7, 16,22,0,2, 16,29,12,8, 23,19,13,7, 32,29,26,13, 24,40,24,3, 15,32,3,5, 32,32,22,4, 43,35,25,2,
  72,36,17,12, 86,19,17,6, 0,0,1,4, 11,15,6,7, 35,23,11,3, 27,20,24,9, 24,25,35,13, 32,15,21,16, 32,18,27,9, 34,19,9,11,
  20,8,24,12, 31,14,24,11, 36,15,23,15, 38,15,27,17, 27,8,26,13, 43,8,37,11, 32,17,18,24, 53,17,34,18, 52,24,55,31, 20,23,40,33,
  65,20,64,10, 56,21,39,21, 81,25,13,30, 69,26,16,27, 50,18,17,32, 88,32,21,12, 72,28,37,40, 40,22,13,18, 74,40,32,38, 108,22,43,42,
  84,28,25,20, 62,23,33,27, 97,32,38,48, 114,42,35,68, 54,37,46,46, 105,31,44,54, 105,34,43,68, 102,46,39,64, 63,34,48,89, 87,59,58,87,
  84,63,56,88, 80,76,63,70, 68,81,86,67, 58,91,55,42, 57,154,84,53, 93,162,91,116, 120,170,107,100, 51,157,97,84, 91,179,78,86, 104,182,116,175,
  66,152,65,81, 83,209,105,136, 86,181,106,136, 77,139,110,98, 45,64,34,69, 13,40,52,28, 26,83,85,39, 43,67,82,43, 33,80,81,89, 57,74,102,127,
  21,80,82,107, 29,53,43,82, 25,40,64,94, 40,43,77,68, 21,45,73,47, 12,34,58,54, 22,23,57,36, 26,40,60,69, 13,38,58,59, 16,39,61,69,
  16,22,49,53, 15,42,66,52, 14,37,45,60, 16,34,74,135, 13,38,64,90, 17,19,50,66, 17,42,47,88, 18,24,42,70, 14,22,68,93, 18,7,57,88,
  16,31,45,48, 16,16,36,98, 16,29,54,116, 18,25,39,88, 12,37,41,94, 15,27,37,92, 19,29,41,87, 23,40,45,56
), ncol = 4, byrow = TRUE))

colnames(Incidence_data) <- c("Northeast","Midwest","West","South")


A = 3
P = 4
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
#                 For Plots (Non-Drug Resistant Model)
#========================================================================
Mindex = 1+ 1:P
Sindex = max(Mindex)+1:(A*P)
Vindex = max(Sindex)+1:(A*P)
Eindex = max(Vindex)+1:(A*P)
Aindex = max(Eindex)+1:(A*P)
CnTindex = max(Aindex)+1:(A*P)
CTindex = max(CnTindex)+1:(A*P)
Tmindex = max(CTindex)+1:(A*P)
Rindex = max(Tmindex)+1:(A*P)
CIncindex = max(Rindex)+1:(A*P)

plotfunc_reg <- function(run, region = "Northeast", ymax = NULL, cex_pt = 0.9) {
  start_date <- as.Date("2022-01-01")
  times <- seq(start_date, by = "1 week", length.out = 189)
  
  if (region == "Northeast") { inds <- 1:A;           Mind <- 1
  } else if (region == "Midwest") { inds <- (A+1):(2*A);   Mind <- 2
  } else if (region == "West")    { inds <- (2*A+1):(3*A); Mind <- 3
  } else if (region == "South")   { inds <- (3*A+1):(4*A); Mind <- 4}
  
  col_A    <- "#191970"  # A
  col_E    <- "#BF1F2B"  # E
  col_CnT  <- "#C79E5F"  # C^{over(T)}
  col_CT   <- "#008EC1"  # C^{(T)}
  col_Tm   <- "#FFD300"  # T_m
  col_S    <- "#FF2800"  # S
  col_V    <- "#735D55"  # V
  col_M    <- "#F37022"  # M
  col_R    <- "#2323FF"  # R
  
  pch_A   <- 16  # solid circle
  pch_E   <- 17  # triangle
  pch_CnT <- 15  # square
  pch_CT  <- 18  # diamond
  pch_Tm  <- 8   # star
  pch_S   <- 1   # open circle
  pch_V   <- 0   # open square
  pch_M   <- 2   # open triangle
  pch_R   <- 4   # x
  
  sA   <- rowSums(run[, Aindex[inds],   drop=FALSE], na.rm=TRUE)
  sE   <- rowSums(run[, Eindex[inds],   drop=FALSE], na.rm=TRUE)
  sCnT <- rowSums(run[, CnTindex[inds], drop=FALSE], na.rm=TRUE)
  sCT  <- rowSums(run[, CTindex[inds],  drop=FALSE], na.rm=TRUE)
  sTm  <- rowSums(run[, Tmindex[inds],  drop=FALSE], na.rm=TRUE)
  
  sS   <- rowSums(run[, Sindex[inds],   drop=FALSE], na.rm=TRUE)
  sV   <- rowSums(run[, Vindex[inds],   drop=FALSE], na.rm=TRUE)
  sM   <- as.numeric(run[, Mindex[Mind]])  # vector for points()
  sR   <- rowSums(run[, Rindex[inds],   drop=FALSE], na.rm=TRUE)
  
  y_all1 <- c(sA, sE, sCnT, sCT, sTm)
  ymax1 <- max(y_all1, na.rm=TRUE) * 1.05
  yl1 <- c(0, ymax1)
  y_all2 = c(sS, sV, sM, sR)
  ymax2 =  max(y_all2, na.rm=TRUE) * 1.05
  yl2 = c(0, ymax2)
  
  op <- par(mfrow=c(1,2), mar=c(4,4,3,1)+0.2, oma=c(0,0,0.5,0))
  on.exit(par(op), add=TRUE)
  
  plot(times, sA, type="l", lwd=2, col=col_A, ylim = yl1,
       xlab="Date", ylab="Number of people")
  grid(nx=NA, ny=NULL, lty=3)
  lines(times, sE,   lwd=2, col=col_E)
  lines(times, sCnT, lwd=2, col=col_CnT)
  lines(times, sCT,  lwd=2, col=col_CT)
  lines(times, sTm,  lwd=2, col=col_Tm)
  
  # add shapes
  points(times, sA,   pch=pch_A,   cex=cex_pt, col=col_A)
  points(times, sE,   pch=pch_E,   cex=cex_pt, col=col_E)
  points(times, sCnT, pch=pch_CnT, cex=cex_pt, col=col_CnT)
  points(times, sCT,  pch=pch_CT,  cex=cex_pt, col=col_CT)
  points(times, sTm,  pch=pch_Tm,  cex=cex_pt, col=col_Tm)
  
  legend("top",
         legend=c(expression(A), expression(E),
                  expression(C^{bar(T)}), expression(C^{T}),
                  expression(T)),
         col=c(col_A,col_E,col_CnT,col_CT,col_Tm),
         lty=1, lwd=2, pch=c(pch_A,pch_E,pch_CnT,pch_CT,pch_Tm),
         bty="n", inset=0.02, cex=0.95)
  
  plot(times, sS, type="l", lwd=2, col=col_S,ylim = yl2,
       xlab="Date", ylab="Number of people")
  grid(nx=NA, ny=NULL, lty=3)
  lines(times, sV, lwd=2, col=col_V)
  lines(times, sM, lwd=2, col=col_M)
  lines(times, sR, lwd=2, col=col_R)
  
  # add shapes
  points(times, sS, pch=pch_S, cex=cex_pt, col=col_S)
  points(times, sV, pch=pch_V, cex=cex_pt, col=col_V)
  points(times, sM, pch=pch_M, cex=cex_pt, col=col_M)
  points(times, sR, pch=pch_R, cex=cex_pt, col=col_R)
  
  legend("top",
         legend=c(expression(S), expression(V), expression(M), expression(R)),
         col=c(col_S,col_V,col_M,col_R),
         lty=1, lwd=2, pch=c(pch_S,pch_V,pch_M,pch_R),
         bty="n", inset=0.02, cex=0.95)
  
  mtext(bquote(bold(.(region))), outer = TRUE, line = -2, cex = 1.1)
}

plotfunc_age <- function(run, age = "0", ymax = NULL, cex_pt = 0.9) {
  start_date <- as.Date("2022-01-01")
  times <- seq(start_date, by = "1 week", length.out = 189)
  
  if (age == "0") {
    inds <- c(1, A+1, 2*A+1, 3*A+1)
  } else if (age == "1") {
    inds <- c(1, A+1, 2*A+1, 3*A+1) + 1
  } else if (age == "2") {
    inds <- c(1, A+1, 2*A+1, 3*A+1) + 2
  }
  
  col_A    <- "#191970"  # A
  col_E    <- "#BF1F2B"  # E
  col_CnT  <- "#C79E5F"  # C^{over(T)}
  col_CT   <- "#008EC1"  # C^{(T)}
  col_Tm   <- "#FFD300"  # T_m
  col_S    <- "#FF2800"  # S
  col_V    <- "#735D55"  # V
  col_M    <- "#F37022"  # M
  col_R    <- "#2323FF"  # R
  
  pch_A   <- 16; pch_E <- 17; pch_CnT <- 15; pch_CT <- 18; pch_Tm <- 8
  pch_S   <- 1;  pch_V <- 0;  pch_M   <- 2; pch_R  <- 4
  
  sA   <- rowSums(run[, Aindex[inds],   drop=FALSE], na.rm=TRUE)
  sE   <- rowSums(run[, Eindex[inds],   drop=FALSE], na.rm=TRUE)
  sCnT <- rowSums(run[, CnTindex[inds], drop=FALSE], na.rm=TRUE)
  sCT  <- rowSums(run[, CTindex[inds],  drop=FALSE], na.rm=TRUE)
  sTm  <- rowSums(run[, Tmindex[inds],  drop=FALSE], na.rm=TRUE)
  
  sS   <- rowSums(run[, Sindex[inds],   drop=FALSE], na.rm=TRUE)
  sV   <- rowSums(run[, Vindex[inds],   drop=FALSE], na.rm=TRUE)
  sM   <- rowSums(run[, Mindex,   drop=FALSE], na.rm=TRUE)
  sR   <- rowSums(run[, Rindex[inds],   drop=FALSE], na.rm=TRUE)
  
  y_all1 <- c(sA, sE, sCnT, sCT, sTm)
  ymax1 <- max(y_all1, na.rm=TRUE) * 1.05
  yl1 <- c(0, ymax1)
  y_all2 = c(sS, sV, sM, sR)
  ymax2 =  max(y_all2, na.rm=TRUE) * 1.05
  yl2 = c(0, ymax2)

  op <- par(mfrow=c(1,2), mar=c(4,4,3,1)+0.2, oma=c(0,0,0.5,0))
  on.exit(par(op), add=TRUE)
  
  plot(times, sA, type="l", lwd=2, col=col_A, ylim= yl1,
       xlab="Date", ylab="Number of people")
  grid(nx=NA, ny=NULL, lty=3)
  lines(times, sE,   lwd=2, col=col_E)
  lines(times, sCnT, lwd=2, col=col_CnT)
  lines(times, sCT,  lwd=2, col=col_CT)
  lines(times, sTm,  lwd=2, col=col_Tm)
  
  points(times, sA,   pch=pch_A,   cex=cex_pt, col=col_A)
  points(times, sE,   pch=pch_E,   cex=cex_pt, col=col_E)
  points(times, sCnT, pch=pch_CnT, cex=cex_pt, col=col_CnT)
  points(times, sCT,  pch=pch_CT,  cex=cex_pt, col=col_CT)
  points(times, sTm,  pch=pch_Tm,  cex=cex_pt, col=col_Tm)
  
  legend("top",
         legend=c(expression(A), expression(E),
                  expression(C^{bar(T)}), expression(C^{T}),
                  expression(T)),
         col=c(col_A,col_E,col_CnT,col_CT,col_Tm),
         lty=1, lwd=2,
         pch=c(pch_A,pch_E,pch_CnT,pch_CT,pch_Tm),
         bty="n", inset=0.02, cex=0.95)
  

  plot(times, sS, type="l", lwd=2, col=col_S,ylim= yl2,
       xlab="Date", ylab="Number of people")
  grid(nx=NA, ny=NULL, lty=3)
  lines(times, sV, lwd=2, col=col_V)
  if (age == "0") {lines(times, sM, lwd=2, col=col_M)}
  lines(times, sR, lwd=2, col=col_R)
  
  points(times, sS, pch=pch_S, cex=cex_pt, col=col_S)
  points(times, sV, pch=pch_V, cex=cex_pt, col=col_V)
  if (age == "0") {points(times, sM, pch=pch_M, cex=cex_pt, col=col_M)}
  points(times, sR, pch=pch_R, cex=cex_pt, col=col_R)
  
  if (age == "0") {
    legend("top",
           legend=c(expression(S), expression(V), expression(M), expression(R)),
           col=c(col_S,col_V,col_M,col_R),
           lty=1, lwd=2,
           pch=c(pch_S,pch_V,pch_M,pch_R),
           bty="n", inset=0.02, cex=0.95)
  } else {
    legend("top",
           legend=c(expression(S), expression(V), expression(R)),
           col=c(col_S,col_V,col_R),
           lty=1, lwd=2,
           pch=c(pch_S,pch_V,pch_R),
           bty="n", inset=0.02, cex=0.95)
  }
  

  if (age == '0'){agething  = '<1 years'}
  if (age == '1'){agething  = '1-10 years'}
  if (age == '2'){agething  = '10+ years'}
  mtext(bquote(bold(.(agething))), outer = TRUE, line = -2, cex = 1.1)
}

t_period = nrow(Incidence_data)


CIncmat = function(run) {
  CInc_indices = 1+(P + 8*(A*P)) + 1:(A*P)
  CIncmat = matrix(NA,t_period, P)
  index = 0
  for (p in 1:P){
    index = max(index) + 1:A
    CIncmat[, p] = diff(rowSums(run[, CInc_indices[index]]))
  }
  return (CIncmat)
}


plot_incidence = function(run,run_res, rho){
  mu  = CIncmat(run)* matrix(rho, nrow = t_period, ncol = P, byrow = TRUE) 
  mu_res = CIncmat(run_res)* matrix(rho, nrow = t_period, ncol = P, byrow = TRUE) 
  op <- par(mfrow = c(2, 2), mar = c(4,4,3,1), oma = c(0,0,0,0)) 
  patch_names <- c("Northeast","Midwest","West","South") 
  for (p in 1:P) {
    start_date <- as.Date("2022-01-01")
    times <- seq(start_date, by = "1 week", length.out = t_period)
    plot(times, Incidence_data[,p], type="p", pch=16, col="#19263B",
         main = patch_names[p], xlab="Date", ylab="Number of People")
    
    lines(times, mu_res[,p], col="#ff2800", lwd=4)               # fitted mean
    lines(times, mu[,p], col="#F37022", lwd=2)               # fitted mean

    legend("topleft", bty="n",
           legend=c("Observed","Fitted mean (Resistance)", "Fitted mean (No Resistance)"),
           col=c("#19263B","#ff2800", "#F37022"),
           pch=c(16, NA, NA), lwd=c(NA,4, 2))
  }
  # mtext(expression(hat(bold(theta))^{MLE[Poi]}), side=3, outer=TRUE, line=0.001, cex=2)
  par(op)
}



#========================================================================
#                 For Plots (Drug Resistant Model)
#========================================================================
Mindex = 1+ 1:P
Sindex = max(Mindex)+1:(A*P)
Vindex = max(Sindex)+1:(A*P)
Eindex = max(Vindex)+1:(A*P)
Aindex = max(Eindex)+1:(A*P)
CnTindex = max(Aindex)+1:(A*P)
CTindex = max(CnTindex)+1:(A*P)
Tmindex = max(CTindex)+1:(A*P)
Rindex = max(Tmindex)+1:(A*P)
CIncindex = max(Rindex)+1:(A*P)
CTRindex= max(CIncindex) + 1:(A*P)

plotfunc_reg_res <- function(run, region = "Northeast", ymax = NULL, cex_pt = 0.9) {
  start_date <- as.Date("2022-01-01")
  times <- seq(start_date, by = "1 week", length.out = 189)
  
  if (region == "Northeast") { inds <- 1:A;           Mind <- 1
  } else if (region == "Midwest") { inds <- (A+1):(2*A);   Mind <- 2
  } else if (region == "West")    { inds <- (2*A+1):(3*A); Mind <- 3
  } else if (region == "South")   { inds <- (3*A+1):(4*A); Mind <- 4}
  
  col_A    <- "#191970"  # A
  col_E    <- "#BF1F2B"  # E
  col_CnT  <- "#C79E5F"  # C^{over(T)}
  col_CT   <- "#008EC1"  # C^{(T)}
  col_Tm   <- "#FFD300"  # T_m
  col_S    <- "#FF2800"  # S
  col_V    <- "#735D55"  # V
  col_M    <- "#F37022"  # M
  col_R    <- "#2323FF"  # R
  col_CTR <- "#228B22" 
  
  pch_A   <- 16  # solid circle
  pch_E   <- 17  # triangle
  pch_CnT <- 15  # square
  pch_CT  <- 18  # diamond
  pch_Tm  <- 8   # star
  pch_S   <- 1   # open circle
  pch_V   <- 0   # open square
  pch_M   <- 2   # open triangle
  pch_R   <- 4   # x
  pch_CTR <- 3
  
  sA   <- rowSums(run[, Aindex[inds],   drop=FALSE], na.rm=TRUE)
  sE   <- rowSums(run[, Eindex[inds],   drop=FALSE], na.rm=TRUE)
  sCnT <- rowSums(run[, CnTindex[inds], drop=FALSE], na.rm=TRUE)
  sCT  <- rowSums(run[, CTindex[inds],  drop=FALSE], na.rm=TRUE)
  sTm  <- rowSums(run[, Tmindex[inds],  drop=FALSE], na.rm=TRUE)
  sCTR <- rowSums(run[, CTRindex[inds],  drop=FALSE], na.rm=TRUE)
  
  sS   <- rowSums(run[, Sindex[inds],   drop=FALSE], na.rm=TRUE)
  sV   <- rowSums(run[, Vindex[inds],   drop=FALSE], na.rm=TRUE)
  sM   <- as.numeric(run[, Mindex[Mind]])  # vector for points()
  sR   <- rowSums(run[, Rindex[inds],   drop=FALSE], na.rm=TRUE)
  
  y_all1 <- c(sA, sE, sCnT, sCT, sTm, sCTR)
  ymax1 <- max(y_all1, na.rm=TRUE) * 1.05
  yl1 <- c(0, ymax1)
  y_all2 = c(sS, sV, sM, sR)
  ymax2 =  max(y_all2, na.rm=TRUE) * 1.05
  yl2 = c(0, ymax2)
  
  op <- par(mfrow=c(1,2), mar=c(4,4,3,1)+0.2, oma=c(0,0,0.5,0))
  on.exit(par(op), add=TRUE)
  
  plot(times, sA, type="l", lwd=2, col=col_A, ylim = yl1,
       xlab="Date", ylab="Number of people")
  grid(nx=NA, ny=NULL, lty=3)
  lines(times, sE,   lwd=2, col=col_E)
  lines(times, sCnT, lwd=2, col=col_CnT)
  lines(times, sCT,  lwd=2, col=col_CT)
  lines(times, sTm,  lwd=2, col=col_Tm)
  lines(times, sCTR,  lwd=2, col=col_CTR)
  
  # add shapes
  points(times, sA,   pch=pch_A,   cex=cex_pt, col=col_A)
  points(times, sE,   pch=pch_E,   cex=cex_pt, col=col_E)
  points(times, sCnT, pch=pch_CnT, cex=cex_pt, col=col_CnT)
  points(times, sCT,  pch=pch_CT,  cex=cex_pt, col=col_CT)
  points(times, sTm,  pch=pch_Tm,  cex=cex_pt, col=col_Tm)
  points(times, sCTR,  pch=pch_CTR,  cex=cex_pt, col=col_CTR)
  
  legend("top",
         legend=c(expression(A), expression(E),
                  expression(C^{bar(T)}), expression(C^{T}),
                  expression(T),expression(C^{paste("T, Res")})),
         col=c(col_A,col_E,col_CnT,col_CT,col_Tm, col_CTR),
         lty=1, lwd=2, pch=c(pch_A,pch_E,pch_CnT,pch_CT,pch_Tm, pch_CTR),
         bty="n", inset=0.02, cex=0.95)
  
  plot(times, sS, type="l", lwd=2, col=col_S,ylim = yl2,
       xlab="Date", ylab="Number of people")
  grid(nx=NA, ny=NULL, lty=3)
  lines(times, sV, lwd=2, col=col_V)
  lines(times, sM, lwd=2, col=col_M)
  lines(times, sR, lwd=2, col=col_R)
  
  # add shapes
  points(times, sS, pch=pch_S, cex=cex_pt, col=col_S)
  points(times, sV, pch=pch_V, cex=cex_pt, col=col_V)
  points(times, sM, pch=pch_M, cex=cex_pt, col=col_M)
  points(times, sR, pch=pch_R, cex=cex_pt, col=col_R)
  
  legend("top",
         legend=c(expression(S), expression(V), expression(M), expression(R)),
         col=c(col_S,col_V,col_M,col_R),
         lty=1, lwd=2, pch=c(pch_S,pch_V,pch_M,pch_R),
         bty="n", inset=0.02, cex=0.95)
  
  mtext(bquote(bold(.(region))), outer = TRUE, line = -2, cex = 1.1)
}

plotfunc_age_res <- function(run, age = "0", ymax = NULL, cex_pt = 0.9) {
  start_date <- as.Date("2022-01-01")
  times <- seq(start_date, by = "1 week", length.out = 189)
  
  if (age == "0") {
    inds <- c(1, A+1, 2*A+1, 3*A+1)
  } else if (age == "1") {
    inds <- c(1, A+1, 2*A+1, 3*A+1) + 1
  } else if (age == "2") {
    inds <- c(1, A+1, 2*A+1, 3*A+1) + 2
  }
  
  col_A    <- "#191970"  # A
  col_E    <- "#BF1F2B"  # E
  col_CnT  <- "#C79E5F"  # C^{over(T)}
  col_CT   <- "#008EC1"  # C^{(T)}
  col_Tm   <- "#FFD300"  # T_m
  col_S    <- "#FF2800"  # S
  col_V    <- "#735D55"  # V
  col_M    <- "#F37022"  # M
  col_R    <- "#2323FF"  # R
  col_CTR <- "#228B22" 
  
  pch_A   <- 16; pch_E <- 17; pch_CnT <- 15; pch_CT <- 18; pch_Tm <- 8;  pch_CTR <- 3
  pch_S   <- 1;  pch_V <- 0;  pch_M   <- 2; pch_R  <- 4
  
  sA   <- rowSums(run[, Aindex[inds],   drop=FALSE], na.rm=TRUE)
  sE   <- rowSums(run[, Eindex[inds],   drop=FALSE], na.rm=TRUE)
  sCnT <- rowSums(run[, CnTindex[inds], drop=FALSE], na.rm=TRUE)
  sCT  <- rowSums(run[, CTindex[inds],  drop=FALSE], na.rm=TRUE)
  sTm  <- rowSums(run[, Tmindex[inds],  drop=FALSE], na.rm=TRUE)
  sCTR <- rowSums(run[, CTRindex[inds],  drop=FALSE], na.rm=TRUE)
  
  sS   <- rowSums(run[, Sindex[inds],   drop=FALSE], na.rm=TRUE)
  sV   <- rowSums(run[, Vindex[inds],   drop=FALSE], na.rm=TRUE)
  sM   <- rowSums(run[, Mindex,   drop=FALSE], na.rm=TRUE)
  sR   <- rowSums(run[, Rindex[inds],   drop=FALSE], na.rm=TRUE)
  
  y_all1 <- c(sA, sE, sCnT, sCT, sTm, sCTR)
  ymax1 <- max(y_all1, na.rm=TRUE) * 1.05
  yl1 <- c(0, ymax1)
  y_all2 = c(sS, sV, sM, sR)
  ymax2 =  max(y_all2, na.rm=TRUE) * 1.05
  yl2 = c(0, ymax2)
  
  op <- par(mfrow=c(1,2), mar=c(4,4,3,1)+0.2, oma=c(0,0,0.5,0))
  on.exit(par(op), add=TRUE)
  
  plot(times, sA, type="l", lwd=2, col=col_A, ylim= yl1,
       xlab="Date", ylab="Number of people")
  grid(nx=NA, ny=NULL, lty=3)
  lines(times, sE,   lwd=2, col=col_E)
  lines(times, sCnT, lwd=2, col=col_CnT)
  lines(times, sCT,  lwd=2, col=col_CT)
  lines(times, sTm,  lwd=2, col=col_Tm)
  lines(times, sCTR,  lwd=2, col=col_CTR)
  
  points(times, sA,   pch=pch_A,   cex=cex_pt, col=col_A)
  points(times, sE,   pch=pch_E,   cex=cex_pt, col=col_E)
  points(times, sCnT, pch=pch_CnT, cex=cex_pt, col=col_CnT)
  points(times, sCT,  pch=pch_CT,  cex=cex_pt, col=col_CT)
  points(times, sTm,  pch=pch_Tm,  cex=cex_pt, col=col_Tm)
  points(times, sCTR,  pch=pch_CTR,  cex=cex_pt, col=col_CTR)
  
  legend("top",
         legend=c(expression(A), expression(E),
                  expression(C^{bar(T)}), expression(C^{T}),
                  expression(T), expression(C^{paste("T, Res")})),
         col=c(col_A,col_E,col_CnT,col_CT,col_Tm, col_CTR),
         lty=1, lwd=2,
         pch=c(pch_A,pch_E,pch_CnT,pch_CT,pch_Tm, pch_CTR),
         bty="n", inset=0.02, cex=0.95)
  
  
  plot(times, sS, type="l", lwd=2, col=col_S,ylim= yl2,
       xlab="Date", ylab="Number of people")
  grid(nx=NA, ny=NULL, lty=3)
  lines(times, sV, lwd=2, col=col_V)
  if (age == "0") {lines(times, sM, lwd=2, col=col_M)}
  lines(times, sR, lwd=2, col=col_R)
  
  points(times, sS, pch=pch_S, cex=cex_pt, col=col_S)
  points(times, sV, pch=pch_V, cex=cex_pt, col=col_V)
  if (age == "0") {points(times, sM, pch=pch_M, cex=cex_pt, col=col_M)}
  points(times, sR, pch=pch_R, cex=cex_pt, col=col_R)
  
  if (age == "0") {
    legend("top",
           legend=c(expression(S), expression(V), expression(M), expression(R)),
           col=c(col_S,col_V,col_M,col_R),
           lty=1, lwd=2,
           pch=c(pch_S,pch_V,pch_M,pch_R),
           bty="n", inset=0.02, cex=0.95)
  } else {
    legend("top",
           legend=c(expression(S), expression(V), expression(R)),
           col=c(col_S,col_V,col_R),
           lty=1, lwd=2,
           pch=c(pch_S,pch_V,pch_R),
           bty="n", inset=0.02, cex=0.95)
  }
  
  
  if (age == '0'){agething  = '<1 years'}
  if (age == '1'){agething  = '1-10 years'}
  if (age == '2'){agething  = '10+ years'}
  mtext(bquote(bold(.(agething))), outer = TRUE, line = -2, cex = 1.1)
}



#========================================================================
#                           Model
#========================================================================

t_period = 188
times = seq(0, t_period, by = 1)

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

#========================================================================
#                           Model (Drug-Resistance)
#========================================================================

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

#========================================================================
#                              UI
#========================================================================

ui <- fluidPage(
  withMathJax(),
  tabsetPanel(
    id = "outer_tabs",
    tabPanel(
      title = strong("Pertussis Transmission Model"),
      titlePanel("Parameter Values"),
      sidebarLayout(
        sidebarPanel(
          tabsetPanel(
            id = "sidebar_tabs",
            
            tabPanel("Initial Values",
                     h4("Initial Values"),
                     sliderInput("V0", "Initial Vaccinated Population", 0, 1,0.25, 0.01),
                     sliderInput("R0", "Initial Recovered Population", 0, 1, 0.25, 0.01)
            ),
            
            tabPanel("Force of Infection",
                     h4(HTML("Baseline transmission \\(\\beta^{0}\\)")),
                     sliderInput("beta10", HTML("Northeast \\(\\beta_{1}^{0}\\)"), 0, 2, 0.34, 0.01),
                     sliderInput("beta20", HTML("Midwest \\(\\beta_{2}^{0}\\)"),   0, 2, 0.22, 0.01),
                     sliderInput("beta30", HTML("West \\(\\beta_{3}^{0}\\)"),      0, 2, 0.35, 0.01),
                     sliderInput("beta40", HTML("South \\(\\beta_{4}^{0}\\)"),     0, 2, 0.38, 0.01),
                     
                     tags$hr(), 
                     h4(HTML("Gaussian bump")),
                     
                     sliderInput("beta11", HTML("Amplitude NE \\(\\beta_{1}^{1}\\)"), 0, 10, 5.52, 0.01),
                     sliderInput("beta21", HTML("Amplitude MW \\(\\beta_{2}^{1}\\)"), 0, 10, 6.56, 0.01),
                     sliderInput("beta31", HTML("Amplitude W  \\(\\beta_{3}^{1}\\)"), 0, 10, 3.76, 0.01),
                     sliderInput("beta41", HTML("Amplitude S  \\(\\beta_{4}^{1}\\)"), 0, 10, 2.59, 0.01),
                     
                     sliderInput("sig1",  HTML("Width NE \\(\\sigma_{1}^{G}\\)"), 0.5, 20, 5.29, 0.01),
                     sliderInput("sig2",  HTML("Width MW \\(\\sigma_{2}^{G}\\)"), 0.5, 20, 10.01, 0.01),
                     sliderInput("sig3",  HTML("Width W  \\(\\sigma_{3}^{G}\\)"), 0.5, 20, 6.68, 0.01),
                     sliderInput("sig4",  HTML("Width S  \\(\\sigma_{4}^{G}\\)"), 0.5, 20, 6.25, 0.01),
                     
                     sliderInput("phi1",  HTML("Centre NE \\(\\phi_{1}\\)"), 0.5, 188, 113, 0.01),
                     sliderInput("phi2",  HTML("Centre MW \\(\\phi_{2}\\)"), 0.5, 188, 129, 0.01),
                     sliderInput("phi3",  HTML("Centre W  \\(\\phi_{3}\\)"), 0.5, 188, 130, 0.01),
                     sliderInput("phi4",  HTML("Centre S  \\(\\phi_{4}\\)"), 0.5, 188, 123, 0.01)
            ),
            
            tabPanel("Migration",
                     h4("Yearly migration flows"),
                     sliderInput("m12", "NE → MW", 0, 1e6, 192109/3, 1),
                     sliderInput("m13", "NE → W",  0, 1e6, 192109/3, 1),
                     sliderInput("m14", "NE → S",  0, 1e6, 192109/3, 1),
                     sliderInput("m21", "MW → NE", 0, 1e6, 49214/3,   1),
                     sliderInput("m23", "MW → W",  0, 1e6, 49214/3,   1),
                     sliderInput("m24", "MW → S",  0, 1e6, 49214/3,   1),
                     sliderInput("m31", "W → NE",  0, 1e6, 169681/3,  1),
                     sliderInput("m32", "W → MW",  0, 1e6, 169681/3,  1),
                     sliderInput("m34", "W → S",   0, 1e6, 169681/3,  1),
                     sliderInput("m41", "S → NE",  0, 1e6, 0,         1),
                     sliderInput("m42", "S → MW",  0, 1e6, 0,         1),
                     sliderInput("m43", "S → W",   0, 1e6, 0,         1)
            ),
            tabPanel("Vaccination",
                     h4("Vaccination"),
                     sliderInput("V_tot", "Yearly DTaP/Tdap vaccines", 
                                 min = 0, max = 1e8, value = 503068145/12, step = 1e4),
                     sliderInput("eps1",  HTML("Vaccine efficacy: <1 years"), 0, 1, 0.8, 0.01),
                     sliderInput("eps2",  HTML("Vaccine efficacy: 1–10 years"), 0, 1, 0.8, 0.01),
                     sliderInput("eps3",  HTML("Vaccine efficacy: ≥ 10 years"), 0, 1, 0.5, 0.01)
            ),
            tabPanel("Infectiousness",
                     h4("Infectiousness"),
                     sliderInput("zetaA", HTML("Relative Infectiousness: Asymptomatic \\(\\zeta^{A}\\)"), 
                                 min = 0, max = 1, value = 0.7, step = 0.01),
                     sliderInput("pA1",   HTML("Proportion Asymptomatic: <1 years"), 0, 1, 0.1, 0.01),
                     sliderInput("pA2",   HTML("Proportion Asymptomatic: 1–10 years"), 0, 1, 0.3, 0.01),
                     sliderInput("pA3",   HTML("Proportion Asymptomatic: ≥ 10 years"), 0, 1, 0.6, 0.01)
            ),
            
            tabPanel("Transmission",
                     sliderInput("ssigma",  "Latency period (days)", 3, 50, 7, 1),
                     sliderInput("ddelta",  "Loss of symptoms period (days)", 3, 50, 14, 1),
                     sliderInput("ggammaI", "Recovery period: Non-treated (days)", 3, 50, 21, 1),
                     sliderInput("ggammaT", "Recovery period: Treated (days)",     3, 50, 5,  1)
            ),
            
            tabPanel("Treatment Seeking",
                     h4("Treatment Seeking"),
                     sliderInput("pT1", "Proportion who seek treatment: <1 years", 0, 1, 0.8, 0.01),
                     sliderInput("pT2", "Proportion who seek treatment: 1–10 years", 0, 1, 0.4, 0.01),
                     sliderInput("pT3", "Proportion who seek treatment: ≥ 10 years", 0, 1, 0.4, 0.01),
                     tags$hr(),
                     sliderInput("ttau1", "Time to treatment: <1 years (days)", 3, 50, 5.6, 1),
                     sliderInput("ttau2", "Time to treatment: 1–10 years (days)", 3, 50, 13.8, 1),
                     sliderInput("ttau3", "Time to treatment: ≥ 10 years (days)", 3, 50, 13.8, 1)
            ),
            
            tabPanel("Immunity",
                     h4("Immunity"),
                     sliderInput("oomegaM", "Maternal immunity period (weeks)", 0, 52, 1/(7*log(2)/30), 1),
                     sliderInput("oomegaR", "Natural immunity period (years)",  0, 50, 30, 1),
                     sliderInput("oomegaV", "Vaccination immunity period (years)", 0, 50, 4, 1),
                     tags$hr(),
                     sliderInput("pi0", "Proportion of maternally immune births", 0, 1, 0.55, 0.01)
            ),
            
            tabPanel("Births and Deaths",
                     h4("Demography"),
                     sliderInput("Bus", "Number of yearly births",  0, 1e7, 3622673, 100),
                     sliderInput("Dus", "Number of yearly deaths",  0, 1e7, 3287000, 100)
            ),
            
            tabPanel("Reporting %",
                     h4("Reporting by Region"),
                     sliderInput("rho_rep1", "Reporting %: Northeast", 0, 1, 0.95, 0.01),
                     sliderInput("rho_rep2", "Reporting %: Midwest",   0, 1, 0.95, 0.01),
                     sliderInput("rho_rep3", "Reporting %: West",      0, 1, 0.95, 0.01),
                     sliderInput("rho_rep4", "Reporting %: South",     0, 1, 0.95, 0.01)
            ),
            
            tabPanel("Drug-Resistance",
                     h4("Drug-Resistance"),
                     sliderInput("pR",   HTML("Proportion whom have primary resistance \\(p^{Res}\\)"), 0, 1, 0.001, 0.001),
                     sliderInput("etaa", HTML("Mean time to acquired drug-resistance (days)"), 3, 350, 50, 1),
                     sliderInput("ggammaR", "Recovery period: Drug-Resistant (days)", 3, 50, 10, 1)
            )
          )
        ),
        mainPanel(
          tabsetPanel(
            id = "model_tabs",
            tabPanel("Non Drug-Resistant Model",
                     tabsetPanel(
                       id = "nr_tabs",
                       tabPanel("Northeast", plotOutput("Northeast_NR")),
                       tabPanel("Midwest",   plotOutput("Midwest_NR")),
                       tabPanel("West",      plotOutput("West_NR")),
                       tabPanel("South",     plotOutput("South_NR")),
                       tabPanel("Age 0–1",   plotOutput("Age0_NR")),
                       tabPanel("Age 1–10",  plotOutput("Age1_NR")),
                       tabPanel("Age 10+",   plotOutput("Age2_NR"))
                     )
            ),
            tabPanel("Drug-Resistant Model",
                     tabsetPanel(
                       id = "r_tabs",
                       tabPanel("Northeast", plotOutput("Northeast_R")),
                       tabPanel("Midwest",   plotOutput("Midwest_R")),
                       tabPanel("West",      plotOutput("West_R")),
                       tabPanel("South",     plotOutput("South_R")),
                       tabPanel("Age 0–1",   plotOutput("Age0_R")),
                       tabPanel("Age 1–10",  plotOutput("Age1_R")),
                       tabPanel("Age 10+",   plotOutput("Age2_R"))
                     )
            ),
            tabPanel("Incidence",   plotOutput("Incidence"))
          )
        )
      )
    )
  )
)



server <- function(input, output) {
  Mreact <- reactive({
    M <- matrix(
      c(0, input$m12, input$m13, input$m14,
        input$m21, 0, input$m23, input$m24,
        input$m31, input$m32, 0, input$m34,
        input$m41, input$m42, input$m43, 0),
      nrow = 4, byrow = TRUE
    )/52
    M <- M / matrix(c(rep(pop1,P), rep(pop2,P), rep(pop3,P), rep(pop4,P)), P, P, byrow = TRUE) / 3
    M
  })

  b0react <- reactive({
    b0 <- c(
      input$Bus*pop1/(pop1+pop2+pop3+pop4)/52,
      input$Bus*pop2/(pop1+pop2+pop3+pop4)/52,
      input$Bus*pop3/(pop1+pop2+pop3+pop4)/52,
      input$Bus*pop4/(pop1+pop2+pop3+pop4)/52
    )
    b0 / Pop[1, ]
  })
  
  # Parameters shared between both Models
  parametersR <- reactive(
    list(
    Marr = array(Mreact(), dim = c(P,P,A)), # migration not a function of age
    zetaA   = matrix(input$zetaA, A, P),
    zetanT  = matrix(1, A, P),
    zetaT   = matrix(1, A, P),
    v       = matrix(input$V_tot/Ptot/52, A, P),
    eps     = matrix(c(rep(input$eps1, P), rep(input$eps2, P), rep(input$eps3, P)), A, P, byrow = TRUE),
    pA      = matrix(c(rep(input$pA1, P), rep(input$pA2, P), rep(input$pA3, P)),A, P, byrow = TRUE),
    pT      = matrix(c(rep(input$pT1, P), rep(input$pT2, P), rep(input$pT3, P)),A, P, byrow = TRUE),
    sigma   = matrix(7/input$ssigma, A, P),
    delta   = matrix(7/input$ddelta, A, P),
    alphas  = matrix(c(rep(1, 4), rep(.1, 4), rep(0, 4)), A, P, byrow = TRUE)/52,
    gammaI  = matrix(7/input$ggammaI, A, P),
    tau     = matrix(c(rep(7/input$ttau1, P), rep(7/input$ttau2, P),rep(7/input$ttau3, P)), A, P, byrow = TRUE) ,
    gammaT  = matrix(7/input$ggammaT, A, P),
    omegaM  = matrix(1/input$oomegaM, 1, P),
    omegaV  = matrix(1/input$oomegaV/52, A, P),
    omegaR  = matrix(1/input$oomegaR/52, A, P),
    mus     = matrix(input$Dus/Ptot/52, A, P),
    b0      = b0react(),
    pi0     = rep(input$pi0, P),
    beta0 = matrix(c(rep(input$beta10, A), rep(input$beta20, A),rep(input$beta30, A), rep(input$beta40, A)), A, P, byrow = FALSE),
    beta1 =  matrix(c(rep(input$beta11, A), rep(input$beta21, A),rep(input$beta31, A), rep(input$beta41, A)), A, P, byrow = FALSE),
    sig = matrix(c(rep(input$sig1, A), rep(input$sig2, A),rep(input$sig3, A), rep(input$sig4, A)), A, P, byrow = FALSE),
    phi = matrix(c(rep(input$phi1, A), rep(input$phi2, A),rep(input$phi3, A), rep(input$phi4, A)), A, P, byrow = FALSE),
    # Drug-resistance parameters
    zetaR =  matrix(1, A, P),
    pR      = input$pR,
    eta     = matrix(7/input$etaa, A, P),
    gammaR = matrix(7/input$ggammaR, A, P) 
    ))
  
  
  # Initial for Non-resistant Model
  y0react <- reactive({
    M0    <- rep(1, P)
    V0    <- input$V0 * Pop
    E0    <- matrix(1, A, P)
    A0    <- matrix(1, A, P)
    CnT0  <- matrix(1, A, P)
    # CT0   <- matrix(rep(Incidence_data[1, ] / A, A), A, P, byrow = TRUE)
    CT0   <-  matrix(1, A, P)
    Tm0   <- matrix(1, A, P)
    R0    <- input$R0 * Pop
    CInc0 <- matrix(0, A, P)
    S0    <- (Pop - V0 - R0)

    c(
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
  })
  
  # Initial for Resistant Model
  y0reactR <- reactive({
    M0    <- rep(1, P)
    V0    <- input$V0 * Pop
    E0    <- matrix(1, A, P)
    A0    <- matrix(1, A, P)
    CnT0  <- matrix(1, A, P)
    # CT0   <- matrix(rep(Incidence_data[1, ] / A, A), A, P, byrow = TRUE)
    CT0   <-  matrix(1, A, P)
    Tm0   <- matrix(1, A, P)
    R0    <- input$R0 * Pop
    CInc0 <- matrix(0, A, P)
    S0    <- (Pop - V0 - R0)
    CTR   <- matrix(1, A, P)
    
    c(
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
  })
  
  rhoreact = reactive({c(input$rho_rep1, input$rho_rep2, input$rho_rep3, input$rho_rep4)})
  # Non-Resistant
  outNR = reactive(ode(y = y0react(),times = times,func = rhs_vec,parms = parametersR(),method = "lsoda" ))

  output$Northeast_NR <- renderPlot({ plotfunc_reg(run = outNR(), region = "Northeast") })
  output$Midwest_NR   <- renderPlot({ plotfunc_reg(run = outNR(), region = "Midwest") })
  output$West_NR      <- renderPlot({  plotfunc_reg(run = outNR(), region = "West") })
  output$South_NR     <- renderPlot({ plotfunc_reg(run = outNR(), region = "South") })
  
  output$Age0_NR <- renderPlot({  plotfunc_age(run = outNR(), age = '0') })
  output$Age1_NR <- renderPlot({  plotfunc_age(run = outNR(), age = '1') })
  output$Age2_NR <- renderPlot({  plotfunc_age(run = outNR(), age = '2') })
  
  # Resistant
  outR = reactive(ode(y = y0reactR(),times = times,func = rhs_vec_res,parms = parametersR(),method = "lsoda" ))
  
  output$Northeast_R <- renderPlot({ plotfunc_reg_res(run = outR(), region = "Northeast") })
  output$Midwest_R   <- renderPlot({ plotfunc_reg_res(run = outR(), region = "Midwest") })
  output$West_R      <- renderPlot({  plotfunc_reg_res(run = outR(), region = "West") })
  output$South_R     <- renderPlot({ plotfunc_reg_res(run = outR(), region = "South") })
  
  output$Age0_R <- renderPlot({  plotfunc_age_res(run = outR(), age = '0') })
  output$Age1_R <- renderPlot({  plotfunc_age_res(run = outR(), age = '1') })
  output$Age2_R <- renderPlot({  plotfunc_age_res(run = outR(), age = '2') })
  
  # Incidence
  output$Incidence <- renderPlot({  plot_incidence(run=outNR(), run_res = outR(), rho =rhoreact())})
  
}

# Run the application 
shinyApp(ui = ui, server = server)


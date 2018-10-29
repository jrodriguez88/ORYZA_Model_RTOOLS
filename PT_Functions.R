#####################################
##### SOIL FUCTIONS for SAXTON & RAWLS (SR): SOIL WATER CHARACTERISTICS ESTIMATES
#####
##### by JRE- https://github.com/jrodriguez88 
###############################################################################

# METADATA FOR ORYZA_model
#
C <- 0.32
S <- 0.35
Si <- 1-C-S
OM <- 2
SPOR <- 0.48
SBDM <- 1.47
DEPTH <- 30

#C <- 0.35*100
#S <- 0.33*100
#Si <- 100-C-S
#OM <- 0.007*100
#SPOR <- 0.48*100
#SBDM <- 1.4


# 1500 kPa moisture, %v
WCWP_Saxton <- function(S, C, OM) {
    WCWP_i <- -0.024*S + 0.487*C + 0.006*OM +
               0.005*(S*OM) - 0.013*(C*OM) +
               0.068 *(S*C) + 0.031
    
    WCWP <- WCWP_i + (0.14*WCWP_i - 0.02)
   
    return(WCWP)             
} #Eq 1
#WCWP_Saxton(S, C,OM)

# 33 kPa moisture, normal density, %v
WCFC_Saxton <- function(S,C,OM) {
    WCFC_i <- -0.251*S + 0.195*C + 0.011*OM +
               0.006*(S*OM) - 0.027*(C*OM) +
               0.452 *(S*C) + 0.299
    
        WCFC <- WCFC_i + (1.283*(WCFC_i^2) - (0.374*WCFC_i) - 0.015)
    
    return(WCFC)
    
}   #Eq 2
#WCFC_Saxton(S, C,OM)

#SAT-33 kPa moisture, normal density %v
WCSAT_Saxton <- function(S,C,OM) {
    
    WCST_i <- 0.278*S + 0.034*C + 0.022*OM -
        0.018*(S*OM) - 0.027*(C*OM) +
        0.584 *(S*C) + 0.078
    
    WCSAT <- WCST_i + (0.6360*WCST_i) - 0.107
    
    return(WCSAT)
    
}  #Eq 3
#WCSAT_Saxton(S,C,OM)

# Tension at air entry (bubbling pressure), kPa
TAE_BP_Saxton <- function(S,C,OM) {
    WCSAT <- WCSAT_Saxton(S,C,OM)
    TAE_BPi <- -2.27 -(27.93*C) -(81.97*WCSAT) + (71.12*S*WCSAT) + (8.29*C*WCSAT) + (14.05*S*C) + 27.16

    TAE_BP <- TAE_BPi + (0.02*(TAE_BPi^2) -0.113*TAE_BPi -0.70)

    return(TAE_BP)
    
} #Eq 4
#TAE_BP_Saxton(S,C,OM)

# Saturated moisture (0 kPa), normal density, %v
WCST_Saxton <- function(S,C,OM) {
    
    WCFC <- WCFC_Saxton(S,C,OM)    
    WCSAT <- WCSAT_Saxton(S,C,OM)
    WCST <- WCFC + WCSAT -0.097*S + 0.043
    
    return(WCST)
    
}   #Eq 5
#WCST_Saxton(S,C,OM)

#pN Normal density, g/cm³
pN_Saxton <- function(S,C,OM) {
    WCST <- WCST_Saxton(S,C,OM)
    pN <- (1 - WCST)*2.65
    return(pN)
}
#pN_Saxton(S,C,OM)
#pDF <- pN_cal(S,C,OM) #DF Compactation soil factor (0.9-1.3)
#pB <- pN

# Saturated moisture (0 kPa), adjusted density, %v
WCST_AD_Saxton <- 1-(SBDM/2.65)


# Saturated conductivity (matric soil), mm/h
SSKS_Saxton <- function(S,C,OM, SBDM){
    WCFC <- WCFC_Saxton(S,C,OM)
    WCWP <- WCWP_Saxton(S,C,OM)
#    WCST <- WCST_cal(S,C,OM)
    WCST_AD <- 1-(SBDM/2.65)
    B <- (log(1500) - log(33))/(log(WCFC) - log(WCWP))
    alp <- 1/B
    
    SSKS <- 1930*(WCST_AD - WCFC)^(3-alp)
    
    return(SSKS*2.4)
    
}
#SSKS_Saxton(S,C,OM, SBDM)


########################################################
########### Predicting Soil properties in the tropics
########### Budiman Minasny, Alfred E. Hartemink. (2011)

#Soil bulk density with OM effect
SDB_Minasny <- function(DEPTH, S, OM, BD_OM=0.224){
    SBDmin <- 0.93 + (0.049*log(DEPTH)) + (0.005*S*100) + 0.000065*((100*S-38.96)^2)
    SBD_M <- 100/((OM/BD_OM)+((100-OM)/SBDmin))
    return(SBD_M)
} #Eq. 1 & 2

# Saturated water content at (-10kPa), may consider WCST
WCST_Minasny <- function(S, SBDM){
    WCST <- 59.9 - (8.78*SBDM) - (0.31*S*100)
    return(WCST)
}  #Eq. 3

#Saturated water content at (-33kPa)
WCFC_Minasny <- function(S, SBDM){
    WCFC <- 56.5 - (7.49*SBDM) -(0.34*S*100)
    return(WCFC)
}  #Eq. 4

# Saturated water content at (-1500kPa)
WCWP_Minasny <- function(OM, C){
    WCWP <- 7.95 + (0.86*OM) + (0.4*C*100) - 0.004*((100*C-37.7)^2)
    return(WCWP)
}    #Eq. 5



#######  PEDOTRANSFER FUNCTIONS FOR TROPICAL SOILS Cap21
######  J. Tomasella, and M. Hodnett (2004)
###### Use "Me" (Equivalent moisture) as predictor, the gravimetric water content at -33 kPa provides an accurate estimation of the moisture equivalent
###### Tomasella et al. (2003)

#Me <- GWCFC <- 0.3
#
#x7 <- -1.0553 + 0.0533922*C*S
#x8 <- -1.07131 + 0.0649731*S
#x9 <- -6.18145 + 4.95385*SBDM
#z7 <- 0.159379 + 0.137397*(x7^2) + 0.265398*x8 + 0.519965*x7*x8 - 0.276027*(x8^2) - 0.362393*x7*(x8^2) - 0.702969*x9 - 0.222252*x8*x9 - 0.244634*x7*x8*x9 - 0.092267*x7*(x9^2) + 0.0332669*(x9^3)
#
#x14 <- -1.05501 + 0.0650857*Si*100
#x15 <- -2.07588 + 0.0423954*C*100
#x16 <- -6.03402 + 4.80572*SBDM
#x17 <- -2.18409 + 8.84963*GWCFC
#z9 <- 0.175202 + 1.818513*x17 - 0.0996042*(x17^2) + 0.327915*x16 - 0.0758657*(x16^2)
#z10 <- 0.929344*z9 + 0.132519*x14
#z11 <- 0.191452 + 1.25652*x17 - 0.079098*(x17^2) + 0.393814*x16 + 0.152095*x17*x16
#z13 <- 0.235084 + 0.33033*x15 - 0.191838*(x15^2) + 0.0543679*(x15^3) + 0.977685*x17 + 0.304174*x15*x17 - 0.218857*(x17^2) - 0.164373*x15*(x17^2) + 0.0415057*(x17^3) + 0.373361*x16 + 0.0811861*x17*x16 - 0.0768087*x15*x17*x16

x1 <- -1.06790 + 0.0536107*CS
x2 <- -1.17468 + 0.0808098*FS
x3 <- -1.05976 + 0.0650437*Si
x4 <- -2.10641 + 0.0427715*C
x5 <- -2.21391 + 8.9226800*Me

x6  6.03516  4.81197  Db
z1  4.25417x1  2.72322x2  3.07242x3
 5.00093x4  0.195062x5  0.377081x6
z2  0.110144  0.640373z1  1.16884(z1)2
 0.155394x4  0.358591z1x4
1.00996(z1)2x4  0.126617 (x4)3
  10 0.0736768  0.789068z2
z3  0.37398x1  0.0940338(x1)3  0.838535x1x5
 0.590525 (x1)2x5  0.76113 (x5)2
 0.789465x1(x5)2  0.273647(x5)3
 0.512764x6  0.455363x1x6  0.38428(x1)2x6
 0.731809x5x6  1.00484x1x5x6  0.172341(x5)2x6
 0.219746(x6)2  0.367679x1(x6)2  0.131251(x6)3
z4  0.360294  0.76878z3  0.0770122(z3)3
 0.193142x2  0.121583z3x2
 0.0889415(z3)2x2  0.284168(x2)2
 0.0674767(x2)3  0.202897x3  0.341951z3x3
 0.270616x2x3  0.0880845(x2)2x3  0.24982(x3)2
 0.102658x2(x3)2  0.0801841(x3)3
n  10 0.140543  0.0797516z4
z5  0.164417  0.126139 (x1)2  0.281797x3
 0.484823x1x3  0.293866 (x3)2
 0.354924x1 (x3)2  0.705803x6
 0.189153x3x6  0.267997x1x3x6
 0.023954 (x3)2x6  0.0918816x1(x6)2
 0.0323997(x6)3
0  0.515224  0.100899z5
z6  0.12867  0.492412x3  0.787425x5
 0.235254x3x5;

r  0.161487  0.101111  z6





# Saturated water content at (0kPa), may consider WCST
WCST_Tomasella1 <- function(C, S, SBDM){
    x7 <- -1.0553 + 0.0533922*C*S
    x8 <- -1.07131 + 0.0649731*S
    x9 <- -6.18145 + 4.95385*SBDM
    z7 <- 0.159379 + 0.137397*(x7^2) + 0.265398*x8 + 0.519965*x7*x8 - 0.276027*(x8^2) - 0.362393*x7*(x8^2) - 0.702969*x9 - 0.222252*x8*x9 - 0.244634*x7*x8*x9 - 0.092267*x7*(x9^2) + 0.0332669*(x9^3)
    
    WCST_T1 <- 0.517589 + 0.0994301*z7
    return(WCST_T1)
}  

# Saturated water content at (-10kPa), may consider WCST
WC10_Tomasella1 <- function(Si, C, SBDM, GWCFC) {
    x14 <- -1.05501 + 0.0650857*Si*100
    x15 <- -2.07588 + 0.0423954*C*100
    x16 <- -6.03402 + 4.80572*SBDM
    x17 <- -2.18409 + 8.84963*GWCFC
    z9 <- 0.175202 + 1.818513*x17 - 0.0996042*(x17^2) + 0.327915*x16 - 0.0758657*(x16^2)
    z10 <- 0.929344*z9 + 0.132519*x14
    
    WC10_T1 <- 0.339255 + 0.112526*z10
    
    return(WC10_T1)
}

#Saturated water content at (-33kPa)    
WCFC_Tomasella1 <- function(SBDM, GWCFC) {
    x16 <- -6.03402 + 4.80572*SBDM
    x17 <- -2.18409 + 8.84963*GWCFC
    z11 <- 0.191452 + 1.25652*x17 - 0.079098*(x17^2) + 0.393814*x16 + 0.152095*x17*x16
    
    WCFC_T1 <- 0.28951 + 0.103815*z11
    
    return(WCFC_T1)
}

# Saturated water content at (-1500kPa)    
WCWP_Tomasella1 <- function(C, SBDM, GWCFC){
    
    x15 <- -2.07588 + 0.0423954*C*100
    x16 <- -6.03402 + 4.80572*SBDM
    x17 <- -2.18409 + 8.84963*GWCFC
    z13 <- 0.235084 + 0.33033*x15 - 0.191838*(x15^2) + 0.0543679*(x15^3) + 0.977685*x17 + 0.304174*x15*x17 - 0.218857*(x17^2) - 0.164373*x15*(x17^2) + 0.0415057*(x17^3) + 0.373361*x16 + 0.0811861*x17*x16 - 0.0768087*x15*x17*x16
    
    WCWP_T1 <- 0.214008 + 0.0862945*z13
}


##############################
######### RAWLS (1986)



SSKS_Rawls <- function(S,C,OM){
    SST_Rawls <- 0.332 - (0.0007251*S*100) + 0.1276*log10(C*100)
    SSKS_R <- exp(12.012 - (0.0755*S*100) + (-3.8950 + (0.03671*S*100) - (0.1103*C*100) + 8.7546*(10^-4)*(100*C^2)*(1/(SST_Rawls))))
    
    return(SSKS_R)
} 
SST <- SSKS_Rawls(S,C,OM)

curve(exp(12.012 - (0.0755*S*100) + (-3.8950 + (0.03671*S*100) - (0.1103*C*100) + 8.7546*(10^-4)*(100*C^2)*(1/(x)))),
        from=0, to=SST, 
        xlab='Moisture (m3/m3)',
        ylab='SSK (cm/d)',
        main='K. E. Saxton, W.J. Rawls, J.S. Romberger, and R. I. Papendick',
        col='mediumblue')
    abline(v=SST, col="red")
    

#########################################
##### Accuracy of sample dimension-dependent pedotransfer functions in estimation of soil saturated hydraulic conductivity
##### Behzad Ghanbarian, Vahid Taslimitehrani, Yakov A.Pachepsky
##### https://doi.org/10.1016/j.catena.2016.10.015 
##### XXXX_GTP

# Brakensiek et al. (1984)
24*exp(19.52348*SPOR - 8.96847 - 0.028212*C + 0.00018107*(S^2) - 0.0094125*(C^2) - 8.395215*(SPOR^2) + 0.077718*(SPOR*S) - 0.00298*(SPOR^2)*(S^2) -0.019492*(SPOR^2)*(C^2) + 0.0000173*(C*S^2) + 0.02733*(SPOR*C^2) + 0.001434*(SPOR*S^2)- 0.0000035*(S*C^2))

# Campbell and Shiozawa (1994)
129.6*exp(-0.07*Si - 0.167*C)

# Cosby et al. (1984)
60.96*10^((-0.6+0.01268*S) - (0.0064*C))

#Jabro (1992)
24*(9.56 - 0.81*log(Si) - 1.09*log(C) - 4.64*SBDM)

# Puckett et al. (1985)
376.7*exp(-0.1975*C)

# Dane and Puckett (1994)
729.22*exp(-0.144*C*100)

# Saxton et al. (1986)
24*exp(12.012 - (0.0755*S*100))





#######################
### Method developed by Suleiman and Ritchie (2001), 

SSSK_Sul_Rit <- function(S, C, OM, SPOR){
    WCFC <- WCFC_Saxton(S,C,OM)*100
    SSKS <- 75*24*((100*SPOR-WCFC)/(WCFC)^2)
    
    return(SSKS)
    
}
SSSK_Sul_Rit(S,C,OM, SPOR)






    
                  







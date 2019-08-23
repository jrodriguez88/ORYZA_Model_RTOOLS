#####################################
##### SOIL FUCTIONS for SAXTON & RAWLS (SR): SOIL WATER CHARACTERISTICS ESTIMATES
##### http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.452.9733&rep=rep1&type=pdf
##### by JRE- https://github.com/jrodriguez88 
###############################################################################

# METADATA FOR ORYZA_model
#
#C <- 0.5
#S <- 0.1
#Si <- 1-C-S
#OM <- 1.2
#SPOR <- 0.48
#SBDM <- 1.41
#DEPTH <- 20
#GWCFC <- 0.3

#C <- 0.35*100
#S <- 0.33*100
#Si <- 100-C-S
#OM <- 0.007*100
#SPOR <- 0.48*100
#SBDM <- 1.4


# 1500 kPa moisture, %v
WCWP_Saxton <- function(S, C, OM) {
    WCWP_i <- -0.024*S/100 + 0.487*C/100 + 0.006*OM +
               0.005*(S*OM/100) - 0.013*(C*OM/100) +
               0.068 *(S/100)*(C/100) + 0.031
    
    WCWP <- WCWP_i + (0.14*WCWP_i - 0.02)
   
    return(WCWP)             
} #Eq 1
#WCWP_Saxton(S, C,OM)

# 33 kPa moisture, normal density, %v
WCFC_Saxton <- function(S,C,OM) {
    WCFC_i <- -0.251*S/100 + 0.195*C/100 + 0.011*OM +
               0.006*(S*OM/100) - 0.027*(C*OM/100) +
               0.452*(S/100)*(C/100) + 0.299
    
        WCFC <- WCFC_i + (1.283*(WCFC_i^2) - (0.374*WCFC_i) - 0.015)
    
    return(WCFC)
    
}   #Eq 2
#WCFC_Saxton(S, C,OM)

#SAT-33 kPa moisture, normal density %v
WCSAT_Saxton <- function(S,C,OM) {
    
    WCST_i <- 0.278*(S/100) + 0.034*(C/100) + 0.022*OM -
        0.018*(S/100)*OM - 0.027*(C/100)*OM +
        0.584 *(S/100)*(C/100) + 0.078
    
    WCSAT <- WCST_i + (0.6360*WCST_i) - 0.107
    
    return(WCSAT)
    
}  #Eq 3
#WCSAT_Saxton(S,C,OM)

# Tension at air entry (bubbling pressure), kPa
TAE_BP_Saxton <- function(S,C,OM) {
    WCSAT <- WCSAT_Saxton(S,C,OM)
    TAE_BPi <- -2.27 -(27.93*C/100) -(81.97*WCSAT) + (71.12*S*WCSAT/100) + (8.29*C*WCSAT/100) + (14.05*(S/100)*(C/100)) + 27.16

    TAE_BP <- TAE_BPi + (0.02*(TAE_BPi^2) -0.113*TAE_BPi -0.70)

    return(TAE_BP)
    
} #Eq 4
#TAE_BP_Saxton(S,C,OM)

# Saturated moisture (0 kPa), normal density, %v
WCST_Saxton1 <- function(S,C,OM) {
    
    WCFC <- WCFC_Saxton(S,C,OM)    
    WCSAT <- WCSAT_Saxton(S,C,OM)
    WCST <- WCFC + WCSAT -0.097*(S/100) + 0.043
    
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
WCST_Saxton <- function(SBDM){
    
    WCST_S <- 1-(SBDM/2.65)
    return(WCST_S)
} 



########################################################
########### Predicting Soil properties in the tropics
########### Budiman Minasny, Alfred E. Hartemink. (2011)

#Soil bulk density with OM effect
SDB_Minasny <- function(DEPTH, S, OM, BD_OM=0.224){
    SBDmin <- 0.93 + (0.049*log(DEPTH)) + (0.005*S) + 0.000065*((S-38.96)^2)
    SBD_M <- 100/((OM/BD_OM)+((100-OM)/SBDmin))
    return(SBD_M)
} #Eq. 1 & 2

# Saturated water content at (-10kPa), may consider WCST
WCST_Minasny <- function(S, SBDM){
    WCST <- 59.9 - (8.78*SBDM) - (0.31*S)
    return(WCST)
}  #Eq. 3

#Saturated water content at (-33kPa)
WCFC_Minasny <- function(S, SBDM){
    WCFC <- 56.5 - (7.49*SBDM) -(0.34*S)
    return(WCFC)
}  #Eq. 4

# Saturated water content at (-1500kPa)
WCWP_Minasny <- function(C, OM){
    WCWP <- 7.95 + (0.86*OM) + (0.4*C) - 0.004*((C-37.7)^2)
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

# Alpha van Genuchten parameter
Alpha_Tomasella1 <- function(S, C, SBDM, CS, FS, GWCFC) {
    Si <- 100-C-S
    x1 <- -1.06790 + 0.0536107*CS  #CS Coarse sand
    x2 <- -1.17468 + 0.0808098*FS  #FS Fine sand
    x3 <- -1.05976 + 0.0650437*Si
    x4 <- -2.10641 + 0.0427715*C
    x5 <- -2.21391 + 8.9226800*GWCFC
    
    x6 <- -6.03516 + 4.81197*SMBD
    z1 <-  4.25417*x1 + 2.72322*x2 + 3.07242*x3 + 5.00093*x4 - 0.195062*x5 - 0.377081*x6
    z2 <- 0.110144 + 0.640373*z1 - 1.16884*(z1)^2 - 0.155394*x4 - 0.358591*z1*x4 - 1.00996*x4*(z1)^2 + 0.126617*(x4)^3

    alpha_T <-  10^(0.0736768 + 0.789068*z2)

return(alpha_T)
}

# N van Genuchten parameter
N_Tomasella1 <-  function(S, C, SBDM, CS, FS, GWCFC) {
    Si <- 100-C-S
    x1 <- -1.06790 + 0.0536107*CS  #CS Coarse sand
    x2 <- -1.17468 + 0.0808098*FS  #FS Fine sand
    x3 <- -1.05976 + 0.0650437*Si
    x4 <- -2.10641 + 0.0427715*C
    x5 <- -2.21391 + 8.9226800*GWCFC
    x6 <- -6.03516 + 4.81197*SMBD
    
    z3 <- 0.37398*x1 - 0.0940338*(x1)^3 + 0.838535*x1*x5 - 0.590525*x5*(x1)^2 + 0.76113*(x5)^2 -
        0.789465*x1*(x5)^2 - 0.273647*(x5)^3 - 0.512764*x6 + 0.455363*x1*x6 - 0.38428*x6*(x1)^2 + 
        0.731809*x5*x6 - 1.00484*x1*x5*x6 - 0.172341*x6*(x5)^2 + 0.219746*(x6)^2 - 0.367679*x1*(x6)^2 + 0.131251*(x6)^3
    
    z4 <- -0.360294 + 0.76878*z3 + 0.0770122*(z3)^3 - 0.193142*x2 - 0.121583*z3*x2 + 0.0889415*x2*(z3)^2 +
        0.284168*(x2)^2 - 0.0674767*(x2)^3 - 0.202897*x3 - 0.341951*z3*x3 - 0.270616*x2*x3 + 0.0880845*x3*(x2)^2 +
        0.24982*(x3)^2 + 0.102658*x2*(x3)^2 - 0.0801841*(x3)^3
    
    n_vg <- 10^(0.140543 + 0.0797516*z4)
    
   return(n_vg) 
}

# van Genuchten parameter
WC0_Tomasella1 <- function(S, C, SBDM) {
    Si <- 100-C-S
    x1 <- -1.06790 + 0.0536107*CS  #CS Coarse sand
    x3 <- -1.05976 + 0.0650437*Si
    x6 <- -6.03516 + 4.81197*SMBD
    
    z5 <- 0.164417 + 0.126139*(x1)^2 + 0.281797*x3 + 0.484823*x1*x3 - 0.293866*(x3)^2 - 0.354924*x1*(x3)^2 -
        0.705803*x6 - 0.189153*x3*x6 - 0.267997*x1*x3*x6 - 0.023954*x6*(x3)^2 - 0.0918816*x1*(x6)^2 + 0.0323997*(x6)^3
    
    WC_0 <- 0.515224 + 0.100899*z5
    
    return(WC_0)
}

# Residual water content 
WCR_Tomasella1 <- function(S, C, GWCFC){ 
    Si <- 100-C-S
    x3 <- -1.05976 + 0.0650437*Si
    x5 <- -2.21391 + 8.9226800*GWCFC
    
    z6 <- 0.12867 - 0.492412*x3 + 0.787425*x5 - 0.235254*x3*x5
    
    WC_r <- 0.161487 + 0.101111*z6
    
    return(WC_r)
}

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
WC10_Tomasella1 <- function(S, C, SBDM, GWCFC) {
    Si <- 100-C-S
    x14 <- -1.05501 + 0.0650857*Si
    x15 <- -2.07588 + 0.0423954*C
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
    
    x15 <- -2.07588 + 0.0423954*C
    x16 <- -6.03402 + 4.80572*SBDM
    x17 <- -2.18409 + 8.84963*GWCFC
    z13 <- 0.235084 + 0.33033*x15 - 0.191838*(x15^2) + 0.0543679*(x15^3) + 0.977685*x17 + 0.304174*x15*x17 - 0.218857*(x17^2) - 0.164373*x15*(x17^2) + 0.0415057*(x17^3) + 0.373361*x16 + 0.0811861*x17*x16 - 0.0768087*x15*x17*x16
    
    WCWP_T1 <- 0.214008 + 0.0862945*z13
}





##############################################################################
######
#####           SOIL SATURATED HYDRAULIC CONDUCTIVITY
###### good paper: http://cdn.intechopen.com/pdfs/23448/InTech-Estimating_hydraulic_conductivity_using_pedotransfer_functions.pdf
#####################################



##### SOIL FUCTIONS for SAXTON & RAWLS (SR): SOIL WATER CHARACTERISTICS ESTIMATES


# Saturated conductivity (matric soil), mm/h
SSKS_Saxton <- function(S,C,OM, SBDM, WCFC=NA, WCWP=NA, WCST=NA){
    if(any(is.na(c(WCFC, WCWP, WCST)))==T){
        message("Water Content PTF are require")
        WCFC <- WCFC_Saxton(S,C,OM)
        WCWP <- WCWP_Saxton(S,C,OM)
        #    WCST <- WCST_cal(S,C,OM)
        WCST <- 1-(SBDM/2.65)
    }else{}
    B <- (log(1500) - log(33))/(log(WCFC) - log(WCWP))
    alp <- 1/B
    
    SSKS <- 1930*(WCST - WCFC)^(3-alp)
    
    return(SSKS*2.4)
    
}

#SSKS_Saxton(S,C,OM, SBDM), WCFC, WCWP, WCST)

#SSKS_Rawls(S,C,OM)
#SST <- SSKS_Rawls(S,C,OM)
#curve(exp(12.012 - (0.0755*S*100) + (-3.8950 + (0.03671*S*100) - (0.1103*C*100) + 8.7546*(10^-4)*(100*C^2)*(1/(x)))),
#        from=0, to=SST, 
#        xlab='Moisture (m3/m3)',
#        ylab='SSK (cm/d)',
#        main='K. E. Saxton, W.J. Rawls, J.S. Romberger, and R. I. Papendick',
#        col='mediumblue')
#    abline(v=SST, col="red")
    

#########################################
##### Accuracy of sample dimension-dependent pedotransfer functions in estimation of soil saturated hydraulic conductivity
##### Behzad Ghanbarian, Vahid Taslimitehrani, Yakov A.Pachepsky
##### https://doi.org/10.1016/j.catena.2016.10.015 
##### XXXX_GTP


##  Constructing a saturated hydraulic conductivity map of Spain using
##  pedotransfer functions and spatial prediction
##  M. Ferrer Julia` et al. / Geoderma 123 (2004) 257–277
##  doi:10.1016/j.geoderma.2004.02.011

#Estimating Hydraulic Conductivity
#Using Pedotransfer Functions
#Ali Rasoulzadeh
#Water Engineering Dept., College of Agriculture
#University of Mohaghegh Ardabili, Ardabil
#Iran

# Wösten et al. (1999) 
SSKS_Wosten99 <- function(S,C,OM,SBDM, tops=0) {
    Si=100-C-S
    x <- 7.755 + 0.0352*Si + 0.93*tops - 0.967*(SBDM^2)- 0.000484*(C^2) - 0.000322*(Si^2) + 
        0.001/Si - 0.0748/OM - 0.643*log(Si) - 0.01398*SBDM*OM - 0.1673*SBDM*OM + 
        0.02986*tops*C - 0.03305*tops*Si
    
    SSKS_W <- 1.15741*(10^-7)*exp(x)*8640000
    return(SSKS_W)
}

# Vereecken et al. (1990)
SSKS_Vereecken <- function(S, C, OM, SBDM) {
    x <- 20.62- 0.96*log(C)- 0.66*log(S) - 0.46*log(OM) - 0.00843*SBDM
   SSKS_Ve <- 1.1574*(10^-7)*exp(x)
    
    return(SSKS_Ve)
    
}

#Ferrer-Julia et al. (2004)
SSKS_Ferrer <- function(S) {
    SSKS_Fer <-  8640000* 2.556*(10^-7)*exp(0.0491*S)
    
    return(SSKS_Fer)
}

# Brakensiek et al. (1984)
SSKS_Brakensiek <- function(S,C,OM, SBDM, SPOR=NA) {
    
    if(is.na(SPOR)){
        message("Porosity was estimated using 2.65g/cm3 as particle density")
        SPOR <- (1-(SBDM/2.65))
    } else {SPOR=SPOR}
    SSKS_Br <- 24*exp(19.52348*SPOR - 8.96847 - 0.028212*C + 0.00018107*(S^2) - 0.0094125*(C^2) - 8.395215*(SPOR^2) + 0.077718*(SPOR*S) - 0.00298*(SPOR^2)*(S^2) -0.019492*(SPOR^2)*(C^2) + 0.0000173*(C*S^2) + 0.02733*(SPOR*C^2) + 0.001434*(SPOR*S^2)- 0.0000035*(S*C^2))
    return(SSKS_Br)
}

# Campbell and Shiozawa (1994)  #Error Si by S
SSKS_Campbell <- function(S,C){
     SSKS_Ca <- 129.6*exp(-0.07*S - 0.167*C)
     return(SSKS_Ca)
 }
     
# Cosby et al. (1984) 
SSKS_Cosby <- function(S,C){
    SSKS_Co <- 60.96*10^((-0.6+0.01268*S) - (0.0064*C))
    return(SSKS_Co)
}

#Jabro (1992)
SSKS_Jabro <- function(S, C, SBDM){
    Si <- 100-C-S
    SSKS_Ja <- 24*(9.56 - 0.81*log(Si/100) - 1.09*log(C/100) - 4.64*SBDM)
    return(SSKS_Ja)
    }

# Puckett et al. (1985)
SSKS_Puckett <- function(C){
    SSKS_Pu <- 376.7*exp(-0.1975*C)
    return(SSKS_Pu)
}

# Dane and Puckett (1994)
SSKS_Dane_Puck <- function(C){
    SSKS_DP <- 729.22*exp(-0.144*C)
    return(SSKS_DP)
}
#
# Saxton et al. (1986)
#24*exp(12.012 - (0.0755*S2))  ## No useful 

# RAWLS (1986)
SSKS_Rawls <- function(S,C){
    SST_Rawls <- 0.332 - (0.0007251*S) + 0.1276*log10(C)
    SSKS_R <- exp(12.012 - (0.0755*S) + (-3.8950 + (0.03671*S) - (0.1103*C) + 8.7546*(10^-4)*(C^2)*(1/(SST_Rawls))))
    
    return(SSKS_R)
}

#############################################################################
#   Using the WISE database to parameterize soil
#   inputs for crop simulation models
#  
#  Arjan J. Gijsman a,b, Philip K. Thornton c,d, Gerrit Hoogenboom e,∗
#  doi:10.1016/j.compag.2007.01.001


####################### https://www.researchgate.net/publication/269802021_Estimating_Saturated_Hydraulic_Conductivity_from_Soil_Porosity
### Method developed by Suleiman and Ritchie (2001), 

SSKS_Sul_Rit <- function(S, C, OM, SBDM, SPOR=NA, WCFC=NA){
#    stopifnot(SPOR)
    if(is.na(WCFC)){
        message("Saxton-PTF for WCFC was used")
        WCFC <- WCFC_Saxton(S,C,OM)*100
    } else {WCFC=WCFC*100}
    
    if(is.na(SPOR)){
        message("Porosity was estimated using 2.65g/cm3 as particle density")
        SPOR <- (1-(SBDM/2.65))*100
    } else {SPOR=SPOR*100}
    
       SSKS <- 75*24*((SPOR-WCFC)/(WCFC)^2)
    
    return(SSKS)
    
}
#SSKS_Sul_Rit(S2,C2,OM, SBDM)


    
                  







#####################################
##### SOIL FUCTIONS for SAXTON & RAWLS: SOIL WATER CHARACTERISTICS ESTIMATES
#####
##### by JRE- https://github.com/jrodriguez88 
###############################################################################

# METADATA FOR ORYZA_model
#
C <- 0.20
S <- 0.40
OM <- 0.025


WCWP_cal <- function(S, C, OM) {
    WCWP_i <- -0.024*S + 0.487*C + 0.006*OM +
               0.005*(S*OM) - 0.013*(C*OM) +
               0.068 *(S*C) + 0.031
    
    WCWP <- WCWP_i + (0.14*WCWP_i - 0.02)
   
    return(WCWP)             
}
WCWP_cal(S, C,OM)


WCFC_cal <- function(S,C,OM) {
    WCFC_i <- -0.251*S + 0.195*C + 0.011*OM +
               0.006*(S*OM) - 0.027*(C*OM) +
               0.452 *(S*C) + 0.299
    
        WCFC <- WCFC_i + (1.283*(WCFC_i^2) - (0.374*WCFC_i) - 0.015)
    
    return(WCFC)
    
}
WCFC_cal(S, C,OM)




WCST_cal <- function(S,C,OM) {
    WCST_i <- 0.278*S + 0.034*C + 0.022*OM -
        0.006*(S*OM) - 0.027*(C*OM) +
        0.452 *(S*C) + 0.299
    
    WCST <- WCST_i + (1.283*(WCST_i^2) - (0.374*WCST_i) - 0.015)
    
    return(WCST)
    
}

WCST_cal(S,C,OM)

















#########################################################
####      Make ORYZA Experimental File (*.EXP)       ####
####     By https://github.com/jrodriguez88          ####
####      ORYZA Rice Crop Model described in:        #### 
####   http://books.irri.org/9712201716_content.pdf  ####
#########################################################

#######################################################################################
#####                                                                             #####
##### GO to ---> Session ---> Set Working Directory ---> To Source File Location  #####
#####                                                                             #####
#####                      =====> SOURCE SCRIPT                                   #####
#####                                                                             #####
#######################################################################################


################################
#### Load Requeriments      ####
################################

if(require(lubridate)==FALSE){install.packages("lubridate")}
if(require(readxl)==FALSE){install.packages("readxl")}
if(require(RCurl)==FALSE){install.packages("RCurl")}
if(require(tidyverse)==FALSE){install.packages("tidyverse")}
if(require(stringr)==FALSE){install.packages("stringr")}
if(require(magrittr)==FALSE){install.packages("magrittr")}
if(require(data.table)==FALSE){install.packages("data.table")}
if(require(plyr)==FALSE){install.packages("plyr")}


#i<- 1
#cul_name <- "F2000"

#lapply(LAI, class)
#lapply(PHEN_df, class)
#lapply(DVR, class)
#c(lsf.str())
################################################################
####   Read Experimental Data Observations INPUNT_data    #####
################################################################

read_INPUT_data <- function(filename) {
    sheets <- readxl::excel_sheets(filename)
    x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
    names(x) <- sheets
    x
}

#names(INPUT_data)

#PHEN_obs <- read.table("clipboard", header = T) %>%
#    mutate(IDAT = as.Date(IDAT, format("%m/%d/%Y")), 
#           PDAT = as.Date(PDAT, format("%m/%d/%Y")), 
#           EDAT = as.Date(EDAT, format("%m/%d/%Y")),
#           MDAT = as.Date(MDAT, format("%m/%d/%Y")),
#           HDAT = as.Date(HDAT, format("%m/%d/%Y"))) %>%
#    dplyr::select(everything())
#str(PHEN_obs)

#AGRO_man <- read.table("clipboard", header = T)

#FERT_obs <- read.table("clipboard", header = T)

##LOC_ID <- exp_df$loc_id[i]
##i <- 1

########################################
### 0. Head_exp
########################################
head_exp_oryza <- function(){
sink(file = names[[i]], append = T)
    cat('*--------------------------------------------------------------------*',sep = '\n')  
    cat('* EXPERIMENTAL DATA FILE                                             *',sep = '\n') 
    cat('*                                                                    *',sep = '\n') 
    cat(paste0('* File name        : ', INPUT_data$AGRO_man$LOC_ID[i], '_', INPUT_data$AGRO_man$CULTIVAR[i], '_', INPUT_data$AGRO_man$PROJECT[i],'_', INPUT_data$AGRO_man$TR_N[i], ".EXP", '                     *'), sep = '\n') 
    cat(paste0('* Crop             : ', INPUT_data$AGRO_man$CULTIVAR[i], '                                       *') ,sep = '\n') 
    cat(paste0('* Year/Season      : ', year(INPUT_data$AGRO_man$PDAT[i]), '                                            *') ,sep = '\n') 
    cat(paste0('* Additional info  : ', 'Create with https://github.com/jrodriguez88', '     *') ,sep = '\n') 
    cat('*--------------------------------------------------------------------*',sep = '\n') 
    cat('\n')

sink()
}
#head_exp_oryza()
########################################
### 1. Selection of modes of running ###
########################################
runmodes_oryza <- function(){
    sink(file = names[[i]], append = T)
    cat('*--------------------------------------------------------------------*',sep = '\n') 
    cat('* 1. Selection of modes of running                                   *',sep = '\n') 
    cat('*--------------------------------------------------------------------*',sep = '\n') 
    cat(paste0("RUNMODE = ","'EXPERIMENT'"),sep = '\n')
    cat(paste0("PRODENV = ", "'WATER BALANCE'"),sep = '\n')
    cat(paste0("WATBAL = ", "'PADDY'"),sep = '\n')
    cat(paste0("NITROENV = ", "'NITROGEN BALANCE'"),sep = '\n')
    cat(paste0("ETMOD = ", "'PRIESTLY TAYLOR'"),sep = '\n')
    cat('\n')
    sink()
}
#runmodes_oryza()

#####################################
### 2. Timer data for simulation  ###
#####################################
## puede requerir If cuando STTIME < 0, 
#then IYEAR=IYEAR-1 ((as.POSIXlt(PHEN_obs$PDAT)$year + 1899) 
#and STTIME=365-(yday(PHEN_obs$PDAT)[i])
#i <- 8
timer_oryza <- function(){
    if (yday(INPUT_data$PHEN_obs$PDAT)[i]-90<0){
    a=year(INPUT_data$PHEN_obs$PDAT)[i]-1
    b=365+(yday(INPUT_data$PHEN_obs$PDAT)[i]-90)} else {
       a= year(INPUT_data$PHEN_obs$PDAT)[i]
       b= yday(INPUT_data$PHEN_obs$PDAT)[i]-90
    }
    
    sink(file = names[[i]], append = T)
#for (i in 1:1){
    cat('*--------------------------------------------------------------------*',sep = '\n') 
    cat('* 2. Timer data for simulation                                       *',sep = '\n') 
    cat('*--------------------------------------------------------------------*',sep = '\n')
    cat(paste0("IYEAR = ", a),sep = '\n')
    cat(paste0("STTIME = ", b, '.'),sep = '\n')
    cat(paste0("FINTIM = ", "1000."),sep = '\n')
    cat(paste0("DELT = ", "1."),sep = '\n')
    cat('\n')
#}
sink()
}
#timer_oryza()

############################################################
### 3. Weather station and climatic data for simulation  ###
############################################################

wtrdir_oryza <- function(){
    sink(file = names[[i]], append = T)
#    for (i in 1:1){
        cat('*--------------------------------------------------------------------*',sep = '\n') 
        cat('* 3. Weather station and climatic data for simulation                *',sep = '\n') 
        cat('*--------------------------------------------------------------------*',sep = '\n')
        cat( paste0(  "WTRDIR = "   ,"' WTH","\\",  "'"),sep = '\n')
        cat( paste0(  "CNTR = "     , "'", INPUT_data$PHEN_obs$LOC_ID[i],"'")  ,sep = '\n')              
        cat( paste0(  "ISTN = "     , 1 ) ,sep = '\n')
        cat( paste0(  "MULTIY = "   , "'NO'")              ,sep = '\n')
        cat( paste0(  "ANGA = "     , "0.29"),sep = '\n')
        cat( paste0(  "ANGB = "     , "0.45"),sep = '\n')
        cat( paste0(  "TMINCTB = "  ),sep = '\n')
        cat(paste0("0., ","0."),sep = '\n')
        cat(paste0("366., ","0."),sep = '\n')
        cat('\n')
        cat(paste0(  "TMAXCTB = "  ),sep = '\n')
        cat(paste0("0., ","0.,"),sep = '\n')
        cat(paste0("366., ","0."),sep = '\n')
        cat('\n')
        cat(paste0("FAOF = ", "1."),sep = '\n')
        cat( paste0("TMPSB = ", "0."),sep = '\n') 
        cat('\n')
#    }
    sink()
}
#wtrdir_oryza()

##############################
### 4. Establishment data  ###
##############################

estab_oryza <- function(){
    sink(file = names[[i]], append = T)
#    for (i in 1:1){
        cat('*--------------------------------------------------------------------*',sep = '\n') 
        cat('* 4. Establishment data                                              *',sep = '\n') 
        cat('*--------------------------------------------------------------------*',sep = '\n')    
        cat(paste0("ESTAB = ", "'",INPUT_data$AGRO_man$ESTAB[i], "'"),sep = '\n')
        cat(paste0("EMD    = ", yday(INPUT_data$PHEN_obs$EDAT)[i]),sep = '\n')
        cat(paste0("EMYR   = ", year(INPUT_data$PHEN_obs$EDAT)[i]),sep = '\n')
        cat(paste0("SBDUR  = ", if (INPUT_data$AGRO_man$ESTAB[i]=="TRANSPLANT"){INPUT_data$AGRO_man$SBDUR[i]}else{0}), sep = '\n')
        cat('\n')
#    }
    sink()
}
#estab_oryza() 

###################################
### 5. Management parameters  ###
###################################

management_oryza <- function(){
    sink(file = names[[i]], append = T)
#    for (i in 1:1){
        cat('*--------------------------------------------------------------------*',sep = '\n') 
        cat('* 5. Management parameters                                           *',sep = '\n') 
        cat('*--------------------------------------------------------------------*',sep = '\n')    
        #cat("NPLH   = ","2.0"),sep = '\n')
        #cat("NH     = ","33.0"),sep = '\n')
        #cat("NPLSB  = ","1000."),sep = '\n')
        cat(paste0("NPLDS  = ", INPUT_data$AGRO_man$NPLDS[i],"."), sep = '\n') 
        cat(paste0("LAPE   = ", "0.0001"), sep = '\n')
        cat(paste0("DVSI   = ", "0.0"), sep = '\n')
        cat(paste0("WLVGI  = ", "0.0"), sep = '\n')
        cat(paste0("WSTI   = ", "0.0"), sep = '\n')
        cat(paste0("WRTI   = ", "0.0"), sep = '\n')
        cat(paste0("WSOI   = ", "0.0"), sep = '\n')
        cat(paste0("ZRTI   = ", "0.0001"),sep = '\n')
        cat(paste0("ZRTTR  = ", "0.05"), sep = '\n')
        cat('\n')
#    }
    sink()
}
#management_oryza() 

################################
### 6. Irrigation parameters ###
################################

irrig_oryza <- function(){
    dvmx <- if (INPUT_data$AGRO_man$CROP_SYS[i]=="RAINFED"){"0.0"} else{1.8}
    sink(file = names[[i]], append = T)
#    for (i in 1:1){
        cat('*--------------------------------------------------------------------*',sep = '\n') 
        cat('* 6. Irrigation parameters                                           *',sep = '\n') 
        cat('*--------------------------------------------------------------------*',sep = '\n')    
        cat('* ',sep = '\n')
        cat(paste0("DVSIMAX = ",dvmx), sep = '\n')
        ##ICOMBA switch critical (1:4)-->c(yday,DVS,DVS-yday, DAE)
        cat(paste0("ICOMBA = ", 1), sep = '\n')
        cat("IRMTAB = ", sep = '\n')
        cat(paste0("0., ",1,".0,"),sep = '\n')
        cat(paste0("366., ",1, ".0"),sep = '\n')
        cat(paste0("AUTODEPT = ", "-10.0"),sep = '\n')

##SWITIR Irrigation Settongs (1:6)---Automatic use SWITIR=2

        cat(paste0("SWITIR = ", (if (INPUT_data$AGRO_man$CROP_SYS[i]=="RAINFED"){0} else{2})),sep = '\n')
        cat(paste0("RIRRIT ="),sep = '\n')
        cat(paste0("1., ",0,".0,"),sep = '\n')
        cat(paste0("365., ",0, ".0"),sep = '\n')
        cat('\n')
        cat(paste0("IRRI2   = ",50,"."),sep = '\n')
        cat(paste0("WL0MIN  = ",1,"."),sep = '\n')
        cat(paste0("IRRI3   = ",50,"."),sep = '\n')
        cat(paste0("KPAMIN  = ",70,"."),sep = '\n')
        cat(paste0("SLMIN3  = ",3),sep = '\n')
        cat(paste0("IRRI4   = ",50,"."),sep = '\n')
        cat(paste0("WCMIN   = ",0.30),sep = '\n')
        cat(paste0("SLMIN4  = ",3),sep = '\n')
        cat(paste0("IRRI5   = ",50,"."),sep = '\n')
        cat(paste0("WL0DAY  = ",5),sep = '\n')
        cat(paste0("IRRI6   = ",50, "."),sep = '\n')
        cat(paste0("SLMIN6  = ",3),sep = '\n')
        cat('\n')
        cat(paste0("ISTAGET ="),sep = '\n')
        cat(paste0("0.00, 0.20, 5.,"),sep = '\n')
        cat(paste0("1.70, 1.80, 5." ),sep = '\n')
        cat('\n')
#    }
    sink()
}
#irrig_oryza()

###############################
### 7. Nitrogen parameters  ###
###############################

nitrogen_oryza <- function(){
    sink(file = names[[i]], append = T)
#    for (i in 1:1){
        cat('*--------------------------------------------------------------------*',sep = '\n') 
        cat('* 7. Nitrogen parameters                                             *',sep = '\n') 
        cat('*--------------------------------------------------------------------*',sep = '\n')
        cat('*  ',sep = '\n')
        cat(paste0("NUTRIENT = ","'", "GENERAL SOM", "'"),sep = '\n')
        #"NUTRIENT = ","'", "FIXED SUPPLY", "'"
        cat(paste0("RECNIT ="),sep = '\n')
        cat(paste0("0.0, 0.30,"),sep = '\n')
        cat(paste0("0.2, 0.35,"),sep = '\n')
        cat(paste0("0.4, 0.50,"),sep = '\n')
        cat(paste0("0.8, 0.75,"),sep = '\n')
        cat(paste0("1.0, 0.75,"),sep = '\n')
        cat(paste0("2.5, 0.75"),sep = '\n')
        cat('\n')
        cat(paste0("SOILSP = ", 0.8),sep = '\n')
        cat('\n')

#    }
    sink()
}
#nitrogen_oryza()
    
Fert_tb <- function(){
        Fert <- split(INPUT_data$FERT_obs, INPUT_data$FERT_obs$ID)
        nit <-cbind(Fert[[i]]$DDE, Fert[[i]]$N)
        
       a <- sprintf("%.1f", nit[,1]) 
       b <- sprintf("%.1f", nit[,2])
       c <- matrix(cbind(a,", ",b,","), ncol = 4)
       
       sink(file = names[[i]], append = T)
       cat(paste0("FERTIL ="),sep = '\n')
       cat(paste0("0.0, 0.0,"),sep = '\n')
       write.table(c, col.names = F, sep="",row.names = F, quote = F)
       cat(paste0("366.0, 0.0"),sep = '\n')
       cat('\n')
       sink()
       }  # Need review. i
#Fert_tb()

###############################################
### 8. Measured data for model calibration  ###
###############################################

measure_phen_oryza <- function(){
    
    sink(file = names[[i]], append = T)
#    for (i in 1:1){
    cat('*--------------------------------------------------------------------*',sep = '\n')
    cat('* 8. Measured data for model calibration and comparison              *',sep = '\n')
    cat('*    And option to force measured LAI during simulation              *',sep = '\n')
    cat('*    (instead of using simulated values)                             *',sep = '\n')
    cat('*--------------------------------------------------------------------*',sep = '\n')
    cat('* Observed phenology: only required if program DRATES is run!!',sep = '\n')
    cat('\n')
    cat(paste0("IDOYTR = ", if (INPUT_data$AGRO_man$ESTAB[i]=="DIRECT-SEED"){0}else{yday(INPUT_data$AGRO_man$TRDAT[i])}),  sep = '\n')
    cat(paste0("IYRTR  = ", if (INPUT_data$AGRO_man$ESTAB[i]=="DIRECT-SEED"){0}else{year(INPUT_data$AGRO_man$TRDAT[i])}), sep = '\n')
    cat(paste0("IDOYPI = ", yday(INPUT_data$PHEN_obs$IDAT)[i]), sep = '\n')
    cat(paste0("IYRPI  = ", year(INPUT_data$PHEN_obs$IDAT)[i]), sep = '\n')
    cat(paste0("IDOYFL = ", yday(INPUT_data$PHEN_obs$FDAT)[i]), sep = '\n')
    cat(paste0("IYRFL  = ", year(INPUT_data$PHEN_obs$FDAT)[i]), sep = '\n')
    cat(paste0("IDOYM  = ", yday(INPUT_data$PHEN_obs$MDAT)[i]-7), sep = '\n')
    cat(paste0("IYRM   = ", year(INPUT_data$PHEN_obs$MDAT)[i]), sep = '\n')
    cat('\n')
#    }
    sink()
}
#measure_phen_oryza()


##*!* Leaf Area Index (m2 leaf / m2 ground):
LAI_tb <- function(){
    Var <- split(INPUT_data$PLANT_gro, INPUT_data$PLANT_gro$ID)
    var <- cbind(year(Var[[i]]$SAMPLING_DATE),yday(Var[[i]]$SAMPLING_DATE), Var[[i]]$LAI_OBS)%>%
        na.omit()
    var2 <- var[-length(var[,1]),]
    
    if (length(var2)<1){
        
    } else {
        a <- sprintf("%.1f", var2[,1]) 
        b <- sprintf("%.1f", var2[,2])
        c <- sprintf("%.1f", var2[,3])
        d <- matrix(cbind(a,", ",b,", ",c,","), ncol = 6)
        
        sink(file = names[[i]], append = T)
        cat('*!* Leaf Area Index (m2 leaf / m2 ground):',sep = '\n')
        cat(paste0("LAI_OBS ="),sep = '\n')
        write.table(d, col.names = F, sep="",row.names = F, quote = F)
        cat(paste0(sprintf("%.1f", var[length(var[,1]),1]),", ", sprintf("%.1f", var[length(var[,1]),2]), ", ", sprintf("%.1f", var[length(var[,1]),3])),sep = '\n')
        cat('\n')
        cat(paste0("LAI_FRC = ", 0),sep = '\n') # 0:No forcing ; 2: Forcing
        cat('\n')
        sink()   
    }

}       #*!* Leaf Area Index (m2 leaf / m2 ground):
#LAI_tb()
#Var=list()
WLVG_tb <- function(){
    Var <- split(INPUT_data$PLANT_gro, INPUT_data$PLANT_gro$ID)
    if (any(is.na(Var[[i]]$WLVG_OBS)) || any(is.na(Var[[i]]$WST_OBS)) || any(is.na(Var[[i]]$WLVD_OBS)) || any(is.na(Var[[i]]$WSO_OBS)) || any(is.na(Var[[i]]$WAGT_OBS))){
        Var[[i]] <-na.omit(Var[[i]])
        var <- cbind(year(Var[[i]]$SAMPLING_DATE),yday(Var[[i]]$SAMPLING_DATE), Var[[i]]$WLVG_OBS)%>%
            na.omit()
    } else {
        var <- cbind(year(Var[[i]]$SAMPLING_DATE),yday(Var[[i]]$SAMPLING_DATE), Var[[i]]$WLVG_OBS)
    }
    var2 <- var[-length(var[,1]),]     
    
    a <- sprintf("%.1f", var2[,1])
    b <- sprintf("%.1f", var2[,2])
    c <- sprintf("%.1f", var2[,3])
    d <- matrix(cbind(a,", ",b,", ",c,","), ncol = 6)
    
    sink(file = names[[i]], append = T)
    cat('*!* Green leaf dry wt (kg/ha)',sep = '\n')
    cat(paste0("WLVG_OBS ="),sep = '\n')
    write.table(d, col.names = F, sep="",row.names = F, quote = F)
    cat(paste0(sprintf("%.1f", var[length(var[,1]),1]),", ", sprintf("%.1f", var[length(var[,1]),2]), ", ", sprintf("%.1f", var[length(var[,1]),3])),sep = '\n')
    cat('\n')
    sink()
}      #*!* Green leaf dry wt (kg/ha)
#WLVG_tb()

WLVD_tb <- function(){
    Var <- split(INPUT_data$PLANT_gro, INPUT_data$PLANT_gro$ID)
    if (any(is.na(Var[[i]]$WLVG_OBS)) || any(is.na(Var[[i]]$WST_OBS)) || any(is.na(Var[[i]]$WLVD_OBS)) || any(is.na(Var[[i]]$WSO_OBS)) || any(is.na(Var[[i]]$WAGT_OBS))){
        Var[[i]] <-na.omit(Var[[i]])
        var <- cbind(year(Var[[i]]$SAMPLING_DATE),yday(Var[[i]]$SAMPLING_DATE), Var[[i]]$WLVD_OBS)%>%
            na.omit()
    } else {
        var <- cbind(year(Var[[i]]$SAMPLING_DATE),yday(Var[[i]]$SAMPLING_DATE), Var[[i]]$WLVD_OBS)
    }
    var2 <- var[-length(var[,1]),]
    
    a <- sprintf("%.1f", var2[,1]) 
    b <- sprintf("%.1f", var2[,2])
    c <- sprintf("%.1f", var2[,3])
    d <- matrix(cbind(a,", ",b,", ",c,","), ncol = 6)
    
    sink(file = names[[i]], append = T)
    cat('*!* Dead leaf dry wt (kg/ha)',sep = '\n')
    cat(paste0("WLVD_OBS ="),sep = '\n')
    write.table(d, col.names = F, sep="",row.names = F, quote = F)
    cat(paste0(sprintf("%.1f", var[length(var[,1]),1]),", ", sprintf("%.1f", var[length(var[,1]),2]), ", ", sprintf("%.1f", var[length(var[,1]),3])),sep = '\n')
    cat('\n')
    sink()
}      #*!* Dead leaf dry wt (kg/ha)
#WLVD_tb()

WST_tb <- function(){
    Var <- split(INPUT_data$PLANT_gro, INPUT_data$PLANT_gro$ID)
    if (any(is.na(Var[[i]]$WLVG_OBS)) || any(is.na(Var[[i]]$WST_OBS)) || any(is.na(Var[[i]]$WLVD_OBS)) || any(is.na(Var[[i]]$WSO_OBS)) || any(is.na(Var[[i]]$WAGT_OBS))){
        Var[[i]] <-na.omit(Var[[i]])
        var <- cbind(year(Var[[i]]$SAMPLING_DATE),yday(Var[[i]]$SAMPLING_DATE), Var[[i]]$WST_OBS)%>%
            na.omit()
    } else {
        var <- cbind(year(Var[[i]]$SAMPLING_DATE),yday(Var[[i]]$SAMPLING_DATE), Var[[i]]$WST_OBS)
    }
    var2 <- var[-length(var[,1]),]
    
    a <- sprintf("%.1f", var2[,1]) 
    b <- sprintf("%.1f", var2[,2])
    c <- sprintf("%.1f", var2[,3])
    d <- matrix(cbind(a,", ",b,", ",c,","), ncol = 6)
    
    sink(file = names[[i]], append = T)
    cat('*!* Stem dry wt (kg/ha)',sep = '\n')
    cat(paste0("WST_OBS ="),sep = '\n')
    write.table(d, col.names = F, sep="",row.names = F, quote = F)
    cat(paste0(sprintf("%.1f", var[length(var[,1]),1]),", ", sprintf("%.1f", var[length(var[,1]),2]), ", ", sprintf("%.1f", var[length(var[,1]),3])),sep = '\n')
    cat('\n')
    sink()
}       #*!* Stem dry wt (kg/ha)
#WST_tb()

WSO_tb <- function(){
    Var <- split(INPUT_data$PLANT_gro, INPUT_data$PLANT_gro$ID)
    if (any(is.na(Var[[i]]$WLVG_OBS)) || any(is.na(Var[[i]]$WST_OBS)) || any(is.na(Var[[i]]$WLVD_OBS)) || any(is.na(Var[[i]]$WSO_OBS)) || any(is.na(Var[[i]]$WAGT_OBS))){
        Var[[i]] <-na.omit(Var[[i]])
        var <- cbind(year(Var[[i]]$SAMPLING_DATE),yday(Var[[i]]$SAMPLING_DATE), Var[[i]]$WSO_OBS)%>%
            na.omit()
    } else {
        var <- cbind(year(Var[[i]]$SAMPLING_DATE),yday(Var[[i]]$SAMPLING_DATE), Var[[i]]$WSO_OBS)
    }
    var2 <- var[-length(var[,1]),]
    
    a <- sprintf("%.1f", var2[,1]) 
    b <- sprintf("%.1f", var2[,2])
    c <- sprintf("%.1f", var2[,3])
    d <- matrix(cbind(a,", ",b,", ",c,","), ncol = 6)
    
    sink(file = names[[i]], append = T)
    cat('*!* Panicle dry wt (kg/ha)',sep = '\n')
    cat(paste0("WSO_OBS ="),sep = '\n')
    write.table(d, col.names = F, sep="",row.names = F, quote = F)
    cat(paste0(sprintf("%.1f", var[length(var[,1]),1]),", ", sprintf("%.1f", var[length(var[,1]),2]), ", ", sprintf("%.1f", var[length(var[,1]),3])),sep = '\n')
    cat('\n')
    sink()
}       #*!* Panicle dry wt (kg/ha)
#WSO_tb()

WAGT_tb <- function(){
    Var <- split(INPUT_data$PLANT_gro, INPUT_data$PLANT_gro$ID)
    if (any(is.na(Var[[i]]$WLVG_OBS)) || any(is.na(Var[[i]]$WST_OBS)) || any(is.na(Var[[i]]$WLVD_OBS)) || any(is.na(Var[[i]]$WSO_OBS)) || any(is.na(Var[[i]]$WAGT_OBS))){
        Var[[i]] <-na.omit(Var[[i]])
        var <- cbind(year(Var[[i]]$SAMPLING_DATE),yday(Var[[i]]$SAMPLING_DATE), Var[[i]]$WAGT_OBS)%>%
            na.omit()
        print(paste0("## Exist NA in Growth Tables! ##-->EXP:", i))
    } else {
        var <- cbind(year(Var[[i]]$SAMPLING_DATE),yday(Var[[i]]$SAMPLING_DATE), Var[[i]]$WAGT_OBS)
    }
    var2 <- var[-length(var[,1]),]
    
    a <- sprintf("%.1f", var2[,1]) 
    b <- sprintf("%.1f", var2[,2])
    c <- sprintf("%.1f", var2[,3])
    d <- matrix(cbind(a,", ",b,", ",c,","), ncol = 6)
    
    sink(file = names[[i]], append = T)
    cat('*!* Total dry wt (kg/ha)',sep = '\n')
    cat(paste0("WAGT_OBS ="),sep = '\n')
    write.table(d, col.names = F, sep="",row.names = F, quote = F)
    cat(paste0(sprintf("%.1f", var[length(var[,1]),1]),", ", sprintf("%.1f", var[length(var[,1]),2]), ", ", sprintf("%.1f", var[length(var[,1]),3])),sep = '\n')  #sprintf("%.1f", as.numeric(var[length(var[,1]),2])-7)
    cat('\n')
    sink()
}      #*!* Total dry wt (kg/ha)
#WAGT_tb()

##########################################
####      MAKE EXPERIMENTAL FILE      ####    
##########################################
#INPUT_data <- read_INPUT_data("AIHU_FEDTANA.xlsx")
#
#names<- list()
##Make_ORYZA_Exp <- function(){
#     
#dir.create(paste0(getwd(),"/EXP"))
#        
#    for (i in 1:length(INPUT_data$AGRO_man$ID)){
#         names[[i]] <- paste0(getwd(),"/EXP/",INPUT_data$AGRO_man$LOC_ID[i], '_', INPUT_data$AGRO_man$CULTIVAR[i], '_', INPUT_data$AGRO_man$PROJECT[i],'_', INPUT_data$AGRO_man$TR_N[i], ".exp")
#         head_exp_oryza()
#         runmodes_oryza()
#         timer_oryza()
#         wtrdir_oryza()
#         estab_oryza()
#         management_oryza()
#         irrig_oryza()
#         nitrogen_oryza()
#         Fert_tb()
#         measure_phen_oryza()
#         LAI_tb()
#         WLVG_tb()
#         WLVD_tb()
#         WST_tb()
#         WSO_tb()
#         WAGT_tb()
#         print(paste0("#### DONE! --->",i))
#     }
#     
##}

#Make_ORYZA_Exp()

local<- local
files <- list.files(pattern = paste0(local, "_"))

for (e in 1:length(files)) {
    
    
    INPUT_data <- read_INPUT_data(files[e])
    
    names<- list()
    #Make_ORYZA_Exp <- function(){
    
    dir.create(paste0(getwd(),"/EXP"), showWarnings = FALSE)
    
    for (i in 1:length(INPUT_data$AGRO_man$ID)){
        names[[i]] <- paste0(getwd(),"/EXP/",INPUT_data$AGRO_man$LOC_ID[i], '_', INPUT_data$AGRO_man$CULTIVAR[i], '_', INPUT_data$AGRO_man$PROJECT[i],'_', INPUT_data$AGRO_man$TR_N[i], ".exp")
        head_exp_oryza()
        runmodes_oryza()
        timer_oryza()
        wtrdir_oryza()
        estab_oryza()
        management_oryza()
        irrig_oryza()
        nitrogen_oryza()
        Fert_tb()
        measure_phen_oryza()
        LAI_tb()
        WLVG_tb()
        WLVD_tb()
        WST_tb()
        WSO_tb()
        WAGT_tb()
        print(paste0("#### DONE! ---> ",files[e], "---> ", i))
    }
}








    
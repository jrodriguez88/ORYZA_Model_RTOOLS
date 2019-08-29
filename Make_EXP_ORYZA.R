#########################################################
####      Make ORYZA Experimental File (*.EXP)       ####
####     By https://github.com/jrodriguez88          ####
####      ORYZA Rice Crop Model described in:        #### 
####   http://books.irri.org/9712201716_content.pdf  ####
#########################################################

make_exp_oryza <- function(INPUT_data, out_path) {
    
    exp_file <- INPUT_data$AGRO_man %>% 
        mutate(file = paste(LOC_ID, CULTIVAR, PROJECT, TR_N, sep = "_") %>% 
                   paste0(out_path, .,".exp")) %>% pull(file) #dplyr::select(ID, file)
    
    id <- 1:length(exp_file)
    
    ########################################
    ### 0. Head_exp
    ########################################
    
    head_exp_oryza <- function(INPUT_data, exp_file, i){
        sink(file = exp_file, append = T)
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
    runmodes_oryza <- function(INPUT_data, exp_file, i){
        sink(file = exp_file, append = T)
        cat('*--------------------------------------------------------------------*',sep = '\n') 
        cat('* 1. Selection of modes of running                                   *',sep = '\n') 
        cat('*--------------------------------------------------------------------*',sep = '\n') 
        cat(paste0("RUNMODE = ","'EXPERIMENT'"),sep = '\n')
        cat(paste0("PRODENV = ", "'WATER BALANCE'"),sep = '\n')
        cat(paste0("WATBAL = ", "'PADDY'"), sep = '\n')
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
    timer_oryza <- function(INPUT_data, exp_file, i){
        #    if (yday(INPUT_data$PHEN_obs$PDAT)[i]-3<0){
        #    a=year(INPUT_data$PHEN_obs$PDAT)[i]-1
        #    b=365+(yday(INPUT_data$PHEN_obs$PDAT)[i]-3)} else {
        #       a= year(INPUT_data$PHEN_obs$PDAT)[i]
        #       b= yday(INPUT_data$PHEN_obs$PDAT)[i]-3
        #    }
        a= year(INPUT_data$PHEN_obs$EDAT)[i]
        b= yday(INPUT_data$PHEN_obs$EDAT)[i]-3
        
        
        
        
        sink(file = exp_file, append = T)
        #for (i in 1:1){
        cat('*--------------------------------------------------------------------*',sep = '\n') 
        cat('* 2. Timer data for simulation                                       *',sep = '\n') 
        cat('*--------------------------------------------------------------------*',sep = '\n')
        cat(paste0("IYEAR = ", a),sep = '\n')
        cat(paste0("STTIME = ", ifelse(b<=0, 1, b), '.'),sep = '\n')
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
    
    wtrdir_oryza <- function(INPUT_data, exp_file, i){
        sink(file = exp_file, append = T)
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
    
    estab_oryza <- function(INPUT_data, exp_file, i){
        sink(file = exp_file, append = T)
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
    
    management_oryza <- function(INPUT_data, exp_file, i){
        sink(file = exp_file, append = T)
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
    
    irrig_oryza <- function(INPUT_data, exp_file, i){
        dvmx <- if (INPUT_data$AGRO_man$CROP_SYS[i]=="RAINFED"){"0.0"} else{1.8}
        sink(file = exp_file, append = T)
        #    for (i in 1:1){
        cat('*--------------------------------------------------------------------*',sep = '\n') 
        cat('* 6. Irrigation parameters                                           *',sep = '\n') 
        cat('*--------------------------------------------------------------------*',sep = '\n')    
        cat('* ',sep = '\n')
        cat(paste0("DVSIMAX = ",dvmx), sep = '\n')
        ##ICOMBA switch critical (1:4)-->c(yday,DVS,DVS-yday, DAE)
        cat(paste0("ICOMBA = ", 1), sep = '\n')
        cat("IRMTAB = ", sep = '\n')
        cat(paste0("0., ",if (INPUT_data$AGRO_man$CROP_SYS[i]=="RAINFED"){0} else{2},".0,"),sep = '\n')
        cat(paste0("366., ",if (INPUT_data$AGRO_man$CROP_SYS[i]=="RAINFED"){0} else{2}, ".0"),sep = '\n')
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
    
    nitrogen_oryza <- function(INPUT_data, exp_file, i){
        sink(file = exp_file, append = T)
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
    
    Fert_tb <- function(INPUT_data, exp_file, i){
        Fert <- split(INPUT_data$FERT_obs, INPUT_data$FERT_obs$ID)
        nit <-cbind(Fert[[i]]$DDE, Fert[[i]]$N)
        
        a <- sprintf("%.1f", nit[,1]) 
        b <- sprintf("%.1f", nit[,2])
        c <- matrix(cbind(a,", ",b,","), ncol = 4)
        
        sink(file = exp_file, append = T)
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
    
    measure_phen_oryza <- function(INPUT_data, exp_file, i){
        
        sink(file = exp_file, append = T)
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
    LAI_tb <- function(INPUT_data, exp_file, i){
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
            
            sink(file = exp_file, append = T)
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
    WLVG_tb <- function(INPUT_data, exp_file, i){
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
        
        sink(file = exp_file, append = T)
        cat('*!* Green leaf dry wt (kg/ha)',sep = '\n')
        cat(paste0("WLVG_OBS ="),sep = '\n')
        write.table(d, col.names = F, sep="",row.names = F, quote = F)
        cat(paste0(sprintf("%.1f", var[length(var[,1]),1]),", ", sprintf("%.1f", var[length(var[,1]),2]), ", ", sprintf("%.1f", var[length(var[,1]),3])),sep = '\n')
        cat('\n')
        sink()
    }      #*!* Green leaf dry wt (kg/ha)
    #WLVG_tb()
    
    WLVD_tb <- function(INPUT_data, exp_file, i){
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
        
        sink(file = exp_file, append = T)
        cat('*!* Dead leaf dry wt (kg/ha)',sep = '\n')
        cat(paste0("WLVD_OBS ="),sep = '\n')
        write.table(d, col.names = F, sep="",row.names = F, quote = F)
        cat(paste0(sprintf("%.1f", var[length(var[,1]),1]),", ", sprintf("%.1f", var[length(var[,1]),2]), ", ", sprintf("%.1f", var[length(var[,1]),3])),sep = '\n')
        cat('\n')
        sink()
    }      #*!* Dead leaf dry wt (kg/ha)
    #WLVD_tb()
    
    WST_tb <- function(INPUT_data, exp_file, i){
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
        
        sink(file = exp_file, append = T)
        cat('*!* Stem dry wt (kg/ha)',sep = '\n')
        cat(paste0("WST_OBS ="),sep = '\n')
        write.table(d, col.names = F, sep="",row.names = F, quote = F)
        cat(paste0(sprintf("%.1f", var[length(var[,1]),1]),", ", sprintf("%.1f", var[length(var[,1]),2]), ", ", sprintf("%.1f", var[length(var[,1]),3])),sep = '\n')
        cat('\n')
        sink()
    }       #*!* Stem dry wt (kg/ha)
    #WST_tb()
    
    WSO_tb <- function(INPUT_data, exp_file, i){
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
        
        sink(file = exp_file, append = T)
        cat('*!* Panicle dry wt (kg/ha)',sep = '\n')
        cat(paste0("WSO_OBS ="),sep = '\n')
        write.table(d, col.names = F, sep="",row.names = F, quote = F)
        cat(paste0(sprintf("%.1f", var[length(var[,1]),1]),", ", sprintf("%.1f", var[length(var[,1]),2]), ", ", sprintf("%.1f", var[length(var[,1]),3])),sep = '\n')
        cat('\n')
        sink()
    }       #*!* Panicle dry wt (kg/ha)
    #WSO_tb()
    
    WAGT_tb <- function(INPUT_data, exp_file, i){
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
        
        sink(file = exp_file, append = T)
        cat('*!* Total dry wt (kg/ha)',sep = '\n')
        cat(paste0("WAGT_OBS ="),sep = '\n')
        write.table(d, col.names = F, sep="",row.names = F, quote = F)
        cat(paste0(sprintf("%.1f", var[length(var[,1]),1]),", ", sprintf("%.1f", var[length(var[,1]),2]), ", ", sprintf("%.1f", var[length(var[,1]),3])),sep = '\n')  #sprintf("%.1f", as.numeric(var[length(var[,1]),2])-7)
        cat('\n')
        sink()
    }
    
######    
    
write_exp <- function(INPUT_data, exp_file, i){
        
        head_exp_oryza(INPUT_data, exp_file, i)
        runmodes_oryza(INPUT_data, exp_file, i)
        timer_oryza(INPUT_data, exp_file, i)
        wtrdir_oryza(INPUT_data, exp_file, i)
        estab_oryza(INPUT_data, exp_file, i)
        management_oryza(INPUT_data, exp_file, i)
        irrig_oryza(INPUT_data, exp_file, i)
        nitrogen_oryza(INPUT_data, exp_file, i)
        Fert_tb(INPUT_data, exp_file, i)
        measure_phen_oryza(INPUT_data, exp_file, i)
        LAI_tb(INPUT_data, exp_file, i)
        WLVG_tb(INPUT_data, exp_file, i)
        WLVD_tb(INPUT_data, exp_file, i)
        WST_tb(INPUT_data, exp_file, i)
        WSO_tb(INPUT_data, exp_file, i)
        WAGT_tb(INPUT_data, exp_file, i)
        
    }
    
walk2(exp_file, id, ~write_exp(INPUT_data, .x, .y))

}
    


    
    
    
     
    
    
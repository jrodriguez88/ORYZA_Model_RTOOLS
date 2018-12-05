## Make Soil
#load("SOILDB_raw.RData")
#data -> raw_data 
#
soil_data <- data %>%
    left_join(SKS_Final, by = c("ID", "DEPTH"))%>%
    mutate(DEPTH_range = DEPTH,
           DEPTH = 20,
           SSKS = ks_ptf_resample,
           WCAD = WCR_Tomasella1(SAND, CLAY, GWCFC), 
           SOC = DEPTH*SBDM*100*SCARBON/0.58,
           SON = DEPTH*SBDM*SLON/10,
           SNH4X = DEPTH*SBDM*SNH4/10,
           SNO3X = DEPTH*SBDM*SNO3/10)

Soil_by_loc <- soil_data %>%
    group_by(LOC_ID, DEPTH_range) %>%
    summarize_if(is.numeric, mean)



#soil_data <- split(soil_data, soil_data$ID)
#path <- getwd()

### require(tidyverse)
### ZRTMS Maximum rooting depth in the soil (m), 
### WL0I Initial ponded water depth at start of simulation (mm)"
### WCLI can take 3 values: Field Capacity ('FC'), 50% of Soil Saturation ('ST50'), Fraction of water content ('0.0'- '1.0') 
### RIWCLI Re-initialize switch RIWCLI is YES or NO
### SATAV Soil annual average temperature of the first layers

Make_SOIL_ORYZA <- function(data, path, ZRTMS = 0.50, WL0I = 0, WCLI='FC' , RIWCLI = 'NO', SATAV=25){
    stopifnot(require(tidyverse)==T)
    inpp <- function(x, div=1){paste0(round(data[,x]/div,2), collapse = ", ")}
    dirfol <- paste0(path,'/', 'SOIL')
    dir.create((paste0(path,"/SOIL")), showWarnings = FALSE)

sink(file=paste0(dirfol,'/', unique(data["ID"]), ".sol"), append = F)

########################################
### 0. Head_sol
########################################
cat("**********************************************************************",sep = '\n')
cat("* Template soil data file for PADDY soil water balance model.        *",sep = '\n')
cat("**********************************************************************",sep = '\n')
cat(paste0("* Soil        : ", unique(data["LOC_ID"]), " - texture classes:", paste(data[1:nrow(data),"STC"], collapse = "-"), sep = '\n'))
cat(paste0('* File name        : ', unique(data["ID"]), ".sol"), sep = '\n') 
cat(paste0('* Sampling date      : ',data$SAMPLING_DATE[1] ) ,sep = '\n') 
cat(paste0('* Additional info  : ', 'Create with https://github.com/jrodriguez88/ORYZA_Model_RTOOLS') ,sep = '\n') 
cat('*--------------------------------------------------------------------*',sep = '\n') 

cat('\n')
#"* Give code name of soil data file to match the water balance PADDY:"
cat("SCODE = 'PADDY'", sep = '\n')
cat('\n')
cat('*---------------------------------------------------------------*
* 1. Various soil and management parameters
*---------------------------------------------------------------*')
cat('\n')
#sink()

cat(paste0("WL0MX = ", 100, ".",   "   ! Bund height (mm)"),sep = '\n')
cat(paste0("NL = ", nrow(data), "        ! Number of soil layers (maximum is 10) (-)"),sep = '\n')
cat(paste0("TKL = ", inpp("DEPTH", 100), "   ! Thickness of each soil layer (m)"),sep = '\n')
cat(paste0("ZRTMS = ", ZRTMS,"   ! Maximum rooting depth in the soil (m)"),sep = '\n')
cat('\n')

cat('*---------------------------------------------------------------*
* 2. Puddling switch: 1=PUDDLED or 0=NON PUDDLED
*---------------------------------------------------------------*')
cat('\n')
cat("SWITPD = 0  !Non puddled")

cat('\n')
#"* If PUDDLED, supply parameters for puddled soil"
cat("NLPUD = 1", sep = '\n') # ! Number of puddled soil layers, including the plow sole (-)

#"* Saturated volumetric water content of ripened (previously puddled) soil (m3 m-3), for each soil layer:"
cat(paste0("WCSTRP = ", inpp("WCST", 100)), sep = '\n')
#"* Soil water tension of puddled soil layer at which cracks reach break through the plow sole (pF):"
cat("PFCR = 6.0", sep = '\n')

cat(paste0("DPLOWPAN = ",sum(data[,"DEPTH"])/100), sep = '\n')
cat('\n')
cat("*---------------------------------------------------------------*
* 3. Groundwater switch: 0=DEEP (i.e., not in profile), 1=DATA
* (supplied), 2=CALCULATE
*---------------------------------------------------------------*")
cat('\n')
cat("SWITGW = 0", sep = '\n') #! 0:Deep groundwater, 1: Obs.data, 2: Calculate"
#"* If DATA, supply table of groundwater table depth (cm; Y-value) * as function of calendar day (d; X value):"
cat("ZWTB =   1.,200.,", sep = '\n')
cat("       366.,200.", sep = '\n')
cat('\n')

#"* If CALCULATE, supply the following parameters:
cat("ZWTBI = 100. ! Initial groundwater table depth (cm)
MINGW = 100. ! Minimum groundwater table depth (cm)
MAXGW = 100. ! Maximum groundwater table depth (cm)
ZWA   = 1.0  ! Receding rate of groundwater with no recharge (cm d-1)
ZWB   = 0.5  ! Sensitivity factor of groundwater recharge (-)")
cat('\n')
cat('\n')
cat("*---------------------------------------------------------------*
* 4. Percolation switch
* Value for SWITVP cannot be 1 (CALCULATE) for non-puddled soil
*---------------------------------------------------------------*")
cat('\n')
cat("SWITVP = -1 ! Fixed percolation rate", sep = '\n')
#"*SWITVP = 0 ! Percolation as function of the groundwater depth"
#"*SWITVP = 1 ! Calculate percolation"
#"*SWITVP = 2 ! Fixed percolation rate as function of time"
#"* If SWITVP = -1, supply fixed percolation rate (mm d-1):"
cat(paste0("FIXPERC = ", data[nrow(data),"SSKS"]/10, "."), sep = '\n')

#"* If SWITVP = 0, supply table of percolation rate (mm d-1; Y-value)"
#"* as function of water table depth (cm; X value):"
#"*PERTB =   0., 3.,"
#"*        200., 3."

#"* If SWITVP = 2, give percolation rate (mm/d) as function of calendar day"
cat('\n')
cat("PTABLE =
  1., 1.0,   
 50., 1.0,
100., 20.0,
366., 20.0")
cat('\n')
cat('\n')
cat("*---------------------------------------------------------------*
* 5. Conductivity switch: 0=NO DATA, 1=VAN GENUCHTEN or 2=POWER
*  OR 3= SPAW  function used
*---------------------------------------------------------------*")
cat('\n')
cat("SWITKH = 0 ! No data", sep = '\n')
cat('\n')
cat("*---------------------------------------------------------------*
* 6. Water retention switch: 0=DATA; 1=VAN GENUCHTEN. When DATA, data
* have to be supplied for saturation, field capacity,
* wilting point and at air dryness
*---------------------------------------------------------------*")
cat('\n')
cat("SWITPF = 0  ! Data", sep = '\n')
cat('\n')
cat("*---------------------------------------------------------------*
* 7.Soil physical properties, these parameters will be used when model
* runs under actual water or nitrogen condition, or even both. Otherwise
* these parameters will not be used.
*---------------------------------------------------------------*")
cat('\n')
cat(paste0("CLAYX = ", inpp("CLAY", 100), sep = '\n'))     #!soil clay content, fraction"
cat(paste0("SANDX = ", inpp("SAND", 100), sep = '\n'))     #!soil sand content, fraction"
cat(paste0("BD = ", inpp("SBDM"), sep = '\n'))            #!soil bulk density (g/cm3)"
cat('\n')

#swph <- data%>%mutate(sw=DEPTH*SBDM*1000)%>%select(sw)
#"*Soil organic carbon and nitrogen content in kg C or N/ha"
cat(paste0("SOC = ", inpp("SOC")), sep='\n')     #! Soil organic C (kg C/ha)"
cat(paste0("SON = ", inpp("SON")), sep='\n')       #! Soil organic N (kg N/ha)

cat(paste0("SNH4X = ", inpp("SNH4X")), sep='\n')      #!*soil NH4-N (kg N/ha)
cat(paste0("SNO3X = ", inpp("SNO3X")), sep='\n')   #!*soil NO3-N (kg N/ha)

#"*FORGANC =200.0,1000.0, 5*0.0   !* Fresh organic carbon (kg C/ha)"
#"*FORGANN = 10.0,100.0,5*0.0     !* Fresh organic nitrogen (kg N/ha)"
#"*FCarboh = 0.54                 !* Fraction of carbonhydrate in fresh organic matter (--)"
#"*FCellulo = 0.38                !* Fraction of cellulos in fresh organic matter (--)"
cat('\n')
cat("*-----------------------------------------------------------------*
* 8. Soil hydrological properties. Required type of data input    *
* according to setting of conductivity and water retention switch *
*-----------------------------------------------------------------*")
cat('\n')
#"* Saturated hydraulic conductivity, for each soil layer (cm d-1) (always required!):"
cat(paste0("KST = ", inpp("SSKS"), sep='\n'))

#"* Saturated volumetric water content, for each soil layer (m3 m-3)(always required!):"
cat(paste0("WCST = ", inpp("WCST", 100), sep='\n'))
#"* Van Genuchten parameters, for each soil layer (needed if SWITKH = 1 and/or SWITPF = 1):"
#"VGA = 2*0.0195,0.0177,0.0147,0.0145,0.0189,0.558     !* a parameter (cm-1)"
#"VGL = -1.945,-1.945,-1.8365,-3.773,-1.646,-0.563,1.268  !* l parameter (-)"
#"VGN = 1.104,1.104,1.120,1.062,1.096,1.097,1.0264     !* n parameter (-)"
#"VGR = 7*0.01                                         !* residual water content (-)"
#"* Power function parameters, for each soil layer (-) (needed if SWITKH = 2):"
#"*PN = 3*-2.5, 3*-2.5, 2*-2.5, -2.5"

#"*!* Volumetric water content at field capacity, for each soil layer (m3 m-3)(needed if SWITPF = 0):"
cat(paste0("WCFC = ", inpp("WCFC", 100), sep='\n'))

#"*!* Volumetric water content at wilting point, for each soil layer (m3 m-3) (needed if SWITPF = 0):"
cat(paste0("WCWP = ", inpp("WCWP", 100), sep='\n'))

#"*!* Volumetric water content at air dryness, for each soil layer (m3 m-3) (needed if SWITPF = 0):"
cat(paste0("WCAD = ", inpp("WCAD",100), sep='\n'))
cat('\n')
cat("*---------------------------------------------------------------*
* 9. Initialization conditions, and re-initialization
*---------------------------------------------------------------*")
cat('\n')
cat(paste0("WL0I = ", WL0I, "."), sep='\n')   #! Initial ponded water depth at start of simulation (mm)"
#"* Initial volumetric water content at the start of simulation,for each soil layer (m3 m-3):  USE ALWAYS FIELD CAPACITY, OR 0.5 TIMES WCST"
cat(paste0("WCLI = ",
           if(WCLI=='FC'){inpp("WCFC", 100)
               }else if(WCLI=='ST50'){paste0(round(data[,"WCST"]*0.5/100,2), collapse = ", ")
                   }else{paste0(rep(WCLI,3), collapse= ", ")}), sep = '\n')

#"* Re-initialize switch RIWCLI is YES or NO"
cat(paste0("RIWCLI = ", "'", RIWCLI, "'"), sep = '\n')

cat('\n')
cat("*---------------------------------------------------------------*
* 10. Initialization of soil thermal conditions
*---------------------------------------------------------------*")
cat('\n')
cat(paste0("SATAV = ", SATAV, "."), sep='\n')     #! Soil annual average temperature of the first layers"
cat("SOILT = ", paste0(c(SATAV, SATAV-2, SATAV-3), collapse = "., "),".", sep = "")
cat('\n')
cat('\n')
cat("*---------------------------------------------------------------*
* 11. Observations/measurements
*    Switches to force observed water content in water balance
*---------------------------------------------------------------*")
cat('\n')

#"*!* WCL1_OBS, WCL2_OBS,...WCL10_OBS: Observed soil water contents"
#"* in layer 1, 2, ..., 10. Format: year, day number, water content"
#"* Not obligatory to give data"
#""
#"*WCL1_OBS ="
#"* TO BE FILLED-IN (OPTIONAL)"
#""
#"*!* Parameter to set forcing of observed water content yes (2) or no (0)"
#"* during simulation (instead of using simulated values)"
#"*WCL1_FRC = 0 ! (0: No forcing, 2: Forcing)"

a <- c("WCLINT = 1,1,1,",
"         2,2,2,",
"         3,3,3,",
"         4,4,4,",
"         5,5,5,",
"         6,6,6,",
"         7,7,7,")
a[nrow(data)] <- str_sub(a[nrow(data)], end = -2)
writeLines(a[1:nrow(data)])
sink()
}

#lapply(soil_data, Make_SOIL_ORYZA, path=path)








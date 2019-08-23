#########################################################
####            Make_SPGF ORYZA_Model_RTOOLS         ####
####     By https://github.com/jrodriguez88          ####
####      ORYZA Rice Crop Model described in:        #### 
####   http://books.irri.org/9712201716_content.pdf  ####
#########################################################

# SPGF: The spikelet growth formation factor (SPGF; number kg−1) was derived as
#       the slope of the relationship between spikelet number m−2 and growth of the
#       crop between panicle initiation and flowering (g m-2)


#library(tidyverse)
#library(lubridate)

#path_data <- getwd()
#CULTIVAR <- "FED2000"

## SPGF_cal function compute SPGF from experimental data, It take yield traits (spikelet number), 
##          growth data (biomass) and phenology observation to find the relationship between spikelet
##          number and and crop growth between panicle initiation and flowering.

## Function to extract variables (from INPUT_data.xls template) requiere to SPGF calculation
SPGF_extract <- function(path_data, filename, max_diff = 5) {
    
    read_INPUT_data <- function(file) {
        sheets <- readxl::excel_sheets(file)
        x <-    lapply(sheets, function(X) readxl::read_excel(file, sheet = X))
        names(x) <- sheets
        x
    }
    ##Load data for each workbook (XLSX)   
    INPUT_data <- read_INPUT_data(paste0(path_data, filename))
    
    ##Extract Spikelets number from YIELD_obs and join with Phenology observations (PHEN_bs)   
    SPIK_by_EXP <- INPUT_data$YIELD_obs %>%
        mutate(SPIK_M2_avg=PAN_M2*GT_PAN,
               SPIK_M2_min=(PAN_M2-PAN_M2_SD)*(GT_PAN-GT_PAN_SD),
               SPIK_M2_max=(PAN_M2+PAN_M2_SD)*(GT_PAN+GT_PAN_SD))%>%
        left_join(INPUT_data$PHEN_obs, by="ID")%>%
        select(ID, contains("SPIK_M2"), IDAT, FDAT)
    
    ##Extract  Total dry weight at panicle initiation or closest sampling date
    WAGT_PI <- INPUT_data$PLANT_gro %>%
        inner_join(SPIK_by_EXP, by="ID") %>%
        mutate(diff_pi=abs(as.integer(difftime(SAMPLING_DATE, IDAT, units = "days"))),
               WAGT_PI=WAGT_OBS, 
               WAGT_PI_SD=WAGT_SD)%>%
        group_by(ID) %>%
        filter(diff_pi==min(diff_pi))%>%
        select(ID, diff_pi, contains("WAGT_PI"))
    
    ##Extract  Total dry weight at flowering initiation or closest sampling date    
    WAGT_FL <- INPUT_data$PLANT_gro %>%
        inner_join(SPIK_by_EXP, by="ID") %>%
        mutate(diff_fl=abs(as.integer(difftime(SAMPLING_DATE, FDAT, units = "days"))),
               WAGT_FL=WAGT_OBS, 
               WAGT_FL_SD=WAGT_SD)%>%
        group_by(ID) %>%
        filter(diff_fl==min(diff_fl))%>%
        select(ID, diff_fl, contains("WAGT_FL"))
    
    ##Join data and compute variables to SPGF calculation  
    SPIK_by_EXP %>%
        left_join(WAGT_PI, by = "ID")%>%left_join(WAGT_FL, by = "ID") %>%
        mutate(diff_pi_fl=(WAGT_FL-WAGT_PI)/10) %>%#(g/m²)
        filter(diff_fl<max_diff, diff_pi<max_diff)
    
}


## Function to calculate SPGF by lm 

SPGF_cal <- function(path_data, files, out="") {
    
    ## Create SPGF_df: Compute SPGF variables by locality and bind rows 
    SPGF_df <- pmap(list(path_data=path_data,  filename = files), SPGF_extract) %>%
        bind_rows()%>%mutate(LOC_ID=substr(ID, 1,4))
    
    SPGF_df <<- SPGF_df %>% filter(ID != out)
    
    
    ## Linear model between Spikelet number (number/ m²) and crop growth between panicle initiation and flowering (g/m²)
    lm_spgf <- lm(formula = SPIK_M2_max ~ diff_pi_fl, data = SPGF_df)
    
    ## Print SPGF compute from lm-slope
    sprintf("%.1f", coef(lm_spgf)[[2]]*1000) # Spikelet growth factor (no kg-1)"))
    
}
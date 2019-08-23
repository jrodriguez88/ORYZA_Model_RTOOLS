# Function to estimate Soil Saturated Hydraulic Conductivity (SSKS) from different Soil Pedotransfer Function (PTF)

# Git Repository: https://github.com/jrodriguez88/ORYZA_Model_RTOOLS 


#require(RCurl)

# Load PTFs
#source("https://raw.githubusercontent.com/jrodriguez88/ORYZA_Model_RTOOLS/master/PT_Functions.R")

# S Sand (%)
# C Clay (%)
# OM Organic matter (%)
#
#SBDM by texture <-  "https://www.nrcs.usda.gov/wps/portal/nrcs/detail/soils/survey/office/ssr10/tr/?cid=nrcs144p2_074844"
# For rice-soils BD values between 1.3 - 1.7 (g/cm³)
# OM values between 1 - 3 (%)
# min and max are SSKS-threshold

SSKs_cal <- function(S, C, SOM=1.5, SBDM=1.5, kmin=0.1, kmax=NA, metric='bootmean') {
    
    stopifnot(require(RCurl))
    stopifnot(require(tidyverse))

    
    ssks_data <- data.frame(S,C,SOM,SBDM) %>%
        mutate(SSKS_Brakensiek = SSKS_Brakensiek(S, C, SOM, SBDM),
#               SSKS_Campbell = SSKS_Campbell(S, C),
               SSKS_Cosby = SSKS_Cosby(S, C),
               SSKS_Dane_Puckett = SSKS_Dane_Puck(C),
#               SSKS_Jabro = SSKS_Jabro(S, C, SBDM),
               SSKS_Puckett = SSKS_Puckett(C),
#               SSKS_Rawls = SSKS_Rawls(S, C),
               SSKS_Saxton = SSKS_Saxton(S, C, SOM, SBDM), 
               SSKS_Suleiman_Ritchie = SSKS_Sul_Rit(S, C, SOM, SBDM), 
               SSKS_Wosten = SSKS_Wosten99(S,C, SOM, SBDM),
               #           SSKS_Vereecken(S, C, SOM, SBDM),
               SSKS_Ferrer = SSKS_Ferrer(S)) %>%
        select(contains("SSKS")) %>%
        gather(key="KS_PTF", value = "SSKS") %>%
        extract(KS_PTF, "KS_PTF", "_([a-zA-Z0-9_]+)") %>%
        mutate(KS_PTF=as.factor(KS_PTF), SSKS=replace(SSKS, SSKS>kmax, NA), SSKS=replace(SSKS, SSKS<kmin, NA))%>%
        na.omit() %>%
        summarize(ssks_bootmean = mean(sample(SSKS, 1000, replace = T)),
                  ssks_mean = mean(SSKS),
                  ssks_bootmedian = median(sample(SSKS, 1000, replace = T)),
                  ssks_median = median(SSKS),
                  ssks_min = quantile(sample(SSKS, 1000, replace = T),0.025),
                  ssks_max = quantile(sample(SSKS, 1000, replace = T),0.975),
                  ssks_sd = sd(SSKS))
    
    switch (metric,
        bootmean = ssks_data$ssks_bootmean,
        mean = ssks_data$ssks_mean,
        bootmedian = ssks_data$bootmedian,
        median = ssks_data$ssks_median,
        min = ssks_data$ssks_min,
        max = ssks_data$ssks_max,
        sd = ssks_data$ssks_sd)
        
        
}

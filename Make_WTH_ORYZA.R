######################################################################################
#######      Make ORYZA weather files by year                      #############
####################    by JRE- https://github.com/jrodriguez88    ###################     
######################################################################################


### ORYZA weather file, include:

#  Column    Daily Value                                                                         
#     1      Station number                                                                       
#     2      Year                                                                           
#     3      Day                                                                    
#     4      irradiance         KJ m-2 d-1                                            
#     5      min temperature            oC                                                        
#     6      max temperature            oC                                                        
#     7      vapor pressure            kPa                                                        
#     8      mean wind speed         m s-1                                                        
#     9      precipitation          mm d-1


## Make_WTH_ORYZA function compute weather information to ORYZA weather file.
## 'data':  csv file name or data.frame. 
#           str-> 
#                ..$ DATE: Date ->(mdy)
#                ..$ TMAX: num  ->(oC)
#                ..$ TMIN: num  ->(oC) 
#                ..$ RAIN: num  ->(mm) 
#                ..$ SRAD: num  ->(MJ) 
#                ..$ RHUM: num  ->(%) 
## 'path':  path folder or working directory
## 'local': 4 letters string of locality name. "AIHU"--> Aipe, Huila
## 'lat':   latitud (decimal degrees)
## 'lon':   longitud (decimal degrees)
## 'alt':   altitude (meters above sea level)


Make_WTH_ORYZA <- function(data, path, local, lat, lon, alt, stn=1) {
    stopifnot(require(tidyverse)==T)
    stopifnot(require(lubridate)==T)
    if(any(class(data)=="data.frame")){
        DATA <- data %>%
            mutate(DATE = as.Date(DATE, format= "%m/%d/%Y"),
                   Station_number = stn,
                   Year = year(DATE),
                   Day = yday(DATE),
                   SRAD = round(SRAD*1000, 2),
                   TMAX = round(TMAX, 2),
                   TMIN = round(TMIN, 2),
                   RAIN = round(RAIN, 2),
                   VPD = -99, 
                   WS = -99) %>%
            select(Station_number, Year, Day, SRAD, TMIN, TMAX, VPD, WS, RAIN)
    
        } else if(str_detect(data, ".csv")==T){
    DATA <- read.csv(paste0(path,"//", data), header = T) %>%
        mutate(DATE = as.Date(DATE, format= "%m/%d/%Y"),
               Station_number = stn,
               Year = year(DATE),
               Day = yday(DATE),
               SRAD = round(SRAD*1000, 2),
               TMAX = round(TMAX, 2),
               TMIN = round(TMIN, 2),
               RAIN = round(RAIN, 2),
               VPD = -99, 
               WS = -99) %>%
        select(Station_number, Year, Day, SRAD, TMIN, TMAX, VPD, WS, RAIN)
        
        } else {message("data no recognized")}
    
dir.create(paste0(path,"/WTH"), showWarnings = FALSE)
set_head <- paste(lon, lat, alt, 0, 0, sep = ",")    
#DATA=read.table(file, head=T)  
data_list <- split(DATA, DATA$Year)
lapply(data_list, function(x){
    fname <- paste(path,"/WTH/" ,local, stn,".", str_sub(unique(x$Year), 2), sep = "")
    sink(file=fname)
    cat(set_head)
    cat("\n")
    write.table(x ,sep=",",row.names=F,col.names=F)
    sink()})

}

#map(data_list, copy_wth)
#data <- "weather_input.csv"
#path <- getwd() 
#local<- "AIHU"
#lat <- 3.5
#lon <- -75.5
#alt <- 250
#Make_WTH_ORYZA(AIHU$WTH_obs, path,local, 3.4, -72, 250)
#Make_WTH_ORYZA("weather_input.csv", path,local, 3.4, -72, 250)

#library(tidyverse)
#list.files(pattern=".RData") %>% lapply(load, .GlobalEnv)
#locs_id <- ls()

#path <- getwd()
#wdata <- function (loc) {
#    loc<- get(loc)
#    data <- loc$WTH_obs
#    path <- path
#    local <- loc$AGRO_man$LOC_ID[1]
#    lat <- loc$AGRO_man$LAT[1]
#    lon <- loc$AGRO_man$LONG[1]
#    alt <- loc$AGRO_man$ALT[1]
#    
#    Make_WTH_ORYZA(data, path, local, lat, lon, alt)
#    }
#
#lapply(locs_id, wdata)

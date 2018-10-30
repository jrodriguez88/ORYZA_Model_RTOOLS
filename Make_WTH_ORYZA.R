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
## 'path':  path folder or working directory
## 'local': 4 letters string of locality name
## 'lat':   latitud (decimal degrees)
## 'lon':   longitud (decimal degrees)
## 'alt':   altitude (meters above sea level)

Make_WTH_ORYZA <- function(data, path, local, lat, lon, alt) {
    stopifnot(require(tidyverse)==T)
    stopifnot(require(lubridate)==T)
    if(any(class(data)=="data.frame")){
        DATA <- data %>%
            mutate(DATE = as.Date(DATE, format= "%m/%d/%Y"),
                   Station_number = 1,
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
               Station_number = 1,
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
year_i=min(DATA$Year)
year_f=max(DATA$Year)


#Crea matrix para orgnizar datos
year=cbind(year_i:year_f) %>%
    as.data.frame()


dates_wth=str_sub(year[1,1],-3,-1)

for (z in 2:nrow(year)){
    dates_wth=rbind(dates_wth,(str_sub(year[z,1],-3,-1)))  
}

dates_wth <- as.data.frame(dates_wth)

wth_year=list()

for (n in year_i:year_f){
    
    wth_year[[n-(year_i-1)]]=DATA[DATA$Year==n,1:9]
    a=dates_wth[n-(year_i-1),1]
    b=paste(path,"//WTH/" ,local, 1, ".",a,sep="")
    sink(b)
    cat(set_head)
    cat("\n")
    write.table(wth_year[[n-(year_i-1)]],sep=",",row.names=F,col.names=F)
    sink()
    
}
print("DONE!")
}

#path <- getwd() 
#local<- "AIHU"
#Make_WTH_ORYZA(AIHU$WTH_obs, path,local, 3.4, -72, 250)
#Make_WTH_ORYZA("weather_input.csv", path,local, 3.4, -72, 250)


#list.files(pattern=".RData") %>% lapply(load, .GlobalEnv)


#path <- getwd()
#wdata <- function (loc) {
    data <- loc$WTH_obs
    path <- path
    local <- loc$AGRO_man$LOC_ID[1]
    lat <- loc$AGRO_man$LAT[1]
    lon <- loc$AGRO_man$LONG[1]
    alt <- loc$AGRO_man$ALT[1]
    
    Make_WTH_ORYZA(data, path, local, lat, lon, alt)
    }

#wdata(YOCS)

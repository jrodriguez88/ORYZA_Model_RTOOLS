######################################################################################
####### Crea la estructura de las extenciones de los archivos climaticos #############
####################    by JRE- https://github.com/jrodriguez88    ###################     
######################################################################################
#file <- "AIHU_FED2000.xlsx"

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

Make_WTH_ORYZA <- function(filename) {
    
    DATA <- read.csv(paste0(getwd(),"//", filename),header = T) %>%
        mutate(DATE = as.Date(DATE, format= "%m/%d/%Y"),
               Station_number = 1,
               Year = year(DATE),
               Day = yday(DATE),
               SRAD = round(SRAD, 2),
               TMAX = round(TMAX, 2),
               TMIN = round(TMIN, 2),
               RAIN = round(RAIN, 2),
               VPD = -99, 
               WS = -99) %>%
        select(Station_number, Year, Day, SRAD, TMIN, TMAX, VPD, WS, RAIN)
    
    
dir.create(paste0(getwd(),"/WTH"), showWarnings = FALSE)
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
    b=paste(getwd(),"//WTH/" ,local, 1, ".",a,sep="")
    sink(b)
    cat(set_head)
    cat("\n")
    write.table(wth_year[[n-(year_i-1)]],sep=",",row.names=F,col.names=F)
    sink()
    
}
print("DONE!")
}

Make_WTH_ORYZA("weather_input.csv")
  
  
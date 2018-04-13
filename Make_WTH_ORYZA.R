
# __    __           _   _   _                  __ _ _              
#/ / /\ \ \___  __ _| |_| |_| |__   ___ _ __   / _(_) | ___         
#\ \/  \/ / _ \/ _` | __| __| '_ \ / _ \ '__| | |_| | |/ _ \  _____ 
# \  /\  /  __/ (_| | |_| |_| | | |  __/ |    |  _| | |  __/ |_____|
#  \/  \/ \___|\__,_|\__|\__|_| |_|\___|_|    |_| |_|_|\___|        

#   ___                     ____   ___   ___   ___  
#  /___\_ __ _   _ ______ _|___ \ / _ \ / _ \ / _ \ 
# //  // '__| | | |_  / _` | __) | | | | | | | | | |
#/ \_//| |  | |_| |/ / (_| |/ __/| |_| | |_| | |_| |
#\___/ |_|   \__, /___\__,_|_____|\___/ \___/ \___/ 
#            |___/                                  

######################################################################################
####### Crea la estructura de las extenciones de los archivos climáticos #############
####################    by JRE- https://github.com/jrodriguez88    ###################     
######################################################################################

#Cargar Packages
require(stringr)

###Defina datos de analisis
user <- "C:\\ORYZA\\WEATHER\\"       #Definir nombre de usuario/path en windows
setwd(user)
loc <- "VVME"                        #Definir nombre de localidad, 4 letras
cord <- "-72.53,4.03,315,0,0"        #Definir coordenadas de localidad, ver info abajo

#Modificar coordenadas (solo info)
#SDTO -74.98,3.917,305,0,0
#AIHU -74.767,3.250,380,0,0
#MRCO -74.15,8.8,14,0,0
#YOCS -72.15,5.15,250,0,0
#VVME -72.53,4.03,315,0,0
#IBTO -75.087, 4.42, 750,0,0


#Definir años de analisis
year_i=2008
year_f=2016


#Cargar Datos copiados, Formato: "cod	ano	dia	rad	tmin tmax viento vapor lluvia"
DATA=read.table("clipboard",head=T) 

#Crea matrix para orgnizar datos
ano=cbind(year_i:year_f)
ano=as.data.frame(ano)

fechas_clima=str_sub(ano[1,1],-3,-1)

for (z in 2:nrow(ano)){
  fechas_clima=rbind(fechas_clima,(str_sub(ano[z,1],-3,-1)))  
}

fechas_clima=as.data.frame(fechas_clima)



####################################################################################
######### Cre los archivos climáticos para el modelo Oryza2000, por cada año #######
####################################################################################


clima_ano=list()
  
  for (n in year_i:year_f){
    
    clima_ano[[n-(year_i-1)]]=DATA[DATA$ano==n,1:9]
    a=fechas_clima[n-(year_i-1),1]
    b=paste(user,loc,"",".",a,sep="")
    sink(b)
    cat(cord)
    cat("\n")
    write.table(clima_ano[[n-(year_i-1)]],sep=",",row.names=F,col.names=F)
    sink()
    
  }
  
  
#### Group of recursive functions used in ORYZA_AUTO_PARAM and ORYZA_Model_RTOOLS

### Download *.EXE  ----> drates.exe, param.exe, oryzav3.exe, standard.crp 
download_ORYZA_Tools <- function(){
    ip <- function() {
        if (.Platform$OS.type == "windows") {
            ipmessage <- system("ipconfig", intern = TRUE)
        } else {
            ipmessage <- system("ifconfig", intern = TRUE)
        }
        validIP <- "((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)[.]){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
        any(grep(validIP, ipmessage))
    }  
    
    if ((file.exists("ORYZA3.exe")) &&
        (file.exists("drate(v2).exe")) &&
        (file.exists("PARAM(v2).exe")) &&
        (file.exists("standard.crp"))){
    } else if(ip()==T){
        
        # Download DRATES and PARAM app  
        download.file(url='https://sites.google.com/a/irri.org/oryza2000/downloads/new-release/download-oryza-version3/AllTools.zip',
                      destfile='AllTools.zip', method='auto')
        ls_tools<- unzip('AllTools.zip', list = T)
        unzip('AllTools.zip', files = ls_tools$Name[c(2,4)])
        
        # Download ORYZA.exe
        download.file(url='https://sites.google.com/a/irri.org/oryza2000/downloads/new-release/download-oryza-version3/ORYZA3.zip',
                      destfile='ORYZA3.zip', method='auto')
        unzip('ORYZA3.zip', files="ORYZA3.exe")
        
        #Download standard.crp
        download.file("https://sites.google.com/a/irri.org/oryza2000/downloads/new-release/download-oryza-version3/standard.crp",
                      destfile = "standard.crp", method='auto')
        
        file.remove('AllTools.zip')
        file.remove('ORYZA3.zip')
    } else {
        mens <- cat(
"#####################################################
####       WARNING! NO INTERNET CONECTION        ####
####      It is need copy ORYZA model Tools:     ####
####  ORYZA3.exe & drate(v2).exe & PARAM(v2).exe ####
####        AND CROP FILE standard.crp           ####
#####################################################")
        
        stopifnot((file.exists("ORYZA3.exe")) &&
                      (file.exists("drate(v2).exe")) &&
                      (file.exists("PARAM(v2).exe")) &&
                      (file.exists("standard.crp")))
        
        print(mens)
    }
    
}

### Install R Packages and dependeces.
inpack <- function(pack){
    new_pack <- pack[!(pack %in% installed.packages()[, "Package"])]
    if (length(new_pack)) 
        install.packages(new_pack, dependencies = TRUE)
    sapply(pack, require, character.only = TRUE)
}


### 'read_INPUT_data' function to read xlsx files ---->  c(LOC_ID, cultivar), base_raw_data
read_INPUT_data <- function(file) {
    sheets <- readxl::excel_sheets(file)
    x <-    lapply(sheets, function(X) readxl::read_excel(file, sheet = X))
    names(x) <- sheets
    x
}


### 'get_STC' function to get Soil Texture Class from soil sand, clay content.. based USDA system class
get_STC <- function(S, C, sysclass="USDA") {
    stopifnot(require(soiltexture))
    
    Si <- 100-(S+C)
    dat <- data.frame(SAND=S, CLAY=C, SILT=Si)
    
    STC <- TT.points.in.classes(
        tri.data = dat,
        class.sys = paste0(sysclass, ".TT"),
        PiC.type = "t"
    )
    
    return(STC)
    
}


###
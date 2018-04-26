#########################################################
####            Master ORYZA_Model_RTOOLS            ####
####     By https://github.com/jrodriguez88          ####
####      ORYZA Rice Crop Model described in:        #### 
####   http://books.irri.org/9712201716_content.pdf  ####
#########################################################


##### LOCAL DATA

local <- "AIHU"
lat <- 3.253
lon <- -75.24
alt <- 380

read_INPUT_data <- function(filename) {
    sheets <- readxl::excel_sheets(filename)
    x <-    lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
    names(x) <- sheets
    x
}

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



###### Make Weather files

source("Make_WTH_ORYZA.R")


###### Make Experimental files

source("Make_EXP_ORYZA.R")



######




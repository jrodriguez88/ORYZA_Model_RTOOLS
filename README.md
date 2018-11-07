
# ORYZA_Model_RTOOLS: Tools for Rice Crop Simulation with ORYZA V3.0
### *Rodriguez-Espinoza J.*
#### International Center for Tropical Agriculture
#### Contact: [Email](mailto:j.r.espinosa@cgiar.org) - [LinkedIn](https://www.linkedin.com/in/jeferson-rodriguez-espinoza-24749625/)
#### Repository: [GitHub](https://github.com/jrodriguez88/ORYZA_Model_RTOOLS)

## Description
Project with many tools to use ORYZA crop model. It Includes functions to create input files (weather `*.WTH`, soil `*.SOL`, experimental `*.EXP`), estimate parameters (crop, weather and soil), import and visualize outputs. It will conect with [ORYZA_AUTO_PARAM](https://github.com/jrodriguez88/ORYZA_AUTO_PARAM) tool to compute diferent parameters required in the crop model parametrization and calibration.

#### For more information about the ORYZA Crop Model:

* Official [ORYZA Model website](https://sites.google.com/a/irri.org/oryza2000/about-oryza-version-3)

* Book about [ORYZA2000: modeling lowland rice.](http://irri.org/resources/publications/books/item/oryza2000-modeling-lowland-rice) 

* Research paper about [ORYZA V3.0](https://www.sciencedirect.com/science/article/pii/S0168192317300680)


## Make_WTH_ORYZA.R
Function to create ORYZA-model weather files.

### Usage
```
require(tidyverse)
require(lubridate)
Make_WTH_ORYZA(data, path, local, lat, lon, alt, stn=1)
```

### Arguments

* `data:` csv file name or data.frame of daily values. [View Weather Template](https://raw.githubusercontent.com/jrodriguez88/ORYZA_Model_RTOOLS/master/weather_input.csv)

| Var_Name | Description  |  Class| Unit |
| --- | --- | --- | --- | 
| DATE |  Date in "mm/dd/yyyy" | Date | (mdy)|
| TMAX |  Maximum temperature | num  | (oC) |
| TMIN |  Minimun temperature | num  | (oC) |
| RAIN |  Rain or Precipitation | num  | (mm) |
| SRAD |  Solar Radiation | num  | (MJ) |
| RHUM |  Relative humidity (opcional) | num  | (%)  |

* `path:`      path folder or working directory
* `local:`     4 letters string of locality name. "AIHU"--> Aipe, Huila
* `lat:`       latitud (decimal degrees)
* `lon:`       longitud (decimal degrees)
* `alt:`       altitude (meters above sea level)
* `stn:`       Station number. default=1

## Make_SOIL_ORYZA.R
Function to create ORYZA-model soil files. Default PADDY model

### Usage
```
require(tidyverse)
Make_SOIL_ORYZA(data, path, ZRTMS = 0.50, WL0I = 0, WCLI='FC' , RIWCLI = 'NO', SATAV=20)
```

### Arguments

* `data:` csv file name or data.frame. [View Soil Template](https://raw.githubusercontent.com/jrodriguez88/ORYZA_Model_RTOOLS/master/soil_input.csv)

| Var_Name  | Description | Class| Unit |
| --- | --- | --- | --- |
| ID |	Trial ID |	character | XXXX |
| SAMPLING_DATE | Sampling date		| date	| 	MM/DD/YYYY	| 
| DEPTH |	Soil depth 	| 	numeric	| 	cm	| 
| PH |	pH in water	| 	numeric	| 	units	| 
| SCARBON |	Total soil carbon	| 	numeric	| 	g/kg	| 
| SCEC| Cation exchange capacity	| 	numeric	| 	cmol/kg	| 
| SNH4| Ammonium, KCl, g elemental N	| 	numeric	| 	mg/kg	| 
| SNO3| Nitrate, KCl, g elemental N	|  	numeric	| 	mg/kg	| 
| SLNI| Total nitrogen	| 	numeric	| 	mg/kg	| 
| SLON| Organic nitrogen	| 	numeric	| 	mg/kg	| 
| SBDM | Bulk density, moist 	| 	numeric	| 	g/cm³	| 
| SRD| Real density	| 	numeric	| 	g/cm³	| 	| 
| SPOR| Total soil porosity	| 	numeric	| 	%	| 
| WCF| Water content at field	| 	numeric	| 	%	| 
| SSKS| Saturated hydraulic conductivity	| 	numeric	| 	cm/h	| 
| WCST| Saturated volumetric water content	| 	numeric	| 	%	| 
| WCFC| Volumetric water content at field capacity	| 	numeric	| 	%	| 
| WC1B| Volumetric water content at 1 bar	| 	numeric	| 	%	| 
| WCWP| Volumetric water content at wilting point	| 	numeric	| 	%	| 
| SAND	| Soil sand content	| numeric	| 	%	| 
| SILT	| Soil silt content	| numeric	| 	%	| 
| CLAY 	| Soil clay content	| numeric	| 	%	| 
| STC	| Soil texture class (USDA)	| 	character	| 	name	| 

* `path:`   path folder or working directory
* `ZRTMS:`  Maximum rooting depth in the soil (m), 
* `WL0I:`   Initial ponded water depth at start of simulation (mm)"
* `WCLI:`   can take 3 values: Field Capacity ('FC'), 50% of Soil Saturation ('ST50'), Fraction of water content ('0.0'- '1.0') 
* `RIWCLI:` Re-initialize switch RIWCLI is YES or NO
* `SATAV:`  Soil annual average temperature of the first layers


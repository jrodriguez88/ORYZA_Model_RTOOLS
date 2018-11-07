
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

* data: csv file name or data.frame.

| Var_Name  |  Class| Unit |
| --- | --- | --- | 
| DATE |  Date | (mdy)|
| TMAX |  num  | (oC) |
| TMIN |  num  | (oC) |
| RAIN |  num  | (mm) |
| SRAD |  num  | (MJ) |
| RHUM |  num  | (%)  |

* `path:`      path folder or working directory
* `local:`     4 letters string of locality name. "AIHU"--> Aipe, Huila
* `lat:`       latitud (decimal degrees)
* `lon:`       longitud (decimal degrees)
* `alt:`       altitude (meters above sea level)
* `stn:`       Station number. default=1


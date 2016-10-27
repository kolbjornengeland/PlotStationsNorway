# PlotStationsNorway

R package for plotting caharcetristics of streamflow stations on  a map of Norway  

## Installation

Download the *.R files into your own directory and 'source' them:

```R
setwd('C:/Users/koe/Documents/PlotStationsNorway')
source('PlotMap.R')
```

You will need the RNetCDF package:

```R
install('maptools')
install('RColorBrewer')
```

The data used here is available upon request
Shape file with catchment boundaries for NVE gauging stations might be downloaded from
http://nedlasting.nve.no/gis/, and you should select the GIS dataset HYDROLOGISKE DATA->Totalnedbørfelt til målestasjon

Shape file with location of gauging stations might be downloaded from
http://nedlasting.nve.no/gis/, and you should select the GIS dataset HYDROLOGISKE DATA->Målseserier


## Preprosessing: Loading the GIS-data into the working environment
The shape-files with catchments boundaries are loaded into teh base environment, this includes:
* Norge : 	A shape file with outline of Norway. Loaded as a SpatialPolygonsDataFrame
* Mycatchments	A shape file with ploygons for all gauged catchments in Norway. Loaded as a SpatialPolygonsDataFrame
* MyStations  	A shape file with points for all hydrological measurement locations in Norway. Loaded as a SpatialPointsDataFrame
* forecasting_c A shape file with ploygons for the flood forecasting catchments. Loaded as a SpatialPolygonsDataFrame
* forecasting_s A shape file with points for the flood forecasting catchments. Loaded as a SpatialPointsDataFrame
* spar		A table with the catchment characteristics

Arguments: 
GiSfolder is the folder where the shape-files are stored
ccfile is a text-file with catchment characteristics.

```R
GisFolder<-'inst/GISDATA/'
ccfile<-"inst/feltparametre_flomstasjoner145.txt"
load_data_covers(GisFolder,ccfile)
```

# Loading example data to be plotted:
```R
corr_all<-read.table('correlation.txt')

```

# Plotting best correlation
```R
plot_map_points(cvalues=corr_all,v_index=8,c_index=10,cbins=c(-1.0,0.3,0.4,0.5,0.6,0.7,0.8,0.9),legtitle="Correlation")
```

#plotting month with best correlation
```R
plot_map_points(cvalues=corr_all,v_index=9,legtitle="Month")
```

# Plotting best Reff
```R
plot_map_points(cvalues=Reff_all,v_index=8,cbins=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7),legtitle="Reff")
```

#plotting month with best Reff
```
plot_map_points(cvalues=Reff_all,v_index=9,legtitle="Month")
```

# Plotting best csi
```
plot_map_points(cvalues=csi_all,v_index=8,cbins=c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7),legtitle="csi")
```

#plotting month with best csi
```
plot_map_points(cvalues=csi_all,v_index=9,legtitle="Month")
```

#plotting best roc
```R
plot_map_points(cvalues=roc_all,v_index=8,p_index=17,cbins=c(0.5,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1.0),legtitle="ROC-area")
```

#plotting month with best roc
```R
plot_map_points(cvalues=roc_all,v_index=9,legtitle="Month")
```

#plotting best crpss
```R
plot_map_points(cvalues=crpss_all,v_index=8,p_index=17,cbins=c(0.0,0.1,0.2,0.3,0.4,0.5),legtitle="CRPSS")
```

#plotting best crpss
```R
plot_map_points(cvalues=crpss_all,v_index=8,cbins=c(0.0,0.1,0.2,0.3,0.4,0.5),legtitle="CRPSS")
```

#plotting month with best crpss
```R
plot_map_points(cvalues=crpss_all,v_index=9,legtitle="Month")
```

#plotting best bss
```R
plot_map_points(cvalues=bss_all,v_index=8,p_index=17,cbins=c(0.0,0.1,0.2,0.3,0.4,0.5),legtitle="BSS")
```

#plotting best bss
```R
plot_map_points(cvalues=bss_all,v_index=8,cbins=c(0.0,0.1,0.2,0.3,0.4,0.5),legtitle="BSS")
```

#plotting month with best bss
```R
plot_map_points(cvalues=bss_all,v_index=9,legtitle="Month")
```
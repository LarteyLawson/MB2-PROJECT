##########################################################################################
#### Title: Investigating Potential Hydropower  sites in Ashanti Region
#### Location: Ashanti, Ghana
#### Author: Prince Lartey Lawson
#### Date: April 2022
##########################################################################################
##########################################################################################
#### Inspiration:
#### I chanced on a video by Turbulent Hydro ("https://www.youtube.com/watch?v=ciodGsZhxDY")
#### This video shows how a school in a rural region with about 700 people benefit from a 
#### rural region benefit from a hydropower generator designed and installed to fit in the 
#### relief nature of the school. I have seen many villages in the Ashanti Region that can 
#### benefit from such a technology but as always, the prior question starts with Where?. 
#### This R script shows the usage of packages like whitebox, and to answer the question of where.

### Study area: Ashanti Region, Ghana

### Investigating Potential Hydropower  sites in Ashanti Region
### 1.) cropping the downloaded Landsat scenes to an predefined extent
### 2.) calculating the NDWI for the years 1989 to 2018
### 3.) image classification, binary mpa of the lake extents
### 4.) change detection in water level per year (based on NDWI)
### 5.) extracting the water area in sqkm for each scene #### change them

### Dataset used: Gadm shapefiles, SRTM, TR55model od Ashanti region
### 

##########################################################################################


### SETTING WORKING DIRECTORY FOR PROJECT
getwd()
setwd("C:/Users/lynes/Desktop/RFinal Proj/")

### INSTALLING REQUIRED PACKAGES
install.packages("GADMTools")
install.packages("spData")
install.packages("whitebox")
install.packages("tmap")
whitebox::wbt_init()

### LOADING LIBRARIES
library(raster)
library(terra)
library(ggplot2)
library(GADMTools)
library(rosm)
library(sf)
library(sp)
library(terra)
library(dplyr)
library(spData)
library(tidyverse)
library(whitebox)
library(tmap)
library(tibble)
library(tidyr)

### CHECKING COUNTRY CODE OF GHANA
ccodes()

### DOWNLOADING GADM DATASET
map <- gadm_sf_loadCountries("GHA", level=1, basefile = "./")
### VARIABLE VISUALIZATION
gadm_plot(map) + theme_light()



### SUBSETTING AOI
Ashanti <- gadm_subset(map, level=1, regions="Ashanti")
### VARIABLE VISUALIZATION
gadm_plot(Ashanti)
### INVESTIGATING SRS
crs(Ashanti)
### DEFINING SRS
crs(Ashanti) <- CRS('+init=EPSG:4326')
### WRITING AOI TO .SHP
gadm_exportToShapefile(x = Ashanti, name = "AshRegion")
### REIMPORTING AS SF
AshRegion <- readOGR("AshRegion/AshRegion .shp")
AshRegion2 = st_as_sf(AshRegion)
### DEFINING SRS
crs(AshRegion) <- CRS('+init=EPSG:4326')
### VARIABLE VISUALIZATION
plot(AshRegion)
### INVESTIGATING SRS & CLASS
crs(AshRegion)
class(AshRegion)



### GETTING SRTM DATA
Elevation <- read.asciigrid("srtm_36_11.asc")
### VARIABLE VISUALIZATION
plot(Elevation)
### INVESTIGATING SRS & CLASS
crs(Elevation)
class(Elevation)
### RASTERIZING DATA & DEFINING SRS
srtm1 <- raster(Elevation)
projection(srtm1) <- CRS('+init=epsg:4326')
### WRITING RASTER TO TIF
writeRaster(srtm1, filename = "srtm1.tif", format="GTiff", overwrite=TRUE)
### RASTER DATA GEOPROCESSING 
cropoff <- crop(srtm1, AshRegion2)
plot(cropoff)
masked <- mask(cropoff, AshRegion2)
plot(masked)
### DEFINING SRS
projection(masked) <- CRS('+init=epsg:4326')
### WRITING RASTER TO TIF
writeRaster(masked, filename = "srtm1clipped.tif", format="GTiff", overwrite=TRUE)



### DEFINING AOI
AOI <- raster("srtm1clipped.tif")
### PREPARING INTERACTIVE MAP MODE FOR VISUALIZATION
tmap_mode("view")
### VARIABLE VISUALIZATION
tm_shape(AOI)+
  tm_raster(style = "cont", palette = "viridis", legend.show = TRUE)+
  tm_scale_bar()



### GENERATING HILLSHADE USING WHITEBOX
wbt_hillshade(dem = "srtm1clipped.tif",
              output = "HillshadeAOI.tif",
              azimuth = 115)                        

hillshade <- raster("HillshadeAOI.tif")
tm_shape(hillshade)+
  tm_raster(style = "cont", 
            palette = "-plasma", 
            legend.show = FALSE )+
  tm_scale_bar()



### GENERATING HILLSHADE USING TERRA
projection(AOI) <- CRS('+init=epsg:4326')
AOI2 <- projectRaster(AOI, crs="+init=EPSG:32630")
AOIslope <- terrain(AOI, opt="slope")
AOIaspect <- terrain(AOI, opt="aspect")
AOIhillshaded <- hillShade(AOIslope, AOIaspect, 130, 115)
### VARIABLE VISUALIZATION
tm_shape(AOIhillshaded)+
  tm_raster(style = "cont", 
            palette = "plasma", 
            legend.show = FALSE )+
  tm_scale_bar()



### BREACHING SURFACE DATA
wbt_breach_depressions_least_cost(dem = "srtm1clipped.tif",
                                  output = "srtm_breached.tif",
                                  dist = 200,
                                  fill = TRUE)      
### VARIABLE VISUALIZATION
tm_shape(raster("srtm_breached.tif"))+
  tm_raster(style = "cont", 
            palette = "plasma", 
            legend.show = FALSE)+
  tm_scale_bar()



### FILLING BREACHES
wbt_fill_depressions_wang_and_liu(
  dem = "srtm_breached.tif",
  output = "srtm1_filled_breached.tif")
### VARIABLE VISUALIZATION
tm_shape(raster("srtm1_filled_breached.tif"))+
  tm_raster(style = "cont", 
            palette = "plasma", 
            legend.show = FALSE)+
  tm_scale_bar()



### LOADING OUTPUT OF FILLING 
filled_breaches <- raster("srtm1_filled_breached.tif")
### VISUALIZING SURFACE CHANGES
difference <- AOI - filled_breaches
difference[difference == 0] <- NA
tm_shape(hillshade)+
  tm_raster(style = "cont",palette = "Greys", legend.show = FALSE)+
  tm_scale_bar()+
  tm_shape(difference)+
  tm_raster(style = "cont",legend.show = TRUE)+
  tm_scale_bar()



###### D8FLOW ACCUMULATION
wbt_d8_flow_accumulation(input = "srtm1_filled_breached.tif",
                         output = "D8FA.tif")
### DEFINING D8 RASTER
d8 <- raster("D8FA.tif")
### VARIABLE VISUALIZATION
tm_shape(hillshade)+
  tm_raster(style = "cont",palette = "Greys", legend.show = FALSE )+
  tm_shape(log(d8))+
  tm_raster(style = "cont", palette = "-PuOr", legend.show = TRUE, alpha = .8, midpoint = NA)+
  tm_scale_bar()



###### DINFINITY FLOW ACCUMULATION
wbt_d_inf_flow_accumulation("srtm1_filled_breached.tif",
                            "DinfFA.tif")
### DEFINING DINF RASTER
dinf <- raster("DinfFA.tif")
### VARIABLE VISUALIZATION
tm_shape(hillshade)+
  tm_raster(style = "cont",palette = "-Greys", legend.show = FALSE)+
  tm_shape(log(dinf))+
  tm_raster(style = "cont", palette = "YlGnBu", legend.show = TRUE )+
  tm_scale_bar()


### TOPOGRAPHIC WETNESS INDEX
wbt_d_inf_flow_accumulation(input = "srtm1_filled_breached.tif",
                            output = "DinfFAsca.tif",
                            out_type = "Specific Contributing Area")

wbt_slope(dem = "srtm1_filled_breached.tif",
          output = "demslope.tif",
          units = "degrees")

wbt_wetness_index(sca = "DinfFAsca.tif",
                  slope = "demslope.tif",
                  output = "TWI.tif")
### DEFINING TWI RASTER
twi <- raster("TWI.tif")
twi[twi > 0] <- NA
### VARIABLE VISUALIZATION
tm_shape(hillshade)+
  tm_raster(style = "cont",palette = "Greys", legend.show = FALSE)+
  tm_shape(twi)+
  tm_raster(style = "cont", palette = "YlGnBu", legend.show = TRUE, alpha = 0.5)+
  tm_scale_bar()


####### Downslope TWI
wbt_downslope_index(dem = "srtm1_filled_breached.tif",
                    output = "downslope_index.tif",
                    out_type = "tangent")
downslope_index <- raster("downslope_index.tif")
### DEFINING DINFFA RASTER
dinfFA <- raster("DinfFAsca.tif")
twid <- log(dinfFA / downslope_index)
twid[twid > 50] <- NA
### VARIABLE VISUALIZATION
tm_shape(hillshade)+
  tm_raster(style = "cont",palette = "Greys", legend.show = FALSE)+
  tm_shape(twid)+
  tm_raster(style = "cont", palette = "PuBu", legend.show = TRUE, alpha = 0.5)+
  tm_scale_bar()

### EXTRACTING STREAM NETWORKS
wbt_extract_streams(flow_accum = "D8FA.tif",
                    output = "raster_streams.tif",
                    threshold = 6000)
wbt_d8_pointer(dem = "srtm1_filled_breached.tif",
               output = "D8pointer.tif")

wbt_raster_streams_to_vector(streams = "raster_streams.tif",
                             d8_pntr = "D8pointer.tif",
                             output = "streams.shp")
### LOADING STREAM VECTOR TO R
streams <- st_read("streams.shp")
### VARIABLE VISUALIZATION
tm_shape(hillshade)+
  tm_raster(style = "cont",palette = "Greys", legend.show = FALSE)+
  tm_shape(streams)+
  tm_lines(col = "blue", size = 1.5 )+
  tm_scale_bar()

### IMPORTING A TR55 WATERDROPPATH (AUTOCAD CIVIL 3D FUNCTION ON DATA)
waterdrop <- st_read("TR55/waterdroppath_TR55.shp")
### INVESTIGATING DATA
crs(waterdrop)
head(waterdrop)
class(waterdrop)
summary(waterdrop2)

### CREATING NEW COLUMN
waterdrop2 <- waterdrop %>%
  add_column(Grade = NA)

### CATEGORIZING DF WIT RESPECT TO STEEPNESS (POTENTIAL ENEGRY)
waterdrop2$Grade <- ifelse(waterdrop2$Gradient < 0.25, 'Flat',
                            ifelse(waterdrop2$Gradient < 0.75, 'Gentle', 'Steep'))
### FACTORIZING DF TO LEVELS
waterdrop2$Grade <- as.factor(waterdrop2$Grade)

### DISTRIBUTION OF WATERDROP PATHS BY GRADES
ggplot(waterdrop2, aes(x = Droplength, y = Elevation ))+
  geom_point(aes(colour = Grade), alpha =.2)+
  geom_smooth(aes(fill = Grade), alpha = NA)

### FINAL MAP VISIUALIZATION
tm_shape(hillshade)+
  tm_raster(style = "cont", palette = "Greys", legend.show = FALSE)+
  tm_shape(waterdrop2)+
  tm_lines(col = "cyan", alpha = .5, group = waterdrop2$Grade)+
  tm_shape(streams)+
  tm_lines(col = "blue", size = 1.5 )+
  tm_scale_bar()



####


# Script created to do and spatial modeling and prediction of
# the Humboldt Penguins located in Chile.
# The data was acquired by Movebank directly from Dr. Klemens Pütz

library(raster)
library(rgdal)
library(maxnet)
library(dismo)
library(rJava)
library(ggplot2)
library(sf)
library(sp)
library(sdm)
library(usdm)
library(mapview)

#setwd('/home/diego/GITHUP_REPO/Spatial_Modeling_and_Prediction/')

# Set the projection that will be use in this project
r <- CRS("+proj=longlat +datum=WGS84")
p <- "+proj=longlat +datum=WGS84"

# Get the borders of Chile using GADM data
#Chile <- getData("GADM", country="Chile", level=0)

# Re-projection of Chile borders
#Chile <- spTransform(x = Chile, CRSobj = r)

# Penguins data in Shapefile format
h_penguins_2009 <- readOGR('shape_PTT/HumboldtPenguins_Punihuil_PTT.shp')

# Re-projection of the Penguins data in Shapefile
h_penguins_2009 <- spTransform(x = h_penguins_2009, CRSobj = r)

# Add a new column namely Penguins
h_penguins_2009$species <- 1

# The spatial data of the penguins is split in the different month we will test
# Remove the data without coodinate
h_penguins_march_2009 <- h_penguins_2009[h_penguins_2009$March == '1',]
h_penguins_march_2009 <- h_penguins_march_2009[,c('species')]
head(h_penguins_march_2009)

h_penguins_april_2009 <- h_penguins_2009[h_penguins_2009$April == '1',]
h_penguins_april_2009 <- h_penguins_april_2009[,c('species')]
head(h_penguins_april_2009)

h_penguins_may_2009 <- h_penguins_2009[h_penguins_2009$May == '1',]
h_penguins_may_2009 <- h_penguins_may_2009[,c('species')]
head(h_penguins_may_2009)

# The data is bind in a total data container
h_penguins_2009 <- bind(h_penguins_march_2009, h_penguins_april_2009, h_penguins_may_2009)
head(h_penguins_2009)
mapview(h_penguins_2009)

# The next lines it will call four list of raster data
bathymetry <- raster('Raster_data_PTT/bathymetry.tif')
names(bathymetry) <- c('bathymetry')

march_2009 <- list.files('Raster_data_PTT',
                    full.names = TRUE,
                    pattern = "March_2009.tif$")

april_2009 <- list.files('Raster_data_PTT',
                    full.names = TRUE,
                    pattern = "April_2009.tif$")

may_2009 <- list.files('Raster_data_PTT',
                    full.names = TRUE,
                    pattern = "May_2009.tif$")

### Data preparation to apply the Multicollinearity test by month
march_2009 <- stack(march_2009,bathymetry)
march_2009 <- projectRaster(march_2009, crs=p)

april_2009 <- stack(april_2009,bathymetry)
april_2009 <- projectRaster(april_2009, crs=p)

may_2009 <- stack(may_2009,bathymetry)
may_2009 <- projectRaster(may_2009, crs=p)

# Make the projection of the Bathymetry same as the others
bathymetry <- projectRaster(bathymetry, crs=p)

### Mean of three months data

Chlorophyll.a_2009 <- stack(march_2009[[1]], april_2009[[1]], may_2009[[1]])
Chlorophyll.a_2009 <- calc(Chlorophyll.a_2009, fun = mean)

Elevation_2009 <- stack(march_2009[[2]], april_2009[[2]], may_2009[[2]])
Elevation_2009 <- calc(Elevation_2009, fun = mean)

Salinity_2009 <- stack(march_2009[[3]], april_2009[[3]], may_2009[[3]])
Salinity_2009 <- calc(Salinity_2009, fun = mean)

Sea_Surface_2009 <- stack(march_2009[[4]], april_2009[[4]], may_2009[[4]])
Sea_Surface_2009 <- calc(Sea_Surface_2009, fun = mean)

U0_2009 <- stack(march_2009[[5]], april_2009[[5]], may_2009[[5]])
U0_2009 <- calc(U0, fun = mean)

V0_2009 <- stack(march_2009[[6]], april_2009[[6]], may_2009[[6]])
V0_2009 <- calc(V0_2009, fun = mean)

## Stack mean values
predictors_2009 <- stack(bathymetry ,Chlorophyll.a_2009, Elevation_2009, Salinity_2009, Sea_Surface_2009, U0_2009, V0_2009)

## Test of VIF and Multicollinearity of March data
vif(march_2009) # calculates vif for the variables in r
v1_march_2009 <- vifcor(march_2009, th=0.7) # identify collinear variables that should be excluded
v1_march_2009

re1_march_2009 <- exclude(march_2009,v1_march_2009) # exclude the collinear variables that were identified in
# the previous step
re1_march_2009

v2_march_2009 <- vifstep(march_2009, th=10) # identify collinear variables that should be excluded
v2_march_2009

re2_march_2009 <- exclude(march_2009, v2_march_2009) # exclude the collinear variables that were identified in
# the previous step
re2_march_2009

re3_march_2009 <- exclude(march_2009) # first, vifstep is called
re3_march_2009

## Test of VIF and Multicollinearity of April data
vif(april_2009) # calculates vif for the variables in r
v1_april_2009 <- vifcor(april_2009, th=0.7) # identify collinear variables that should be excluded
v1_april_2009

re1_april_2009 <- exclude(april_2009,v1_april_2009) # exclude the collinear variables that were identified in
# the previous step
re1_april_2009

v2_april_2009 <- vifstep(april_2009, th=10) # identify collinear variables that should be excluded
v2_april_2009

re2_april_2009 <- exclude(april_2009, v2_april_2009) # exclude the collinear variables that were identified in
# the previous step
re2_april_2009

re3_april_2009 <- exclude(april_2009) # first, vifstep is called
re3_april_2009

## Test of VIF and Multicollinearity of May data
vif(may_2009) # calculates vif for the variables in r
v1_may_2009 <- vifcor(may_2009, th=0.7) # identify collinear variables that should be excluded
v1_may_2009

re1_may_2009 <- exclude(may_2009,v1_may_2009) # exclude the collinear variables that were identified in
# the previous step
re1_may_2009

v2_may_2009 <- vifstep(may_2009, th=10) # identify collinear variables that should be excluded
v2_may_2009

re2_may_2009 <- exclude(may_2009, v2_may_2009) # exclude the collinear variables that were identified in
# the previous step
re2_may_2009

re3_may_2009 <- exclude(may_2009) # first, vifstep is called
re3_may_2009
##

## Test of VIF and Multicollinearity of Mean data
vif(predictors_2009) # calculates vif for the variables in r
v1_predictors_2009 <- vifcor(predictors_2009, th=0.7) # identify collinear variables that should be excluded
v1_predictors_2009

re1_predictors_2009 <- exclude(predictors_2009,v1_predictors_2009) # exclude the collinear variables that were identified in
# the previous step
re1_predictors_2009

v2_predictors_2009 <- vifstep(predictors_2009, th=10) # identify collinear variables that should be excluded
v2_predictors_2009

re2_predictors_2009 <- exclude(predictors_2009, v2_predictors_2009) # exclude the collinear variables that were identified in
# the previous step
re2_predictors_2009

re3_predictors_2009 <- exclude(predictors_2009) # first, vifstep is called
re3_predictors_2009
##

# Monthly raster brick which has passed the Multicollinearity test
predictors_march_2009 <- re3_march_2009
predictors_april_2009 <- re3_april_2009
predictors_may_2009 <- re3_may_2009
predictors_2009 <- re3_predictors_2009
names(predictors_2009) <- c('bathymetry','Chlorophyll.a','Elevation','Salinity','Sea_Surface','U0','V0')

# Plot the raster data to be sure if the mask process was well done
plot(predictors_2009)

################## MaxEnt

### MaxEnt by month
## March data
sdm_march_2009 <- maxent(predictors_march_2009, removeDuplicates=TRUE, 
                         h_penguins_march_2009, nbg=10000)
sdm_march_2009

prediction_march_2009 <- predict(sdm_march_2009, predictors_march_2009)
names(prediction_march_2009) <- c('prediction_march_2009')
plot(prediction_march_2009)
#plot(Chile, add=T)
points(h_penguins_march_2009, col='blue', pch=3)

## April data
sdm_april_2009 <- maxent(predictors_april_2009, removeDuplicates=TRUE, 
                         h_penguins_april_2009, nbg=10000)
sdm_april_2009

prediction_april_2009 <- predict(sdm_april_2009, predictors_april_2009)
names(prediction_april_2009) <- c('prediction_april_2009')
plot(prediction_april_2009)
#plot(Chile, add=T)
points(h_penguins_april_2009, col='blue', pch=3)

## May data
sdm_may_2009 <- maxent(predictors_may_2009, removeDuplicates=TRUE, h_penguins_may_2009, nbg=10000)
sdm_may_2009

prediction_may_2009 <- predict(sdm_may_2009, predictors_may_2009)
names(prediction_may_2009) <- c('prediction_may_2009')
plot(prediction_may_2009)
#plot(Chile, add=T)
points(h_penguins_may_2009, col='blue', pch=3)

### MaxEnt with all the data
sdm_2009 <- maxent(predictors_2009, removeDuplicates=TRUE, h_penguins_2009, nbg=10000)
sdm_2009

prediction_2009 <- predict(sdm_2009, predictors_2009)
names(prediction_2009) <- c('prediction_2009')
plot(prediction_2009)
#plot(Chile, add=T)
points(h_penguins_2009, col='blue', pch=3)

################# Kernel Area
library(adehabitatHR)

# converting
utmcoord<-spTransform(h_penguins_2009,CRS("+init=epsg:32718"))
#just to double check the transformation has occured
utmcoord
plot(utmcoord)
# Estimation of Kernel Home-Range
ud<-kernelUD(utmcoord)
image(ud)
ver95 <- getverticeshr(ud, 95, unout = "km2")
ver50 <- getverticeshr(ud, 50, unout = "km2")
plot(ver95)
plot(ver50)

## Example of estimation using LSCV
udbis <- kernelUD(utmcoord, h = "LSCV")
image(udbis)

## Compare the estimation with ad hoc and LSCV method
## for the smoothing parameter
cuicui1 <- kernel.area(ud) ## ad hoc
plot(cuicui1)
cuicui2 <- kernel.area(udbis) ## LSCV
plot(cuicui2)

## Diagnostic of the cross-validation
plotLSCV(udbis)

################# SDM analysis
library(sdm)

d_2009 <- sdmData(species~., h_penguins_2009, predictors = predictors_2009, bg = list(n=10000))
d_2009

m_2009 <- sdm(species~., d_2009, methods=c('glm', 'svm', 'rf'))
m_2009

# Output of the model
gui(m_2009)

p_2009 <- predict(m_2009, predictors_2009, 'Predictors2009.tif', overwrite=TRUE)

p_2009

en_2009 <- ensemble(m_2009, predictors_2009, 'ensemble2009.tif',
               setting=list(method='weighted',stat='TSS'))

# Replace NA's with 0 for raster data
en_2009[is.na(en_2009[])] <- 0

plot(en_2009)

mapview(en_2009)

# Prediction using data of 2019
# To begin with, it is necessary to load the data of the year 2019

bathymetry_2019 <- raster('Raster_data_PTT/bathymetry.tif')
names(bathymetry_2019) <- c('bathymetry_2019')

march_2019 <- list.files('Raster_data_2019',
                         full.names = TRUE,
                         pattern = "March_2019.tif$")

april_2019 <- list.files('Raster_data_2019',
                         full.names = TRUE,
                         pattern = "April_2019.tif$")

may_2019 <- list.files('Raster_data_2019',
                       full.names = TRUE,
                       pattern = "May_2019.tif$")

### Data preparation
march_2019 <- stack(march_2019,bathymetry_2019)
march_2019 <- projectRaster(march_2019, crs=p)

april_2019 <- stack(april_2019,bathymetry_2019)
april_2019 <- projectRaster(april_2019, crs=p)

may_2019 <- stack(may_2019,bathymetry_2019)
may_2019 <- projectRaster(may_2019, crs=p)

# Make the projection of the Bathymetry same as the others
bathymetry_2019 <- projectRaster(bathymetry_2019, crs=p)

### Mean of three months data

Chlorophyll.a_2019 <- stack(march_2019[[1]], april_2019[[1]], may_2019[[1]])
Chlorophyll.a_2019 <- calc(Chlorophyll.a_2019, fun = mean)

Elevation_2019 <- stack(march_2019[[2]], april_2019[[2]], may_2019[[2]])
Elevation_2019 <- calc(Elevation_2019, fun = mean)

Salinity_2019 <- stack(march_2019[[3]], april_2019[[3]], may_2019[[3]])
Salinity_2019 <- calc(Salinity_2019, fun = mean)

Sea_Surface_2019 <- stack(march_2019[[4]], april_2019[[4]], may_2019[[4]])
Sea_Surface_2019 <- calc(Sea_Surface_2019, fun = mean)

U0_2019 <- stack(march_2019[[5]], april_2019[[5]], may_2019[[5]])
U0_2019 <- calc(U0_2019, fun = mean)

V0_2019 <- stack(march_2019[[6]], april_2019[[6]], may_2019[[6]])
V0_2019 <- calc(V0_2019, fun = mean)

## Stack mean values
predictors_2019 <- stack(bathymetry_2019 ,Chlorophyll.a_2019, Elevation_2019, Salinity_2019, Sea_Surface_2019, U0_2019, V0_2019)
predictors_2019 <- projectRaster(predictors_2019, crs=p)
names(predictors_2019) <- c('bathymetry','Chlorophyll.a','Elevation','Salinity','Sea_Surface','U0','V0')

# Prediction of suitability data in the year 2019
p_2019 <- predict(m_2009, predictors_2019, 'Predictors2019.tif', overwrite=TRUE)

p_2019

en_2019 <- ensemble(m_2009, predictors_2019, 'ensemble2019.tif',
                    setting=list(method='weighted',stat='TSS'))

# Replace NA's with 0 for raster data
en_2019[is.na(en_2019[])] <- 0

plot(en_2019)

mapview(en_2019)

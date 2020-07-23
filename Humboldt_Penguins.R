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
library(sdm)
library(usdm)

setwd('/home/diego/GITHUP_REPO/Spatial_Modeling_and_Prediction')

# Set the projection that will be use in this project
r <- CRS("+proj=longlat +datum=WGS84")
p <- "+proj=longlat +datum=WGS84"

# Get the borders of Chile using GADM data
Chile <- getData("GADM", country="Chile", level=0)

# Re-projection of Chile borders
Chile <- spTransform(x = Chile, CRSobj = r)

# Penguins data in Shapefile format
h_penguins <- readOGR('/shape_PTT/HumboldtPenguins_Punihuil_PTT.shp')

# Re-projection of the Penguins data in Shapefile
h_penguins <- spTransform(x = h_penguins, CRSobj = r)

# The spatial data of the penguins is split in the different month we will test
h_penguins_march <- h_penguins[h_penguins$March == '1',]
h_penguins_april <- h_penguins[h_penguins$April == '1',]
h_penguins_may <- h_penguins[h_penguins$May == '1',]

# The data is bind in a total data container
h_penguins <- bind(h_penguins_march, h_penguins_april, h_penguins_may)

# Penguins data in CSV format
#h_penguins_csv <- read.csv('/home/diego/Desktop/2do_Semestre/Spatial_Modeling_and_Prediction_(04-GEO-MET1)/Humbolt_Penguins/HumboldtPenguins_Punihuil_PTT.csv')

# The next lines it will call four list of raster data
bathymetry <- stack('/Raster_data_PTT/bathymetry.tif')
names(bathymetry) <- c('bathymetry')

march <- list.files('/Raster_data_PTT',
                              full.names = TRUE,
                              pattern = "March_2009.tif$")

april <- list.files('/Raster_data_PTT',
                    full.names = TRUE,
                    pattern = "April_2009.tif$")

may <- list.files('/Raster_data_PTT',
                    full.names = TRUE,
                    pattern = "May_2009.tif$")

march <- stack(march,bathymetry)
march <- projectRaster(march, crs=p)

## Test of VIF and Multicollinearity of March data

vif(march) # calculates vif for the variables in r
v1_march <- vifcor(march, th=0.9) # identify collinear variables that should be excluded
v1_march
re1_march <- exclude(march,v1_march) # exclude the collinear variables that were identified in
# the previous step
re1_march
v2_march <- vifstep(march, th=10) # identify collinear variables that should be excluded
v2_march
re2_march <- exclude(march, v2_march) # exclude the collinear variables that were identified in
# the previous step
re2_march
re3_march <- exclude(march) # first, vifstep is called
re3_march

##

april <- stack(april,bathymetry)
april <- projectRaster(april, crs=p)

## Test of VIF and Multicollinearity of April data

vif(april) # calculates vif for the variables in r
v1_april <- vifcor(april, th=0.9) # identify collinear variables that should be excluded
v1_april
re1_april <- exclude(april,v1_april) # exclude the collinear variables that were identified in
# the previous step
re1_april
v2_april <- vifstep(april, th=10) # identify collinear variables that should be excluded
v2_april
re2_april <- exclude(april, v2_april) # exclude the collinear variables that were identified in
# the previous step
re2_april
re3_april <- exclude(april) # first, vifstep is called
re3_april

##

may <- stack(may,bathymetry)
may <- projectRaster(may, crs=p)

## Test of VIF and Multicollinearity of May data

vif(may) # calculates vif for the variables in r
v1_may <- vifcor(may, th=0.9) # identify collinear variables that should be excluded
v1_may
re1_may <- exclude(may,v1_may) # exclude the collinear variables that were identified in
# the previous step
re1_may
v2_may <- vifstep(may, th=10) # identify collinear variables that should be excluded
v2_may
re2_may <- exclude(may, v2_may) # exclude the collinear variables that were identified in
# the previous step
re2_may
re3_may <- exclude(may) # first, vifstep is called
re3_may

##
# Monthly raster brick which has passed the Multicollinearity test
predictors_march <- re3_march
predictors_april <- re3_april
predictors_may <- re3_may

# Regroup the raster which has passed the Multicollinearity test in one stack
predictors <- stack(re3_march, re3_april, re3_may)

# Remove the repeated layer bathymetry that it was used in the Multicollinearity 
#test before
predictors <- dropLayer(predictors,c(14,21))

# Masked the inland area
masked_march <- mask(predictors_march, Chile)
masked_april <- mask(predictors_april, Chile)
masked_may <- mask(predictors_may, Chile)
masked <- mask(predictors, Chile)

# Plot the mask to observe if it is works
plot(masked, 7)

# Remove the inland data that could affect the prediction
predictors_march[!is.na(masked)] <- NA
predictors_april[!is.na(masked)] <- NA
predictors_may[!is.na(masked)] <- NA
predictors[!is.na(masked)] <- NA

# Plot the raster data to be sure if the mask process was well done
plot(predictors)

################## MaxEnt

### MaxEnt by month
## March data
sdm_march <- maxent(predictors_march, h_penguins_march)
sdm_march

prediction_march <- predict(sdm_march, predictors_march)
plot(prediction_march)
plot(Chile, add=T)
points(h_penguins_march, col='blue', pch=3)

## April data
sdm_april <- maxent(predictors_april, h_penguins_april)
sdm_april

prediction_april <- predict(sdm_april, predictors_april)
plot(prediction_april)
plot(Chile, add=T)
points(h_penguins_april, col='blue', pch=3)

## May data
sdm_may <- maxent(predictors_may, h_penguins_may)
sdm_may

prediction_may <- predict(sdm_may, predictors_may)
plot(prediction_may)
plot(Chile, add=T)
points(h_penguins_may, col='blue', pch=3)

### MaxEnt with all the data
sdm <- maxent(predictors, h_penguins)
sdm

prediction <- predict(sdm, predictors)
plot(prediction)
plot(Chile, add=T)
points(h_penguins, col='blue', pch=3)
#ggsave(filename = "prediction.jpeg", width = 20, height = 40, units = "cm")










################# Kernel Area

#library(adehabitatHR)

#longlatcoor<-SpatialPoints(cbind(h_penguins_csv$Longitude,h_penguins_csv$Latitude), proj4string=CRS("+proj=longlat +datum=WGS84"))
#longlatcoor
#plot(longlatcoor)
# converting
#utmcoord<-spTransform(longlatcoor,CRS("+proj=utm +south +zone=18 +datum=WGS84"))
#utmcoord #just to double check the transformation has occured
#kud<-kernelUD(utmcoord, h="href", grid=100, hlim = c(1, 100), kern = c("bivnorm"), extent = 0.5)
#plot(kud)
#area <- kernel.area(kud,percent=seq(50,95,by=5),unin=("m"),unout=("km2"))
#plot(area)


################### SDM package
#predictors_csv <- extract(predictors, h_penguins)
#df <- as.data.frame(cbind(h_penguins, predictors_csv))
#df <- df %>% select(1:30)

#fractionTraining   <- 0.70
#fractionValidation <- 0.30
#sampleSizeTraining   <- floor(fractionTraining   * nrow(df))
#sampleSizeValidation <- floor(fractionValidation * nrow(df))
#indicesTraining    <- sort(sample(seq_len(nrow(df)), size=sampleSizeTraining))
#indicesNotTraining <- setdiff(seq_len(nrow(df)), indicesTraining)
#indicesValidation  <- sort(sample(indicesNotTraining, size=sampleSizeValidation))
#dfTraining   <- df[indicesTraining, ]
#dfValidation <- df[indicesValidation, ]


#d <- sdmData(sex~bathymetry+Chlorophyll.a_April_2009
#             +Chlorophyll.a_March_2009+Chlorophyll.a_May_2009
#             +Elevation_April_2009+Elevation_March_2009+Elevation_May_2009
#             +Salinity_April_2009+Salinity_March_2009+Salinity_May_2009
#             +Sea_Surface_Temp_April_2009+Sea_Surface_Temp_March_2009
#             +Sea_Surface_Temp_May_2009+U0_April_2009+U0_March_2009
#             +U0_May_2009+V0_April_2009+V0_March_2009+V0_May_2009
#             ,train=dfTraining,test=dfValidation)

#m1 <- sdm(h_penguins_csv~predictors_csv,data=d,methods=c('glm','gbm'))
#m1
#m2 <- sdm(h_penguins_csv~predictors_csv,data=d,methods=c('svm'))
#m2
#m <- m1 + m2
#m

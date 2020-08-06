# Script created to do and spatial modeling and prediction of
# the Humboldt Penguins located in Chile.
# The data was acquired by Movebank directly from Dr. Klemens Pütz

library(dismo)
library(raster)
library(maptools)

# Set the projection that will be use in this project
r <- CRS("+proj=longlat +datum=WGS84")
p <- "+proj=longlat +datum=WGS84"

# Get the borders of Chile using GADM data
Chile <- raster::getData("GADM", country="Chile", level=0)

# Re-projection of Chile borders
Chile <- spTransform(x = Chile, CRSobj = r)

# Predictor data in 2009
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
U0_2009 <- calc(U0_2009, fun = mean)

V0_2009 <- stack(march_2009[[6]], april_2009[[6]], may_2009[[6]])
V0_2009 <- calc(V0_2009, fun = mean)

## Stack mean values
predictors_2009 <- stack(bathymetry ,Chlorophyll.a_2009, Elevation_2009, 
                         Salinity_2009, Sea_Surface_2009, U0_2009, V0_2009)

predictors_2009 <- na.omit(predictors_2009)

# Name of each layer
names(predictors_2009) <- c('bathymetry','Chlorophyll.a','Elevation',
                            'Salinity','Sea_Surface','U0','V0')

# Extention of the study area
ext <- extent(predictors_2009[[1]])

h_penguins <- read.csv('HumboldtPenguins_Punihuil_PTT.csv',  header=TRUE,  sep=',')
h_penguins <- h_penguins[,8:9]
presvals <- extract(predictors_2009, h_penguins)
presvals <- na.omit(presvals)
set.seed(0)
backgr <- randomPoints(predictors_2009, 500)
absvals <- extract(predictors_2009, backgr)
absvals <- na.omit(absvals)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmdata <- data.frame(cbind(pb, rbind(presvals, absvals)))

# Standardize the name of the variable in a stack file
pred_nf <- predictors_2009

# We make a training and a testing set.
set.seed(0)
group <- kfold(h_penguins, 5)
pres_train <- h_penguins[group != 1, ]
pres_test <- h_penguins[group == 1, ]

# We continue with Background data for training and a testing set
set.seed(10)
backg <- randomPoints(pred_nf, n=1000, ext=ext, extf = 1.25)
colnames(backg) = c('Longitude', 'Latitude')
group <- kfold(backg, 5)
backg_train <- backg[group != 1, ]
backg_test <- backg[group == 1, ]

# Now we take a look of the data generated
r <- raster(pred_nf, 1)
plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE)
plot(ext, add=TRUE, col='red', lwd=2)
points(backg_train, pch='-', cex=0.5, col='yellow')
points(backg_test, pch='-',  cex=0.5, col='black')
points(pres_train, pch= '+', col='green')
points(pres_test, pch='+', col='blue')

# Extraction of the environmental data values
train <- rbind(pres_train, backg_train)
pb_train <- c(rep(1, nrow(pres_train)), rep(0, nrow(backg_train)))
envtrain <- extract(pred_nf, train)
envtrain <- data.frame( cbind(pa=pb_train, envtrain) )
envtrain[,'biome'] = factor(envtrain[,'biome'], levels=1:14)
head(envtrain)
testpres <- data.frame( extract(pred_nf, pres_test) )
testbackg <- data.frame( extract(pred_nf, backg_test) )

################## MaxEnt
library(maxnet)
library(dismo)
library(rJava)

xm <- maxent(pred_nf, pres_train, removeDuplicates=TRUE, progress='text')
plot(xm)

# A response plot
response(xm)

e <- evaluate(pres_test, backg_test, xm, pred_nf)
e
px <- predict(pred_nf, xm, ext=ext, na.action=na.exclude,
              'MaxEnt2009.tif', overwrite=TRUE)
px

par(mfrow=c(1,2))
plot(px, main='Maxent, raw values')
plot(Chile, add=TRUE, border='dark grey')
tr <- threshold(e, 'spec_sens')
plot(px > tr, main='presence/absence')
plot(Chile, add=TRUE, border='dark grey')
points(pres_train, pch='+')

################## Random Forest
library(randomForest)

# Preparing the model
model <- pa ~ bathymetry + Chlorophyll.a + Elevation + Salinity + Sea_Surface + U0 + V0

rf1 <- randomForest(model, data=envtrain, na.action=na.exclude)

# Evaluation of test and background data
erf <- evaluate(testpres, testbackg, rf1)
erf

pr <- predict(pred_nf, rf1, na.action=na.exclude, 
              ext=ext, 'RandomForest2009.tif', overwrite=TRUE)

par(mfrow=c(1,2))
plot(pr, main='Random Forest prediction/ Penguins data 2009')
plot(Chile, add=TRUE, border='dark grey')
tr <- threshold(erf, 'spec_sens')
plot(pr > tr, main='presence/absence')
plot(Chile, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backg_train, pch='-', cex=0.25)

################# Prediction using data of 2019
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
predictors_2019 <- stack(bathymetry_2019 ,Chlorophyll.a_2019, Elevation_2019, 
                         Salinity_2019, Sea_Surface_2019, U0_2019, V0_2019)
predictors_2019 <- projectRaster(predictors_2019, crs=p)

predictors_2019 <- na.omit(predictors_2019)

names(predictors_2019) <- c('bathymetry','Chlorophyll.a','Elevation','Salinity','Sea_Surface','U0','V0')

############################## Prediction of suitability data in the year 2019

# using MaxEnt
mx_2019 <- predict(predictors_2019, xm, na.action=na.exclude, 
                   ext=ext, 'MaxEnt2019.tif', overwrite=TRUE)

mx_2019

par(mfrow=c(1,2))
plot(mx_2019, main='Maxent prediction/ Penguins data 2019')
plot(Chile, add=TRUE, border='dark grey')
tr <- threshold(e, 'spec_sens')
plot(mx_2019 > tr, main='presence/absence')
plot(Chile, add=TRUE, border='dark grey')
points(pres_train, pch='+')


# using Random Forest prediction
rf_2019 <- predict(predictors_2019, rf1, na.action=na.exclude, 
                   ext=ext, 'RandomForest2019.tif', overwrite=TRUE)

rf_2019

par(mfrow=c(1,2))
plot(rf_2019, main='Random Forest prediction/ Penguins data 2019')
plot(Chile, add=TRUE, border='dark grey')
tr <- threshold(erf, 'spec_sens')
plot(rf_2019 > tr, main='presence/absence')
plot(Chile, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backg_train, pch='-', cex=0.25)

# Script created to do and spatial modeling and prediction
# Humboldt Penguins located in Chile.
# The data was acquired by Movebank directly from Dr. Klemens Pütz

library(sdm)
library(usdm)
library(dismo)
library(dplyr)
library(raster)
library(ggplot2)
library(mapview)

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

### Data preparation to apply the Multi-collinearity test by month
march_2009 <- stack(march_2009,bathymetry) %>% projectRaster(crs=p)

april_2009 <- stack(april_2009,bathymetry) %>% projectRaster(crs=p)

may_2009 <- stack(may_2009,bathymetry) %>% projectRaster(crs=p)

### Mean of three months data and clean NA values

Chlorophyll.a_2009 <- stack(march_2009[[1]], april_2009[[1]], may_2009[[1]]) %>% 
  calc(fun = mean) %>% 
  na.omit()

Elevation_2009 <- stack(march_2009[[2]], april_2009[[2]], may_2009[[2]]) %>% 
  calc(fun = mean) %>% 
  na.omit()

Salinity_2009 <- stack(march_2009[[3]], april_2009[[3]], may_2009[[3]]) %>% 
  calc(fun = mean) %>% 
  na.omit()

Sea_Surface_2009 <- stack(march_2009[[4]], april_2009[[4]], may_2009[[4]]) %>% 
  calc(fun = mean) %>% 
  na.omit()

U0_2009 <- stack(march_2009[[5]], april_2009[[5]], may_2009[[5]]) %>% 
  calc(fun = mean) %>% 
  na.omit()

V0_2009 <- stack(march_2009[[6]], april_2009[[6]], may_2009[[6]]) %>% 
  calc(fun = mean) %>% 
  na.omit()

bathymetry_2009 <- raster('Raster_data_PTT/bathymetry.tif') %>% projectRaster(crs=p)

## Stack mean values
predictors_2009 <- stack(bathymetry_2009 ,Chlorophyll.a_2009, Elevation_2009, 
                         Salinity_2009, Sea_Surface_2009, U0_2009, V0_2009) %>% 
  na.omit()

names(predictors_2009) <- c('bathymetry','Chlorophyll.a','Elevation',
                            'Salinity','Sea_Surface','U0','V0')

################ Test of VIF and Multicollinearity of March data
# calculates vif for the variables in r
vif(march_2009)
# identify collinear variables that should be excluded
v1_march_2009 <- vifcor(march_2009, th=0.7)
v1_march_2009

# exclude the collinear variables that were identified in
# the previous step
re1_march_2009 <- exclude(march_2009,v1_march_2009)
re1_march_2009

# identify collinear variables that should be excluded
v2_march_2009 <- vifstep(march_2009, th=10)
v2_march_2009

# exclude the collinear variables that were identified in
# the previous step
re2_march_2009 <- exclude(march_2009, v2_march_2009)
re2_march_2009

# first, vifstep is called
re3_march_2009 <- exclude(march_2009)
re3_march_2009

################ Test of VIF and Multicollinearity of April data
# calculates vif for the variables in r
vif(april_2009)
# identify collinear variables that should be excluded
v1_april_2009 <- vifcor(april_2009, th=0.7)
v1_april_2009

# exclude the collinear variables that were identified in
# the previous step
re1_april_2009 <- exclude(april_2009,v1_april_2009)
re1_april_2009

# identify collinear variables that should be excluded
v2_april_2009 <- vifstep(april_2009, th=10)
v2_april_2009

# exclude the collinear variables that were identified in
# the previous step
re2_april_2009 <- exclude(april_2009, v2_april_2009)
re2_april_2009

# first, vifstep is called
re3_april_2009 <- exclude(april_2009)
re3_april_2009

################ Test of VIF and Multicollinearity of May data
# calculates vif for the variables in r
vif(may_2009)
# identify collinear variables that should be excluded
v1_may_2009 <- vifcor(may_2009, th=0.7)
v1_may_2009

# exclude the collinear variables that were identified in
# the previous step
re1_may_2009 <- exclude(may_2009,v1_may_2009)
re1_may_2009

# identify collinear variables that should be excluded
v2_may_2009 <- vifstep(may_2009, th=10)
v2_may_2009

# exclude the collinear variables that were identified in
# the previous step
re2_may_2009 <- exclude(may_2009, v2_may_2009)
re2_may_2009

re3_may_2009 <- exclude(may_2009) # first, vifstep is called
re3_may_2009

################ Test of VIF and Multi-collinearity of Mean data
# calculates vif for the variables in r
vif(predictors_2009)
# identify collinear variables that should be excluded
v1_predictors_2009 <- vifcor(predictors_2009, th=0.7)
v1_predictors_2009

# exclude the collinear variables that were identified in
# the previous step
re1_predictors_2009 <- exclude(predictors_2009,v1_predictors_2009)
re1_predictors_2009

# identify collinear variables that should be excluded
v2_predictors_2009 <- vifstep(predictors_2009, th=10)
v2_predictors_2009

# exclude the collinear variables that were identified in
# the previous step
re2_predictors_2009 <- exclude(predictors_2009, v2_predictors_2009)
re2_predictors_2009

# first, vifstep is called
re3_predictors_2009 <- exclude(predictors_2009)
re3_predictors_2009

################## End Multi-collinearity and begging of raster stack
# Monthly raster brick which has passed the Multicollinearity test
predictors_march_2009 <- re3_march_2009 %>% na.omit()
predictors_april_2009 <- re3_april_2009 %>% na.omit()
predictors_may_2009 <- re3_may_2009 %>% na.omit()
predictors_2009 <- re3_predictors_2009 %>% na.omit()
names(predictors_2009) <- c('bathymetry','Chlorophyll.a','Elevation',
                            'Salinity','Sea_Surface','U0','V0')

# Extention of the study area
ext <- extent(predictors_2009[[1]])

h_penguins <- read.csv('HumboldtPenguins_Punihuil_PTT.csv', header=TRUE,  sep=',')
h_penguins <- h_penguins[,8:9]
presvals <- extract(predictors_2009, h_penguins) %>% na.omit()

set.seed(0)
backgr <- randomPoints(predictors_2009, 500)
absvals <- extract(predictors_2009, backgr) %>% na.omit()

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
head(envtrain)
testpres <- data.frame( extract(pred_nf, pres_test) )
testbackg <- data.frame( extract(pred_nf, backg_test) )

################## Random Forest
library(randomForest)
library(maxnet)
library(dismo)
library(rJava)

# Preparing the model
model <- pa ~ bathymetry + Chlorophyll.a + 
  Elevation + Salinity + Sea_Surface + U0 + V0

# Clean NA data from envtrain
envtrain <- na.omit(envtrain)

# RandomForest implements Breiman's random forest algorithm
rf1 <- randomForest(model, data=envtrain, na.action=na.exclude)
rf1

# variable importance
varImpPlot(rf1, sort=TRUE, scale=TRUE, main= 'Variable Importance')

# Calculation and plot of the Dependence variables used
imp <- importance(rf1)
impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
op <- par(mfrow=c(2, 4))
for (i in seq_along(impvar)) {
  partialPlot(rf1, envtrain, impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]),
              ylim=c(0, 1))
}
par(op)

# Variables used in a random forest
varUsed(rf1, by.tree=FALSE, count=TRUE)

# Evaluation of test and background data
erf <- evaluate(testpres, testbackg, rf1)
erf

# Make a Raster object with predictions from a fitted model object
pr <- predict(pred_nf, rf1, na.action=na.exclude, 
              ext=ext, 'Products/RandomForest2009.tif', 
              overwrite=TRUE) %>% projectRaster(crs=p)
pr
pr[pr < 0.4] <- NA

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
bathymetry_2019 <- raster('Raster_data_2019/bathymetry.tif') %>% projectRaster(crs=p)

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

############### Data preparation
march_2019 <- stack(march_2019) %>% projectRaster(crs=p)

april_2019 <- stack(april_2019) %>% projectRaster(crs=p)

may_2019 <- stack(may_2019) %>% projectRaster(crs=p)

### Mean of three months data

Chlorophyll.a_2019 <- stack(march_2019[[1]], april_2019[[1]], may_2019[[1]]) %>% 
  calc(fun = mean) %>% 
  na.omit()

Elevation_2019 <- stack(march_2019[[2]], april_2019[[2]], may_2019[[2]]) %>% 
  calc(fun = mean) %>% 
  na.omit()

Salinity_2019 <- stack(march_2019[[3]], april_2019[[3]], may_2019[[3]]) %>% 
  calc(fun = mean) %>% 
  na.omit()

Sea_Surface_2019 <- stack(march_2019[[4]], april_2019[[4]], may_2019[[4]]) %>% 
  calc(fun = mean) %>% 
  na.omit()

U0_2019 <- stack(march_2019[[5]], april_2019[[5]], may_2019[[5]]) %>% 
  calc(fun = mean) %>% 
  na.omit()

V0_2019 <- stack(march_2019[[6]], april_2019[[6]], may_2019[[6]]) %>% 
  calc(fun = mean) %>% 
  na.omit()

## Stack mean values
predictors_2019 <- stack(bathymetry_2019 ,Chlorophyll.a_2019, Elevation_2019, 
                         Salinity_2019, Sea_Surface_2019, U0_2019, V0_2019) %>% 
                        na.omit()

names(predictors_2019) <- c('bathymetry','Chlorophyll.a','Elevation',
                            'Salinity','Sea_Surface','U0','V0')

############################## Prediction of suitability data in the year 2019
# using Random Forest prediction
rf_2019 <- predict(predictors_2019, rf1, na.action=na.exclude, 
                   ext=ext, 'Products/RandomForest2019.tif', 
                   overwrite=TRUE) %>% projectRaster(crs=p)

rf_2019
rf_2019[rf_2019 < 0.4] <- NA

par(mfrow=c(1,2))
plot(rf_2019, main='Random Forest prediction/ Penguins data 2019')
plot(Chile, add=TRUE, border='dark grey')
tr <- threshold(erf, 'spec_sens')
plot(rf_2019 > tr, main='presence/absence')
plot(Chile, add=TRUE, border='dark grey')
points(pres_train, pch='+')
points(backg_train, pch='-', cex=0.25)
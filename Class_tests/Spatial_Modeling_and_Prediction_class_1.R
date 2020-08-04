# height vs. weight pred

hw <- read.csv("/home/diego/Desktop/2do_Semestre/Spatial_Modeling_and_Prediction_(04-GEO-MET1)/weight-height.csv")
head(hw)
summary(hw)

#install.packages("measurements", dependencies = T)
library(measurements)
hw2<- data.frame(Gender=hw$Gender, Weight=conv_unit(hw$Weight,"lbs","kg"), Height=conv_unit(hw$Height,"inch","cm"))

head(hw2)
summary(hw2)

plot(hw2$Height,hw2$Weight)

library(dplyr)

dplyr::sample_n(hw2, 10)

summary(filter(hw2, Gender=="Female"))
summary(filter(hw2, Gender=="Male"))

boxplot(filter(hw2, Gender=="Female")$Weight,filter(hw2, Gender=="Male")$Weight, notch = T)
boxplot(filter(hw2, Gender=="Female")$Height,filter(hw2, Gender=="Male")$Height, notch = T)



shapiro.test(hw2$Weight) #Does not works because our sample is over 5000

shapiro.test(dplyr::sample_n(hw2,5000)$Weight)
shapiro.test(dplyr::sample_n(hw2,5000)$Height)

plot(density(hw2$Weight)) # there are not normal distribution cus the gender are mixed
plot(density(hw2$Height))


plot(density(filter(hw2, Gender=="Female")$Weight), col="red")
plot(density(filter(hw2, Gender=="Male")$Weight),col="blue") # does not work with add=T


plot(density(filter(hw2, Gender=="Female")$Weight), col="red")
lines(density(filter(hw2, Gender=="Male")$Weight),col="blue")

plot(density(filter(hw2, Gender=="Female")$Height), col="red")
lines(density(filter(hw2, Gender=="Male")$Height),col="blue")


shapiro.test(dplyr::sample_n(filter(hw2,Gender=="Female"),5000)$Weight)
shapiro.test(dplyr::sample_n(filter(hw2,Gender=="Male"),5000)$Weight)

shapiro.test(dplyr::sample_n(filter(hw2,Gender=="Female"),5000)$Height)
shapiro.test(dplyr::sample_n(filter(hw2,Gender=="Male"),5000)$Height)


hw2.male <- filter(hw2,Gender=="Male")
summary(hw2.male)

hw.lm <- lm(formula = Weight ~ Height, data=hw2.male)
summary(hw.lm)

hw.new <- data.frame(name=c("Jakob","Diego","Marius", "James", "Giovanni"),
                     Height = c(181,179,180,186,183))

hw.lm.p <- predict(object = hw.lm, newdata = hw.new)

pred.weight <- data.frame(hw.new$name,
                          weight.pred = hw.lm.p)   # dataframe of new data

pred.weight

hw.new.2 <- data.frame(Height=hw.new,Weight=pred.weight)

pred.hw.p<-predict(object = hw.lm,     # The regression model
                   newdata = hw.new,   # dataframe of new data
                   interval = "prediction")

pred.hw.c<-predict(object = hw.lm,     # The regression model
                   newdata = hw.new,   # dataframe of new data
                   interval = "confidence")

plot(hw2$Height,hw2$Weight)

matplot(hw.new$Height, cbind(pred.hw.c, pred.hw.p[,-1]),
        lty = c(3), type = "l", ylab = "predicted weight",add=T)


# ggplot2

library(ggplot2)
#install.packages("cowplot", dependencies = T)
library(cowplot)


p1 <- ggplot(hw2, aes(x = Height)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density..),
               geom = "ribbon",
               fill = "blue", colour = "gray", alpha = 0.6,
               position = "identity") +
  facet_grid(. ~ Gender,  scales="free") +
  coord_flip()

p1
p2 <- ggplot(hw2, aes(x = Weight)) +
  stat_density(aes(ymax = ..density..,  ymin = -..density..),
               geom = "ribbon",
               fill = "green", colour = "gray", alpha = 0.6,
               position = "identity") +
  facet_grid(. ~ Gender,  scales="free") +
  coord_flip()


p1p2<-plot_grid(p1, p2, labels = c("A", "B"), align = "h")

save_plot("plot2by2.png", p1p2,
          ncol = 2, 
          nrow = 1, 
          base_aspect_ratio = 1.3
)

library(rgdal)
library(raster)

occ <- readOGR("/home/diego/Desktop/2do_Semestre/Spatial_Modeling_and_Prediction_(04-GEO-MET1)/occurence.gpkg")

class(occ)
summary(occ)
plot(occ)

bui <- readOGR("/home/diego/Desktop/2do_Semestre/Spatial_Modeling_and_Prediction_(04-GEO-MET1)/campus_buildings.gpkg")

plot(bui)

plot(occ[occ$students==1,], col = "blue", pch = 16, add = T)
plot(occ[occ$students==0,], col = "red", pch = 16, add = T)

# Create an empty raster
r <- raster(bui, ncol = 100, nrows = 100)
# Populate the raster
rr.0 <- rasterize(bui, r, progress = "text")

rr.0.d <-  distance(rr.0)
preds <- rr.0.d
plot(rr.0.d)

#install.packages("sdm", dependencies = T)
library(sdm)

d <- sdmData(formula = students~layer, train = occ, predictors = preds)

d

m1 <- sdm(students~., data = d, methods = c("glm", "svm"))

p1 <- predict(m1, newdata=preds, filename="sdm_preds_1.grd", overwrite=T)

plot(p1)

rr <- rasterize(bui, r, progress = "Text", field = "id")
plot(rr)

# Separate the information for the different buildings
rr.1 <- rr == 1
rr.1[rr.1==0] <- NA
plot(rr.1)

rr.2 <- rr == 2
rr.2[rr.2==0] <- NA
plot(rr.2)

rr.3 <- rr == 3
rr.3[rr.3==0] <- NA
plot(rr.3)

# Calcule distances for the next buildings
rr.1.d <- distance(rr.1)
plot(rr.1.d)

rr.2.d <- distance(rr.2)
plot(rr.2.d)

rr.3.d <- distance(rr.3)
plot(rr.3.d)

preds <- stack(rr.1.d, rr.2.d, rr.3.d)

plot(preds)

preds

# Spatial prediction 2


d <- sdmData(formula = students~layer.1+layer.2+layer.3, train = occ, predictors = preds)

m1 <- sdm(students~., data = d, methods = c("glm", "svm"))

p1 <- predict(m1, newdata=preds, filename="sdm_preds_2.grd", overwrite=T)

plot(p1[[1]])
plot(bui, add=T)

occ.10h <- occ[occ$time==10,]
occ.13h <- occ[occ$time==13,]
occ.22h <- occ[occ$time==22,]

plot(occ.10h)


d.10h <- sdmData(formula = students~layer.1+layer.2+layer.3, train = occ.10h, predictors = preds)
d.13h <- sdmData(formula = students~layer.1+layer.2+layer.3, train = occ.13h, predictors = preds)
d.22h <- sdmData(formula = students~layer.1+layer.2+layer.3, train = occ.22h, predictors = preds)

m.10h <- sdm(students~., data = d.10h, methods = c("glm", "svm"))
m.13h <- sdm(students~., data = d.13h, methods = c("glm", "svm"))
m.22h <- sdm(students~., data = d.22h, methods = c("glm", "svm"))

p.10h <- predict(m.10h, newdata=preds)
p.13h <- predict(m.13h, newdata=preds)
p.22h <- predict(m.22h, newdata=preds)

p.time <- stack(p.10h,p.13h,p.22h)

plot(p.time,1)

plotRGB(p.time,1,2,3, stretch="lin")

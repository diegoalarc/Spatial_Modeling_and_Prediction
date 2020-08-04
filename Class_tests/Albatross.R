#install.packages("move")
#install.packages("ggmap")
#install.packages("mapproj")
#install.packages("RStoolbox")
#install.packages("sf")
#remotes::install_github("r-spatial/rgee")

library(move)
library(lubridate)
library(sp)
library(raster)
library(rgdal)
library(ggmap)
library(mapproj)

login <- movebankLogin(username="diegoalarc", password="Dv41413la")
Albatrosses <- getMovebankData(study="Galapagos Albatrosses", login=login)

head(Albatrosses)
slotNames(Albatrosses)

# Here are the counts of observations per Albatrosses:
table(Albatrosses@trackId)
# Bounding box 
Albatrosses@bbox

# A basic plot of the Albatrosses data:
plot(Albatrosses)

# Convert a move object to a data frame.
Albatrosses.df <- as(Albatrosses, "data.frame")
str(Albatrosses.df)

basemap <- get_map(location = Albatrosses@bbox, zoom = 8, maptype = "hybrid")

ggmap(basemap) + 
  geom_path(data = Albatrosses.df, mapping = aes(x = location_long, y = location_lat, col = trackId), alpha=.7, size=1) + 
  coord_map() + scale_colour_hue(l = 60) + 
  labs(x = "Longitude", y = "Latitude") + ggtitle("Waved albatrosses were tracked during breeding and non-breeding periods between 2008 and 2010.") 

# spread tracking and modeling of covid-19
###########
setwd("D:/covid-19/") # as a working directory to store, prepare and analyse data

###########
# load of libraries to process and animate the spread
library(dplyr) # prepare and process data
library(ggplot2) # display data on a plot 
library(leaflet) # display data on a map
library(gganimate) # animatation of display  data
# library(gifski) optional for overlapping on image data, if any.

###########
## Uploading and preparing datasets including image/raster datasets for simulation
## Datasets - (e.g. covid19_data)
# merging two files:
file (a) on incidence  
file (b) on location 
# merge two dataframes by ID
file_a <- read.table(file.choose(), header=TRUE, sep=",")
file_b <- read.table(file.choose(), header=TRUE, sep=",")

dataset <- merge(file_a,file_b,by="country")

########### list variables in the merged file
names(dataset)
head(dataset)
# longitude (x)
# latitude (y)
# Time (per day or per hour)

dataset$x <- dataset$longitude
dataset$y <- dataset$latitude
meanx <- mean(dataset$x)
meany <- mean(dataset$y)
minx <- min(dataset$x)
miny <- min(dataset$y)
maxx <- mean(dataset$x)
maxy <- mean(dataset$y)

# Dispay of spread geographically and over time (day or hour)

spread <- ggplot(
  dataset, 
  aes(x = dataset$x, y=dataset$y, size = dataset$confirmed_cases, colour = dataset$country)
) +
  geom_point(show.legend = FALSE, alpha = 1) +
  scale_color_viridis_d() +
  scale_size(range = c(0, 70)) +
  labs(x = "Longitude", y = "Latitude")

spread

# Transition through time 

spread <- spread + transition_time(dataset$day) + labs(title = "Time (Day): {frame_time}")

anim_save("spread.gif", spread)


##########################
# Dispay data on maps

library("ggplot2")
theme_set(theme_bw())
library("sf")
# For mapping purpose we could use different sources, such as ESRI map shape file 
# The package rnaturalearth provides such shape map files of countries for the entire world. 
# As a function "ne_countries" can be used to pull country data and choose the scale (rnaturalearthhires is necessary for scale = "large"). 
# The function can return sp classes (default) or directly sf classes, as defined in the argument returnclass.
  
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# Load the shape file data in three steps
# 
# step 1 - downlOad the zip file
download.file("https://raw.githubusercontent.com/abari212/data/master/coastlines.zip", 
              destfile = 'coastlines.zip')

# step 2 - unzip the file
unzip(zipfile = "coastlines.zip", 
      exdir = 'world_map')

# step 3 - load the data by openning the shape file using readOGR from the sp (spatial) package.
world_map <- readOGR("world_map/ne_10m_coastline.shp")
## OGR data source with driver: ESRI Shapefile 

# Map spatial attributes and extent
class(world_map)
extent(world_map)

# Projecion information of the map
crs(coastlines)

# Display the map
plot(coastlines, 
     main = "World Map")

# To limit and simplify the geometry of the map
world_map_simp <- gSimplify(world_map, 
                             tol = 3, 
                             topologyPreserve = TRUE)
plot(world_map_simp,
     main = "World map with boundaries simplified")

# simplify with a lower tolerance value (keeping more detail)
world_map_2 <- gSimplify(world_map, 
                             tol = .1, 
                             topologyPreserve = TRUE)
plot(world_map_2, 
     main = "World Map")

# Convert map data into data frame to use with ggplot
world_map_df <- SpatialLinesDataFrame(world_map_2,
                                            coastlines@data) 
# Dsiplay the map 
ggplot() +
  geom_path(data = world_map_df, aes(x = long, y = lat, group = group)) +
  labs(title = "World Map")

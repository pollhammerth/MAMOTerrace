# Template file, with a typical workflow 

getwd()
setwd("H:/R_Package_Devel/MAMOTerrace")

#### MAMOT
# install from local repo
library(roxygen2)
roxygenise()
load_all(".")
?mm_alignRas
# install from github
library(devtools)
install_github("pollhammerth/MAMOTerrace")
library(MAMOTerrace)
?mm_alignRas
?mm_metering



# load profile line
path_profile = "notInPackage/testdata/profile.gpkg"

require("terra")

profile_line = terra::vect(path_profile)

searchRadius = 3000

profile_buffer = terra::buffer(x = profile_line, width = searchRadius, capstyle = "flat", joinstyle = "round")



# load rasters
dem = terra::rast("notInPackage/testdata/lidar.tif")

dem_slope = terra::terrain(x = dem, v = "slope", unit = "degrees", neighbors=4) # neighbours has only two options: 8 for rough surfaces, 4 for smoother surfaces
#plet(dem_slope, col = rainbow(256, rev = T) )

rast_2 = terra::rast("notInPackage/testdata/lidarFilled.tif")



#### preparing a map plot (static or interactive)
# hillshade
dem_hs = terra::shade(
  slope = terra::terrain(x = dem, v = "slope", unit = "radians", neighbors=4), 
  aspect = terra::terrain(x = dem, v = "aspect", unit = "radians"), 
  angle = 45, direction = 225, normalize = T
)

# calculate metering of profile
profile_metering = mm_metering(profile_line = profile_line, spacing = 5000, label = T, labelUnit = "km")

# static verification plot
terra::plot(dem_hs, col = grey.colors(256, rev = T))
terra::polys(profile_buffer, lty = 3)
terra::lines(profile_line, lwd = 1)
terra::points(profile_metering, cex = 2, col = "white")
terra::text(profile_metering, labels = profile_metering$label, halo = T, cex = 0.5)
terra::north()
terra::sbar()


# leaflet plot (with terra)
# terra::plet requires a devel version of leaflet
# remotes::install_github("rstudio/leaflet")
m = plet(dem_hs, col = grey.colors(256, rev = T) )
m = lines(m, profile_line, lwd=2, col= "black")
m = lines(m, profile_buffer, lwd=1, col="black")
points(m, profile_metering, cex=2, col="white")



# clip rasters to region of interest (profile_buffer)
dem_crop = terra::crop(dem, profile_buffer, extend = T, mask = T)
dem_slope_crop = terra::crop(dem_slope, profile_buffer, extend = T, mask = T)
rast_2_crop = terra::crop(rast_2, profile_buffer, extend = T, mask = T)


# create empty "align raster", which to align other rasters to
alignRaster = mm_alignRas(x=dem_crop,rN=50)


# resample cropped rasters (align) to matched resolution
dem_res = terra::resample(dem_crop, alignRaster, method="cubic", threads=TRUE)
dem_slope_res = terra::resample(dem_slope_crop, alignRaster, method="cubic", threads=TRUE)
rast_2_res = terra::resample(rast_2_crop, alignRaster, method="cubic", threads=TRUE)






rasterCollection = terra::sprc(x = list(dem_res, dem_slope_res, rast_2_res))






#install.packages("rgl")
require("rgl")

#fem = terra::as.points(rasterCollection)
fem = terra::as.points(dem_res)

d = cbind( as.data.frame( terra::geom(fem)[,c(3,4)] ), z = fem$lidar)

plot3d(d, size = 0.01)












?terra::sprc()


# -> stack rasters
# -> convert to fem
# -> import and prepare maps
# -> spatial join
# -> import and prepare lines
# -> sample lines and extract lidar values
# -> import points and extract lidar values
# -> project everything













# sample points along a line
sf::st_line_sample(sf::st_as_sf(profile_line), density = 10^-3, type = "regular", sample = c(0,0.5,1)) # sample overrides density and n


################################################################################



x1 <- rbind(c(-175,-20), c(-140,55), c(10, 0), c(-140,-60))
x2 <- rbind(c(-125,0), c(0,60), c(40,5), c(15,-45))
x3 <- rbind(c(-10,0), c(140,60), c(160,0), c(140,-55))
x4 <- rbind(c(80,0), c(105,13), c(120,2), c(105,-13))
z <- rbind(cbind(object=1, part=1, x1), cbind(object=2, part=1, x2), 
           cbind(object=3, part=1, x3), cbind(object=3, part=2,  x4))
colnames(z)[3:4] <- c('x', 'y')
z <- cbind(z, hole=0)
z[(z[, "object"]==3 & z[,"part"]==2), "hole"] <- 1

p <- vect(z, "polygons")
geom(p)

f <- system.file("ex/lux.shp", package="terra")
v <- vect(f)
g <- geom(v)
head(g)

w <- geom(v, wkt=TRUE)
substr(w, 1, 60)




install.packages("tidyterra")
require(tidyterra)

require(dplyr)


?terra::window
?terra::viewshed
?terra::values
?terra::trim
?terra::tmpFiles
?terra::tighten
?terra::svc
?terra::sprc
?terra::split
?terra::spin
?terra::spatSample
?terra::snap
?terra::size
?terra::sel
?terra::sds
?terra::same.crs
?terra::resample
?terra::predict
?terra::plet
?terra::persp
?terra::math
?terra::makeNodes
?terra::geom
?terra::extractAlong
?terra::extract
?terra::emptyGeoms
?terra::draw
?terra::describe
?terra::contour
?terra::disagg
?terra::click
?terra::clearance




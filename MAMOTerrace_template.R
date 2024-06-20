# Template file, with a typical workflow 

getwd()
setwd("H:/R_Package_Devel/MAMOTerrace")

# MAMOT
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



require("terra")

# parameters
path_profile = "notInPackage/testdata/profile.gpkg"
searchRadius = 3000




# get profile
profile_line = terra::vect(path_profile)
profile_buffer = terra::buffer(x = profile_line, width = searchRadius, capstyle = "flat", joinstyle = "round")
profile_metering = mm_metering(profile_line = profile_line, spacing = 5000, label = T, labelUnit = "km")


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













# -> import and clip maps to roi
map = terra::vect("notInPackage/testdata/terraceMap.gpkg")
map_crop = terra::crop(map, profile_buffer)


# -> rasterize map matching the alignRaster
map_raster = terra::rasterize(map_crop, alignRaster, field = "NAME_KURZ") # rasterize full map and keep field attributes
# HT = map_crop[map_crop$NAME_KURZ == "02_HT"] %>% terra::rasterize(dem_crop) # rasterize specific polygons only


# -> stack rasters
tem_rast = terra::rast(list(dem_res,map_raster))


# -> convert to tem
tem = terra::as.points(tem_rast)



# -> import and prepare lines
# -> sample lines and extract lidar values
# -> import points and extract lidar values

# -> project rasters
require("s2")
x_profile = s2::s2_project(st_as_sf(profile_line), st_as_sf(tem))
tem_proj = cbind(tem, as.data.frame(x_profile))

p = tem_proj[tem_proj$NAME_KURZ == "02_HT"]

plot(p$x_profile, p$lidar, pch = 46)









# create 3d plot
#install.packages("rgl")
require("rgl")
d = cbind( as.data.frame( terra::geom(tem)[,c(3,4)] ), z = tem$lidar)
plot3d(d, size = 0.01)




# sample points along a line
sf::st_line_sample(sf::st_as_sf(profile_line), density = 10^-3, type = "regular", sample = c(0,0.5,1)) # sample overrides density and n


################################################################################



ras = mm_prepRas(profile="notInPackage/testdata/profile.gpkg",searchRadius=3000,raster=c("notInPackage/testdata/lidar.tif","notInPackage/testdata/lidarFilled.tif"),analysisReso=100, makeSlope = T, makeShade = T)
plot(ras)


require(tidyterra)
require(dplyr)
require(stars)
require(sf)
require(sp)
require(s2)



?s2::s2_distance
?s2::s2_interpolate
?s2::s2_interpolate_normalized
?s2::s2_project
?s2::s2_project_normalized





?sp::select.spatial
?sp::panel.RgoogleMaps
?sp::point.in.polygon
?sp::select.spatial
?sp::spsample
?sp::surfaceArea



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




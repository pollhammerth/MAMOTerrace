# Template file, with a typical workflow 

rm(list=ls())

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
?mm_alignRas # terra
?mm_metering # terra, sf
?mm_prepRas # terra
?mm_prepMap # terra
?mm_linRef # terra, sf, s2



require("terra")
require("s2")
require("sf")


# parameters
path_profile = "notInPackage/testdata/profile.gpkg"
searchRadius = 3000
analysisReso = 50
raster = c("notInPackage/testdata/lidar.tif","notInPackage/testdata/lidarFilled.tif")


# get profile
profile_line = terra::vect(path_profile)
profile_buffer = terra::buffer(x = profile_line, width = searchRadius, capstyle = "flat", joinstyle = "round")
profile_metering = mm_metering(profile_line = profile_line, spacing = 5000, label = T, labelUnit = "km")

# get rasters
ras = mm_prepRas(profile=profile_line, searchRadius, raster, analysisReso, makeSlope = T, makeShade = T)






# static verification plot
terra::plot(ras$hillshade, col = grey.colors(256, rev = T))
terra::polys(profile_buffer, lty = 3)
terra::lines(profile_line, lwd = 1)
terra::points(profile_metering, cex = 2, col = "white")
terra::text(profile_metering, labels = profile_metering$label, halo = T, cex = 0.5)
terra::north()
terra::sbar()


# leaflet plot (with terra)
# terra::plet requires a devel version of leaflet
# remotes::install_github("rstudio/leaflet")
m = plet(ras$hillshade, col = grey.colors(256, rev = T) )
m = lines(m, profile_line, lwd=2, col= "black")
m = lines(m, profile_buffer, lwd=1, col="black")
points(m, profile_metering, cex=2, col="white")



# project raster data
rap = terra::as.points(ras)
rasp = mm_linRef(p = rap, l = profile_line, addz = T, asVector = F)


# get and prepare a map
map = mm_prepMap(map="notInPackage/testdata/terraceMap.gpkg", field = "NAME_KURZ", cropper = profile_buffer, aligner = ras, asVector = F)













# -> convert raster data to spatVector
ras_vect = terra::as.points(ras)
# -> project rasters and add distance along profile as raster layer
require("s2")
require("sf")
x_profile = s2::s2_project(st_as_sf(profile_line), st_as_sf(ras_vect))
#z_profile = s2::s2_distance(st_as_sf(profile_line), st_as_sf(ras_vect)) # optional. Takes a little processing time. Dont if not needed.
z_profile = as.vector( terra::distance(profile_line,ras_vect) ) # alternatively use terra package, which is faster
#ras_vect_proj = cbind(ras_vect, as.data.frame(x_profile), as.data.frame(z_profile))
ras_vect_proj = cbind(ras_vect, as.data.frame(x_profile), as.data.frame(z_profile))
ras_rast_proj = terra::rasterize(ras_vect_proj,ras, field = names(ras_vect_proj))




# -> import and clip maps to roi
map = terra::vect("notInPackage/testdata/terraceMap.gpkg")
map_crop = terra::crop(map, profile_buffer)

# -> rasterize map matching the alignRaster and convert to spatVector::Points
map_raster = terra::rasterize(map_crop, ras, field = "NAME_KURZ") # rasterize full map and keep field attributes
# HT = map_crop[map_crop$NAME_KURZ == "02_HT"] %>% terra::rasterize(dem_crop) # rasterize specific polygons only
map_raster_points = terra::as.points(map_raster)

# -> stack rasters
#tem_rast = terra::rast(list(ras,map_raster))




# -> import and prepare lines
# -> sample lines and extract lidar values
# -> import points and extract lidar values









# Prepare Plotdata
p = terra::rast(list(rasp,map)) # add map to raster data
p = terra::as.points(p) # convert to spatVector
p = p[p$NAME_KURZ == "02_HT"] # filter pixels by map. e.g. HT ...

p = terra::as.points(rasp)

# ... and continue with PMT pt.2 as usual. e.g.:
terra::plot(p$x,p$lidar,pch=46)






# create 3d plot
#install.packages("rgl")
require("rgl")
d = cbind( as.data.frame( terra::geom(p)[,c(3,4)] ), z = p$lidar)
plot3d(d, size = 0.01)




# sample points along a line
sf::st_line_sample(sf::st_as_sf(profile_line), density = 10^-3, type = "regular", sample = c(0,0.5,1)) # sample overrides density and n


################################################################################





plot(map_raster$NAME_KURZ,ras$lidar)


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




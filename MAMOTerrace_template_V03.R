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
?mm_sampleLines # terra, sf



require("MAMOTerrace")
require("terra")
require("s2")
require("sf")


require("sessioninfo")
session_info()
sessionInfo()


# parameters
path_profile = "notInPackage/testdata/profile.gpkg"
searchRadius = 7000
analysisReso = 50
raster = c("notInPackage/testdata/lidar.tif","notInPackage/testdata/lidarFilled.tif")


# get profile
profile_line = terra::vect(path_profile)
profile_buffer = terra::buffer(x = profile_line, width = searchRadius, capstyle = "flat", joinstyle = "round")
profile_metering = mm_metering(profile_line = profile_line, spacing = 5000, label = T, labelUnit = "km")

# get rasters
ras = mm_prepRas(profile=profile_line, searchRadius, raster, analysisReso, makeSlope = T, makeShade = T)



# project rasters
rap = terra::as.points(ras)
rasp = mm_linRef(p = rap, l = profile_line, addz = T, asVector = F)


# get and prepare a map
map = mm_prepMap(map="notInPackage/testdata/terraceMap.gpkg", field = "NAME_KURZ", cropper = profile_buffer, aligner = ras, asVector = F)



# get lines, sample them and extract lidar values
riv = terra::vect("notInPackage/testdata/rivers.gpkg")
rivLidar = mm_sampleLines(l = riv, cropper = profile_buffer,density = 0.01, raster = ras$lidar, fieldName = "lidar")

ice = terra::vect("notInPackage/testdata/iceExtent.gpkg")
iceLidar = mm_sampleLines(l = ice, cropper = profile_buffer,density = 0.01, raster = ras$lidar, fieldName = "lidar")




# import points and extract lidar values
nam = terra::vect("notInPackage/testdata/localNames.gpkg") # nam = terra::disagg(nam) # convert to singlepart if it is multipart
nam[["lidar"]] <- terra::extract(ras$lidar,nam,method = "bilinear")[,2]
age = terra::vect("notInPackage/testdata/datedOutcrops.gpkg")
age[["lidar"]] <- terra::extract(ras$lidar,age,method = "bilinear")[,2]



# project points
rivp = mm_linRef(p = rivLidar, l = profile_line, addz = T, asVector = T)
icep = mm_linRef(p = iceLidar, l = profile_line, addz = T, asVector = T)
namp = mm_linRef(p = nam, l = profile_line, addz = T, asVector = T)
agep = mm_linRef(p = age, l = profile_line, addz = T, asVector = T)









# Prepare Plotdata
p = terra::rast(list(rasp,map)) # add map to raster data
p = terra::as.points(p) # convert to spatVector
p = p[p$NAME_KURZ == "02_HT"] # filter pixels by map. e.g. HT ...
ps = p[p$slope < 2] # filter by slope

# ... and plot or continue with PMT pt.2 as usual:
plot(rasp$x, rasp$lidar, pch=NA, xlab = "profile distance", ylab = "elevation")
points(p$x, p$lidar, pch=46, col = "steelblue", cex = 3)
points(ps$x, ps$lidar, pch=46, col = "blue", cex = 3)
with(rasp, points(x = "x", y = "lidar", pch = 46) )
points(namp$x, namp$lidar, pch = 21, cex = 2, col = "black", bg = "white")
points(agep$x, agep$lidar, pch = 21, cex = 2, col = "black", bg = "green")
points(rivp$x, rivp$lidar, pch = 16, cex = 1, col = "darkblue")
points(icep$x, icep$lidar, pch = 16, cex = 1, col = "lightblue")




# static verification plot
terra::plot(ras$hillshade, col = grey.colors(256, rev = T))
terra::polys(profile_buffer, lty = 3)
terra::lines(profile_line, lwd = 1)
terra::points(profile_metering, cex = 2, col = "white")
terra::text(profile_metering, labels = profile_metering$label, halo = T, cex = 0.5)
terra::north()
terra::sbar()
terra::points(nam, pch=21, cex=2, col = "black", bg="white")
terra::text(nam, labels = nam$name, halo = T, cex = 1,pos=3)
terra::points(age, pch=21, cex=2, col = "black", bg="green")
terra::text(age, labels = age$age, halo = T, cex = 1.5,pos=3)



# leaflet plot (with terra)
# terra::plet requires a devel version of leaflet
# remotes::install_github("rstudio/leaflet")
m = plet(ras$hillshade, col = grey.colors(256, rev = T) )
m = lines(m, profile_line, lwd=2, col= "black")
m = lines(m, profile_buffer, lwd=1, col="black")
points(m, profile_metering, cex=2, col="white")



# create a 3d plot
#install.packages("rgl")
require("rgl")
d = cbind( as.data.frame( terra::geom(rap)[,c(3,4)] ), z = rap$lidar)
rgl::plot3d(d, size = 0.01)
r = cbind( as.data.frame( terra::geom(rivLidar)[,c(3,4)] ), z = rivLidar$lidar)
rgl::points3d(r,col="blue",size=7)
i = cbind( as.data.frame( terra::geom(iceLidar)[,c(3,4)] ), z = iceLidar$lidar)
rgl::points3d(i,col="steelblue",size=7)
n = cbind( as.data.frame( terra::geom(nam)[,c(3,4)] ), z = nam$lidar)
rgl::points3d(n,col="darkgreen",size=20)




#### testing ###################################################################

?rgl::select3d()


if (interactive() && !in_pkgdown_example()) {
  x <- d$x
  y <- d$y
  z <- d$z
#  open3d()
#  points3d(x, y, z)
  plot3d(x,y,z, size = 0.001)
  f <- select3d()
  if (!is.null(f)) {
    keep <- f(x, y, z)
    pop3d()
    points3d(x[keep], y[keep], z[keep], color = 'red')
    points3d(x[!keep], y[!keep], z[!keep])
  }
}

if (interactive() && !in_pkgdown_example()) {
  x <- d$x
  y <- d$y
  z <- d$z
  #  open3d()
  #  points3d(x, y, z)
  plot3d(x,y,z, size = 0.0001)
}
f <- select3d()
if (!is.null(f)) {
  keep <- f(x, y, z)
  pop3d()
  points3d(x[keep], y[keep], z[keep], color = 'red')
  points3d(x[!keep], y[!keep], z[!keep])
}




################################################################################







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




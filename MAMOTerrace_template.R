# Template file, with a typical workflow 

getwd()
setwd("H:/R_Package_Devel/MAMOTerrace")


# load profile line
path_profile = "notInPackage/testdata/profile.gpkg"

require("terra")

profile_line = terra::vect(path_profile)

searchRadius = 3000

profile_buffer = terra::buffer(x = profile_line, width = searchRadius, capstyle = "flat", joinstyle = "round")



# load rasters
dem = terra::rast("notInPackage/testdata/lidar.tif")

dem_slope = terra::terrain(x = dem, v = "slope", unit = "degrees", neighbors=4) # neighbours has only two options: 8 for rough surfaces, 4 for smoother surfaces

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








# sample points along a line
sf::st_line_sample(sf::st_as_sf(profile_line), density = 10^-3, type = "regular", sample = c(0,0.5,1)) # sample overrides density and n








# FUNCTION :: points along a line, spacing in m, starting with line distance = 0
# dependency: sf, terra
mm_metering = function(profile_line, spacing = 1000, label = T, labelUnit = "km") {
  
  # convert to simple feature for package sf
  sf = sf::st_as_sf(profile_line)
  
  # for correct labels, crs units must be m when doing sf::st_line_sample()
  # check and transform if necessary
  original_proj = terra::crs(profile_line, proj=T)
  unitOK = grepl("+units=m", original_proj)
  if(unitOK == FALSE) { sf = sf::st_transform(sf, 3857) }
  
  # create metering
  l = as.numeric( sf::st_length(sf) ) # get profile length
  metr = sf::st_line_sample( sf, sample = seq(0, 1, by = 1/l*spacing) )
  metr = sf::st_cast(metr,"POINT") # convert to POINT instead of MULTIPOINT
  
  # transform metering result back to original crs
  if(unitOK == FALSE) {
    original_EPSG = sf::st_crs(profile_line)$epsg # get original epsg
    metr = sf::st_transform(metr, original_EPSG)
  }
  
  # convert to spatVector
  profile_metering = terra::vect(metr)
  
  #### add field for labels
  if (label) {
    n = seq(0, 1, by = 1/l*spacing)
    label = n * l / if (labelUnit == "km") {1000} else if (labelUnit == "m") {1}
    terra::values(profile_metering) = as.data.frame(label)
  }
  
  return(profile_metering)
}












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
?terra::extractAlong
?terra::extract
?terra::emptyGeoms
?terra::draw
?terra::describe
?terra::contour
?terra::disagg
?terra::click
?terra::clearance




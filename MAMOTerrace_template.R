# Template file, with a typical workflow 


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

dem_slope = terra::terrain(x = dem, v = "slope", unit = "degrees", neighbors=4)

rast_2 = terra::rast("notInPackage/testdata/lidarFilled.tif")


dem_hs = terra::shade(
  slope = terra::terrain(x = dem, v = "slope", unit = "radians", neighbors=4), 
  aspect = terra::terrain(x = dem, v = "aspect", unit = "radians"), 
  angle = 45, direction = 225, normalize = T
)



# verification plot
terra::plot(dem_hs, col = grey.colors(256, rev = T))
terra::plot(profile_buffer, add = T, lty = 3)
terra::plot(profile_line, add = T, lwd = 1)





?terra::plot


?grey.colors()

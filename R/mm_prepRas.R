#' Load, crop and align rasters
#'
#' Requires terra package.
#' Loads one or more rasters, that can be given as a vector or list of raster paths or spatRasters or mixed. The rasters are cropped and resampled to a desired analysis resolution and search radius around a given Profile. The profile can be given as path to, or spatVector.
#' Output is a spatRast, with each imported raster as a layer.
#' @param profile character (path to) or spatVector of a profile line.
#' @param searchRadius Radius around the profile to create a buffer to clip rasters with.
#' @param raster vector or list of character (path to) and or spatRaster. 
#' @param analysisReso Resolution to which all rasters should be resampled.
#' @param makeSlope logical. Should a slope raster be added for the first provided raster?
#' @param makeShade logical. Should a hillshade be added for the first provided raster?
#' @return spatRaster
#' @examples 
#' ras = mm_prepRas(profile="profile.gpkg",searchRadius=3000,raster=c("data/lidar.tif","data/lidarFilled.tif"),analysisReso=100, makeSlope = T, makeShade = T);
#' @export
mm_prepRas = function (profile, searchRadius, raster, analysisReso, makeSlope = T, makeShade = T) {
  
  # get profile line and calculate buffer
  if (class(profile)=="character") { profile = terra::vect(profile) }
  profile_buffer = terra::buffer(x = profile, width = searchRadius, capstyle = "flat", joinstyle = "round")

  # get first raster and crop it to buffer
  ras = terra::rast(raster[1])
  ras_crop = terra::crop(ras, profile_buffer, extend = T, mask = T)
  
  # calculate slope
  if (makeSlope) { slope = terra::terrain(x = ras_crop, v = "slope", unit = "degrees", neighbors=4) } # neighbours has only two options: 8 for rough surfaces, 4 for smoother surfaces
  
  # create empty raster with analysisReso and extend of cropped raster
  alignRaster = mm_alignRas(x=ras_crop,rN=analysisReso)
  
  # resample cropped raster to match alignRaster  
  dem = terra::resample(ras_crop, alignRaster, method="cubic", threads=TRUE)
  
  # resample and stack slope
  slope = terra::resample(slope, alignRaster, method="cubic", threads=TRUE)
  dem = terra::rast(list(dem,slope))
  
  # calculate hillshade
  if (makeShade) {
  hillshade = terra::shade(
    slope = terra::terrain(x = dem[[1]], v = "slope", unit = "radians", neighbors=4), 
    aspect = terra::terrain(x = dem[[1]], v = "aspect", unit = "radians"), 
    angle = 45, direction = 225, normalize = T
  )
  dem = terra::rast(list(dem,hillshade))
  }
  
  # load, crop, align and stack additional rasters to the first raster
  if (length(raster)>1) { 
    for (i in 2:length(raster)) { 
      rasi = terra::rast(raster[i])
      rasi_crop = terra::crop(rasi, profile_buffer, extend = T, mask = T)
      rasi_crop_resam = terra::resample(rasi_crop, alignRaster, method="cubic", threads=TRUE)
      dem = terra::rast(list(dem,rasi_crop_resam))
    } 
  }
  
  return(dem)
  
}

# profile="notInPackage/testdata/profile.gpkg"
# searchRadius=3000
# raster=c("notInPackage/testdata/lidar.tif","notInPackage/testdata/lidarFilled.tif")
# analysisReso=100
# makeSlope = T
# makeShade = T

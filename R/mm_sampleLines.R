#' Sample points along lines, extracting raster value for each point.
#'
#' Requires terra, sf packages.
#' Takes a spatVector of lines (or the path to a shapefile) and a spatRaster. Points are placed along the line in a given density (number per unit), and raster values are extracted at the point locations. The lines are cropped, to match the profile buffer.
#' Output is a SpatVector of points attributed by the local raster values.
#' @param l SpatVector or path to gpkg of lines to sample.
#' @param cropper SpatVector(e.g. profile_buffer), to which the lines are cropped.
#' @param density Number of points within one unit.
#' @param raster SpatRaster, from which values are attached to the output.
#' @param fieldName Name of the field, where the raster values will be stored. Standard is "rastValu".
#' @return spatVector of points
#' @examples 
#' profile_buffer = terra::buffer(x = terra::vect("notInPackage/testdata/profile.gpkg"), width = 3000, capstyle = "flat", joinstyle = "round");
#' ras = mm_prepRas(profile=profile_line, 3000, c("notInPackage/testdata/lidar.tif","notInPackage/testdata/lidarFilled.tif"), 50, makeSlope = T, makeShade = T);
#' samplePoints = mm_sampleLines(l = "notInPackage/testdata/rivers.gpkg", cropper = profile_buffer, density = 0.01, raster = ras$lidar);
#' @export
mm_sampleLines = function (l,cropper=NA,density,raster=NA,fieldName="rastValu") {
  
  if (is.character(l)) { l = terra::vect(l) }
  
  if (class(cropper) == "SpatVector") {l_crop = terra::crop(l,cropper) } else { l_crop = l } # if no cropper is given, dont crop.
  
  # convert for package sf
  l_spat = methods::as(l_crop, "Spatial")
  l_single = sf::st_cast(sf::st_as_sf(l_spat), "LINESTRING")
  
  # sample points along lines
  p_sample = sf::st_line_sample(l_single, density = density, type = "regular")# , sample = c(0,0.5,1)) # sample overrides density and n
  
  # reconvert to SpatVector
  p_spatVector = terra::vect(p_sample)
  
  # make it a singlepart feature object (if the source was multipart, the result will be multipart and would create problems)
  p_spatVector = terra::disagg(p_spatVector)
  
  if (class(raster)=="SpatRaster") {  
    # extract raster values at points
    rastValu = terra::extract(raster,p_spatVector,method = "bilinear")
    # add values to points
    p_spatVector[[fieldName]] <- rastValu[,2]
  }
  
  return(p_spatVector)
  
}



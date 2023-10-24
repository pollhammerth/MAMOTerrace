#' Load, crop and rasterize polygon map
#'
#' Requires terra package.
#' Loads a polygon file or takes a spatVector of Polygons. Then clips it with a given polygon. Rasterizes map, aligned to given raster, keeping specified field. Output can be chosen as spatRaster or spatVector of points.
#' Output is a spatRast, or spatVector of points. Only specified fields are kept.
#' @param map character (path to) or spatVector of a polygon shapefile.
#' @param field Field of the map, that schould be kept.
#' @param cropper spatVector, Polygon to which the map should be cropped (i.e. profile_buffer). 
#' @param aligner spatRaster to which the rasterized map should be aligned.
#' @param asVector logical. Should the output be a spatVector = T or a spatRaster = F.
#' @return spatVector or spatRaster
#' @examples 
#' profile_buffer = terra::buffer(x = terra::vect("notInPackage/testdata/profile.gpkg"), width = 3000, capstyle = "flat", joinstyle = "round");
#' ras = mm_prepRas(profile=profile_line, 3000, c("notInPackage/testdata/lidar.tif","notInPackage/testdata/lidarFilled.tif"), 50, makeSlope = T, makeShade = T);
#' map = mm_prpMap(map="notInPackage/testdata/terraceMap.gpkg",field="NAME_KURZ",cropper=profile_buffer,aligner=ras,asVector=T);
#' @export
mm_prepMap = function (map,field,cropper,aligner,asVector=T) {
  
  if (is.character(map)) { map = terra::vect(map) } # load map if path is given
  
  map_crop = terra::crop(map, cropper) # clip map to polygon (profile_buffer)
  
  map_raster = terra::rasterize(map_crop, aligner, field = field) # rasterize full map and keep specified field
  
  if (asVector) 
    { map_raster_points = terra::as.points(map_raster); return(map_raster_points) } 
  else 
    { return(map_raster) }
  
}







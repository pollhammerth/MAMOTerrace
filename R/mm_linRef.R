#' Calculate the orthogonally projected distance of points on a line.
#'
#' Requires terra, s2, and sf packages.
#' Takes a SpatVector of points, that are projected on a SpatVector line, and the distance along the line is added to the point object as a new column. Additionally the distance to the line can be added as z. The standard output is a SpatVector, but can be set to SpatRaster.
#' Output is a SpatVector of points or a SpatRaster.
#' @param p SpatVector of Points for which the orthogonally projected distance on a line should be calculated.
#' @param l SpatVector, line on which points should be projected.
#' @param addz logical. Should also the distance to the line be added? = T. Increases calculation time. Only set T, if you need z values.
#' @param asVector logical. Should the output be a spatVector = T or a spatRaster = F.
#' @return spatVector or spatRaster
#' @examples 
#' profile_line = terra::vect("notInPackage/testdata/profile.gpkg");
#' ras = mm_prepRas(profile=profile_line, 3000, c("notInPackage/testdata/lidar.tif","notInPackage/testdata/lidarFilled.tif"), 50, makeSlope = T, makeShade = T);
#' rap = terra::as.points(ras);
#' rapp = mm_linRef(p = ras, l = profile_line, addz = T, asVector = T);
#' @export
mm_linRef = function (p,l,addz=F,asVector=T) {
  
  if (class(p) == "SpatRaster") { p = terra::as.points(p) } # convert x to spatVector if a raster is provided
  
  x = s2::s2_project(st_as_sf(l), st_as_sf(p)) # get projected distance along line. SpatVector need to be converted to simple features for this.
  
  # add distance along line (x) and, if desired, also distance to line (z) and store as SpatVector
  if (addz) {
    #z = s2::s2_distance(st_as_sf(profile_line), st_as_sf(ras_vect)) # optional. Takes a little processing time. Dont do it, if not needed.
    z = as.vector( terra::distance(l,p) ) # alternatively use terra package, which is faster.
    out_Vector = cbind(p, as.data.frame(x), as.data.frame(z))
  } 
  else {
    out_Vector = cbind(p, as.data.frame(x))
  }
  
  # output either as SpatVector or SpatRaster  
  if (asVector) {
    return(out_Vector)
  } else {
    out_Raster = terra::rasterize(out_Vector, ras, field = names(out_Vector))
    return(out_Raster)
  }
  
}




#' Create raster to align to afterwards
#'
#' Requires terra package.
#' Create raster with same or slightly larger extent than a given raster, but with a new desired resolution. Extent may be enlarged in all directions, so that the exact desired resolution fits in.
#' Output can be used for terra::resample()
#' @param x Raster that is used to define the extent
#' @param rN Desired resolution of the newly created raster
#' @return New raster
#' @examples 
#' lidar = data("lidar.tif");
#' alignRas = mm_alignRas(x=lidar,rN=100);
#' @export
mm_alignRas = function(x,rN){
  ex = terra::ext(x) # get extent of input raster
  # get length of input raster dimensions
  xL = ex[2]-ex[1]
  yL = ex[4]-ex[3]
  # calculate number, that needs to be added or subtracted to original raster xmin,xmax,ymin,ymax, so that new res fits in exactly
  pmx = ( 1 - ( xL/rN - floor(xL/rN) ) ) * rN / 2
  pmy = ( 1 - ( yL/rN - floor(yL/rN) ) ) * rN / 2
  # calculate extent of new raster
  exN = c( ex[1]-pmx, ex[2]+pmx, ex[3]-pmy, ex[4]+pmy )
  # create new raster with same crs as input raster
  s <- terra::rast(nrows=(exN[4]-exN[3])/rN, 
                   ncols=(exN[2]-exN[1])/rN, 
                   xmin=exN[1], xmax=exN[2], ymin=exN[3], ymax=exN[4])
  values(s) = 1:ncell(s)
  terra::crs(s) <- terra::crs(x)
  # output
  return(s)
}
# x <- terra::resample(lidar, s, method="cubic", threads=TRUE)

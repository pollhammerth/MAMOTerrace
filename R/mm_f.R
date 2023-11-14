#' Filter projected data by rasterized map or numeric values and convert to data.frame.
#'
#' Requires terra packages
#' Takes SpatRaster or SpatVector of projected data (output of mm_prepRas() %>% mm_linRef()) and SpatRaster of rasterized map (output of mm_prepMap()) and arguments defining which pixels should be filtered. Output is a data.frame, which allows compatibility with PMT3 functions.
#' Output is a data.frame with x,y,z coordinates of the profile view and X,Y coordinates of the map view and slope values.
#' @param v Character or Numeric. Value of a specified field (f) of pixels that should be kept in the output.
#' @param f Character. Field name, containing the values, that should be kept. This can be field (layer) of m or x.
#' @param c Character. Condition, defining what should be done with pixels matching v. Standard is "==", which means keep. "!=" = keep all other, "<=" or similar can be used for numeric field content.
#' @param n Integer. The map needs to be provided as a list and n defines which map list entry should be used.
#' @param x SpatRaster, SpatVector or data.frame. Projected raster data.
#' @param y Character. Name of the field (layer) containing elevation values. This field will be named "y" in the output.
#' @param m SpatRaster. List of rasterized maps. Maps as created with mm_prepMap(). The resolution needs to be the same as of x.
#' @return data.frame
#' @examples 
#' DEM = mm_prepRas(profile = "notInPackage/testdata/profile.gpkg", searchRadius = 3000, raster = c("notInPackage/testdata/lidar.tif", "notInPackage/testdata/lidarFilled.tif"), analysisReso=100, makeSlope = T, makeShade = T);
#' DEMp = mm_linRef(p = ras, l = profile_line, addz = T, asVector = F);
#' map = mm_prpMap(map="notInPackage/testdata/terraceMap.gpkg",field="NAME_KURZ",cropper=profile_buffer,aligner=ras,asVector=T);
#' maps = list(map);
#' 
#' # stds is a helper, setting standard argument input for mm_f(), so values do not have to be typed each time.;
#' stds = list(projectedRaster = "DEMp", yValue = names(rasp)[1], rasterizedMaps = "maps", mapNumber = 1, mapField = "NAME_KURZ");
#' 
#' # filter single mapped value;
#' mm_f("01_NT");
#' 
#' # filter more values, keeping both;
#' mm_f( c("01_NT", "02_HT") );
#' 
#' # filter by values in "slope", keeping all values <= than 2;
#' mm_f(2,"slope","<=");
#' 
#' # filter all pixels that are within mapped areas;
#' mm_f();
#' 
#' # convert full raster to data.frame;
#' mm_f(m=NA);
#' 
#' @export
mm_f = function( v, f = stds$mapField, c = "==", n = stds$mapNumber, x = get(stds$projectedRaster), y = stds$yValue, m = get(stds$rasterizedMaps) ) {
  
  if (class(x)[1] == "SpatRaster") { # if the input is "SpatVector" or data.frame, it is assumed, this has been done before.
    if (n != 0) {
      x = terra::rast( list( x, m[[n]] ) ) # add map to raster data (reduces pixels to exising polygon areas)
    }
    x = terra::as.points( x ) # convert map to spatVector
  }
  
  names(x)[names(x) == y] = "y" # rename column, containing y values
  
  coords = as.data.frame(geom(x)[,c(3,4)])
  x = as.data.frame(x,X=coords[,1],Y=coords[,2]) # add map coordinates, so they do not get lost when converting to data.frame
  
  if (missing(v)) {} else { # if no v is given, do not filter
    if (length(v) > 1) { x = x[x[[f]] %in% v,] } else { # either filter for more than one value, or one value with customizable condition
      e = paste0("x = x[x[[f]] ", c, " v,]")
      eval(parse(text=e))
    }
  }
  
  return(x)
  
}

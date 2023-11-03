#' Interactively create a horizontal map from a 3d view.
#'
#' Requires terra, rgl packages.
#' Takes three vectors of x, y and z coordinates of points (digital elevation model), to be plotted in 3d. And a hillshade of the same region. The points can be viewed and mapped in 3d iteratively, resulting in a horizontal map of the selected region, that is returned as SpatVector of points, polygons or SpatRaster.
#' Output is a SpatVector of polygons or alternatively points or SpatRaster
#' @param x,y,z Vectors, containing the x, y and z coordinates of points (Digital Elevation Model, converted to points).
#' @param r SpatRaster, containing a hillshade of the same region as the points.
#' @param type Specifies whether the output should be "polygons", "points", or "raster". Standard is "polygons".
#' @return spatVector of polygons, points or SpatRaster
#' @examples 
#' DEM = mm_prepRas(profile = "notInPackage/testdata/profile.gpkg", searchRadius = 3000, raster = c("notInPackage/testdata/lidar.tif", "notInPackage/testdata/lidarFilled.tif"), analysisReso=100, makeSlope = T, makeShade = T);
#' DEMp = terra::as.points(DEM[[1]]) # lidar to points
#' xyz = data.frame( geom(DEMp)[,c(3,4)], DEMp[[1]] ) # points to data.frame
#' map3dResult = mm_map3d(x = d[[1]], y = d[[2]], z = d[[3]], DEM$hillshade, type = "polygons")
#' @export
mm_map3d = function (x,y,z,r,type = "polygons") {  
  
  # create 3d plot from x, y and z points
  plot3d(x,y,z, size = 0.01, col = "white"); clear3d(); points3d(x,y,z, size = 0.01, col = "black")
  
  #### select points and polygonize them in a map view. Iteratively remove or add points from/to the selection
  repeat{
    
    # select points in 3d plot
    f <- select3d(button = c("middle"), dev = cur3d())
    
    # if points are selected, highlight them in 3d view and map them in a map view
    if (!is.null(f)) {
      
      # keep the old "keep" (vector of T & F, defining which points to select) from the previous run, if there has already been a run.
      if (exists("keep")) { old_keep = keep } 
      
      # execute select function, to create a vector of T & F, defining the current selection.
      keep <- f(x, y, z) 
      
      # edit "keep", so that the new selection will be subtracted from the previous selection
      if (exists("continue") & continue == "r") {
        onkeep = data.frame(old_keep, keep, final = rep(FALSE, length(old_keep)) )
        onkeep[onkeep[[1]] != onkeep[[2]] & onkeep[[1]] == TRUE,][[3]] = TRUE
        keep = onkeep[[3]]
      }
      
      # edit "keep", so that the new selection will be added to the previous selection
      if (exists("continue") & continue == "a") {
        onkeep = data.frame(old_keep, keep, final = rep(TRUE, length(old_keep)) )
        onkeep[onkeep[[1]] == onkeep[[2]] & onkeep[[2]] == FALSE,][[3]] = FALSE
        keep = onkeep[[3]]
      }
      
      #### create/update 3d plot with selected points highlighted
      clear3d()
      points3d(x[keep], y[keep], z[keep], color = 'red', size = 0.01)
      points3d(x[!keep], y[!keep], z[!keep], color = "black", size = 0.01)
      
      #### create a map of the selection in horizontal view (control plot)
      # save selected points coordinates as data.frame
      mapped = data.frame(x = x[keep], y = y[keep], z = z[keep])
      # convert to SpatPoints - SpatRaster - SpatPolygons
      map_points = terra::vect(data.frame(x=mapped$x,y=mapped$y), geom=c("x","y"), crs = crs(r,proj=T) )
      map_raster = terra::rasterize(map_points, r) # align raster to given raster r
      map_polygons = terra::as.polygons(map_raster)
      # create control plot, with r (hillshade) as background and the newly mapped  polygon on top
      terra::plot(r, col = grey.colors(256, rev = T))
      terra::polys(map_polygons,col="#ff000033", border="#00000033")
      
    }
    
    # ask the user, if he/she wants to remove, add, restart or finish the mapping process
    continue = readline(prompt = "do you want to: remove points / add points / start new / end selection? r/a/s/e ")
    if (continue == "e") { break }
    
  }  
  
  # return polygons, points or raster, depending on user request (argument 5)
  if (exists("map_points")) {  
    if (type=="polygons"){return(map_polygons)} else if (type=="points"){return(map_points)} else if (type=="raster"){return(map_raster)}
  }
  
}

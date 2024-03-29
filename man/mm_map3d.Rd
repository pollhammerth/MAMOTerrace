% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mm_map3d.R
\name{mm_map3d}
\alias{mm_map3d}
\title{Interactively create a horizontal map from a 3d view.}
\usage{
mm_map3d(x, y, z, r, type = "polygons")
}
\arguments{
\item{x, y, z}{Vectors, containing the x, y and z coordinates of points (Digital Elevation Model, converted to points).}

\item{r}{SpatRaster, containing a hillshade of the same region as the points.}

\item{type}{Specifies whether the output should be "polygons", "points", or "raster". Standard is "polygons".}
}
\value{
spatVector of polygons, points or SpatRaster
}
\description{
Requires terra, rgl packages.
Takes three vectors of x, y and z coordinates of points (digital elevation model), to be plotted in 3d. And a hillshade of the same region. The points can be viewed and mapped in 3d iteratively, resulting in a horizontal map of the selected region, that is returned as SpatVector of points, polygons or SpatRaster.
Output is a SpatVector of polygons or alternatively points or SpatRaster
}
\examples{
DEM = mm_prepRas(profile = "notInPackage/testdata/profile.gpkg", searchRadius = 3000, raster = c("notInPackage/testdata/lidar.tif", "notInPackage/testdata/lidarFilled.tif"), analysisReso=100, makeSlope = T, makeShade = T);
DEMp = terra::as.points(DEM[[1]]) # lidar to points
xyz = data.frame( geom(DEMp)[,c(3,4)], DEMp[[1]] ) # points to data.frame
map3dResult = mm_map3d(x = d[[1]], y = d[[2]], z = d[[3]], DEM$hillshade, type = "polygons")
}

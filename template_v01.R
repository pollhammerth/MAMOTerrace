rm(list=ls())
setwd("H:/TEMP/MAMOT")
setwd("H:/R_Package_Devel/MAMOTerrace")

library(devtools)
install_github("pollhammerth/MAMOTerrace")
require("MAMOTerrace")
require("terra")
require("s2")
require("sf")




analysisReso = 40
searchRadius = 5000
path_profile = "H:/GIS/PhD/profiles/Iller/parallel_01/profile/profil.gpkg"
path_raster = NA # c("notInPackage/testdata/lidar.tif","notInPackage/testdata/lidarFilled.tif")




# choose lidar path fitting analysisReso and add lidar path in front of additional raster paths
if ( analysisReso <  10 | analysisReso == 15         ) { lidar = c("H:/GIS/DEMs/5m/alps_5m.tif") } else if (
     analysisReso <  20 | analysisReso == 30         ) { lidar = c("H:/GIS/DEMs/10m/alps_10m.tif") } else if (
     analysisReso <  50 | analysisReso %in% c(60, 80)) { lidar = c("H:/GIS/DEMs/20m/alps_20m.tif") } else if (
     analysisReso >= 50                             ) { lidar = c("H:/GIS/DEMs/50m/alps_50m.tif") }
if ( is.na(path_raster) ) { path_raster = lidar } else { path_raster = c(lidar, path_raster) }; rm(lidar)

# load and prepare profile
profile_line = terra::vect(path_profile)
profile_buffer = terra::buffer(x = profile_line, width = searchRadius, capstyle = "flat", joinstyle = "round")
profile_metering = mm_metering(profile_line = profile_line, spacing = 5000, label = T, labelUnit = "km") # --------------------------------------------- spacing

# load rasters
ras = mm_prepRas(profile=profile_line, searchRadius, path_raster, analysisReso, makeSlope = T, makeShade = T)

# test plot to see if data makes sense so far
terra::plot(ras$hillshade, col = grey.colors(256, rev = T))
terra::polys(profile_buffer, lty = 3)
terra::lines(profile_line, lwd = 1)
terra::points(profile_metering, cex = 2, col = "white")
terra::text(profile_metering, labels = profile_metering$label, halo = T, cex = 0.5)
terra::north(); terra::sbar()

# project rasters on to profile line
st=Sys.time(); rasp = mm_linRef(p = ras, l = profile_line, addz = T, asVector = F); et=Sys.time(); et-st

# load and prepare a map(s)
map = list(      m1 = mm_prepMap(map="H:/GIS/PhD/maps/flagg/terraces.gpkg", field = "NAME_KURZ", cropper = profile_buffer, aligner = ras, asVector = F) ) # -------------- map; field
map = append( map, mm_prepMap(map="H:/GIS/PhD/maps/hydro/rivers_OPAL_buffer200.gpkg", field = "name", cropper = profile_buffer, aligner = ras, asVector = F) ); names(map)[length(map)] = "m2" # -------------- map; field; names(map) <-






# -> prepare data for pt.2

# get full lidar and set plot extent
y.source = names(rasp)[1]
rapp = terra::as.points(rasp) # convert to spatVector
names(rapp)[names(rapp) == y.source] = "y" # rename y column
rapp = as.data.frame(rapp)
extent = pmt.extent(data=rapp,x="x",y="y",z="z") # set plot extent

# get map
m1 = terra::rast(list(rasp,map[[1]])) # add map to raster data
m1 = terra::as.points(m1) # convert map to spatVector
names(m1)[names(m1) == y.source] = "y" # rename y column
# filter map
HT_b2 = as.data.frame( m1[m1$NAME_KURZ == "02_HT" & m1$slope <= 2] ) # filter plot object
HT_a2 = as.data.frame( m1[m1$NAME_KURZ == "02_HT" & m1$slope > 2] ) # filter plot object

# plot profile
dev.new()
pmt.empty(grid=T,main="")
pmt.plot( HT_a2, col = "steelblue", cex = 30 )
pmt.plot( HT_b2, col = "blue", cex = 30 )
pmt.plot( rapp )


b = pmt.bin(HT_b2, interval = 200, value = "median", mode = "bin", cth = NA, sth = NA)
m = pmt.model( b, deg = 2)
pmt.plotModel(m, col = "blue")




########################################################################## START
################################################################################

require("rgl")

d = data.frame( geom(rapp)[,c(3,4)], rapp[[1]] )

open3d()
points3d(d, size = 0.01)
axes3d(box = F, labels = F, tick = F, expand = 2)
title3d(main = NULL, zlab = "elev", line =1, level = 3)



plot3d(d, size = 1, col = "steelblue", type = "p")
play3d(spin3d(axis = c(0,0,1), rpm = 10), duration = 5)


open3d()
bg3d(color = "black")
points3d(d$x,d$y,d$alps_20m*100, size = 1, col = "white")
# pch3d(d$x,d$y,d$alps_20m*100, col = "white", pch = 16, cex = 0.003) # too slow
aspect3d(1,1,1) # adjust axis ratios
grid3d(side = "z-") # add horizontal grid at the bbox bottom
observer3d(0,-5000,150000) # adjust viewpoint
observer3d(auto = T) # reset viewpoint
# view3d()
par3d(mouseMode = c("none","trackball","zAxis","selecting","zoom")) # c("none", "left", "right", "middle", "wheel")


play3d(spin3d(axis = c(0,0,1), rpm = 10), duration = 5)


rgl::clear3d()
rgl::close3d()
rgl::.check3d()



rgl::view3d()
rgl::tkspin3d()
rgl::tkspinControl()
rgl::title3d()
rgl::spin3d()
rgl::snapshot3d()
rgl::shapelist3d()
rgl::selectpoints3d()
rgl::selectionFunction3d()
rgl::select3d()
rgl::rotate3d()
rgl::playwidgetOutput()
rgl::propertyControl()
rgl::par3d()
rgl::movie3d()
rgl::ids3d()
rgl::identify3d()
rgl::hover3d()
rgl::contourLines3d()
rgl::cur3d()





if (interactive() && !in_pkgdown_example()) {
  x <- d$x
  y <- d$y
  z <- d$alps_20m*100
  #  open3d()
  #  points3d(x, y, z)
  plot3d(x,y,z, size = 0.001)
  f <- select3d()
  if (!is.null(f)) {
    keep <- f(x, y, z)
    pop3d()
    points3d(x[keep], y[keep], z[keep], color = 'red')
    points3d(x[!keep], y[!keep], z[!keep])
  }
}






mm_map = function (x,y,z,r,type = "polygons") {  
  # create 3d plot
  plot3d(x,y,z, size = 0.01, col = "white"); clear3d(); points3d(x,y,z, size = 0.01, col = "black")
  
  repeat{
  
  # select points in 3d plot
  f <- select3d(button = c("middle"), dev = cur3d())

  if (!is.null(f)) {
    keep <- f(x, y, z)
    
    # 3d plot with selected points highlighted
    clear3d()
    points3d(x[keep], y[keep], z[keep], color = 'red', size = 0.01)
    points3d(x[!keep], y[!keep], z[!keep], color = "black", size = 0.01)
    
    #### map control plot
    # get selected points
    mapped = data.frame(x = x[keep], y = y[keep], z = z[keep])
    # convert to points - raster - polygons
    map_points = terra::vect(data.frame(x=mapped$x,y=mapped$y), geom=c("x","y"), crs = crs(r,proj=T) )
    map_raster = terra::rasterize(map_points, r)
    map_polygons = terra::as.polygons(map_raster)
    # create plot
    terra::plot(r, col = grey.colors(256, rev = T))
    terra::polys(map_polygons,col="#ff000033", border="#00000033")
  }
  
  continue = readline(prompt = "Continue? y/n")
  if (continue == "n") { break }
  
  }  
  
  if (exists("map_points")) {  # return polygons, points or raster
  if (type=="polygons"){return(map_polygons)} else if (type=="points"){return(map_points)} else if (type=="raster"){return(map_raster)}
  }
  
}





mm_map = function (x,y,z,r,type = "polygons") {  
  # create 3d plot
  plot3d(x,y,z, size = 0.01, col = "white"); clear3d(); points3d(x,y,z, size = 0.01, col = "black")
  
  repeat{
    
    # select points in 3d plot
    f <- select3d(button = c("middle"), dev = cur3d())
    
    if (!is.null(f)) {
      
      if (exists("keep")) { old_keep = keep } # keep the old keep from the previous tun, if there has already been a run.
      
      keep <- f(x, y, z)
      
      # edit keep, to remove points from previous selection
      if (exists("continue") & continue == "r") {
        onkeep = data.frame(old_keep, keep, final = rep(FALSE, length(old_keep)) )
        onkeep[onkeep[[1]] != onkeep[[2]] & onkeep[[1]] == TRUE,][[3]] = TRUE
        keep = onkeep[[3]]
      }
      
      # edit keep, to add points to previous selection
      if (exists("continue") & continue == "a") {
      onkeep = data.frame(old_keep, keep, final = rep(TRUE, length(old_keep)) )
      onkeep[onkeep[[1]] == onkeep[[2]] & onkeep[[2]] == FALSE,][[3]] = FALSE
      keep = onkeep[[3]]
      }
      
      #### update 3d plot with selected points highlighted
      clear3d()
      points3d(x[keep], y[keep], z[keep], color = 'red', size = 0.01)
      points3d(x[!keep], y[!keep], z[!keep], color = "black", size = 0.01)
      
      #### map control plot
      # get selected points
      mapped = data.frame(x = x[keep], y = y[keep], z = z[keep])
      # convert to points - raster - polygons
      map_points = terra::vect(data.frame(x=mapped$x,y=mapped$y), geom=c("x","y"), crs = crs(r,proj=T) )
      map_raster = terra::rasterize(map_points, r)
      map_polygons = terra::as.polygons(map_raster)
      # create plot
      terra::plot(r, col = grey.colors(256, rev = T))
      terra::polys(map_polygons,col="#ff000033", border="#00000033")
    }
    
    continue = readline(prompt = "do you want to: remove points / add points / start new / end selection? r/a/s/e ")
    if (continue == "e") { break }
    
  }  
  
  if (exists("map_points")) {  # return polygons, points or raster
    if (type=="polygons"){return(map_polygons)} else if (type=="points"){return(map_points)} else if (type=="raster"){return(map_raster)}
  }
  
}


DEM = terra::as.points(ras[[1]])
DEM$alps_20m



d = data.frame( geom(rapp)[,c(3,4)], rapp[[1]] )
x=d[[1]]
y=d[[2]]
z=d[[3]]

k = mm_map(d$x,d$y,d[[3]],ras$hillshade)

t = "r"

if (exists("t") & t == "r") {"ok"}

?select3d


dev.new()


identical(old_keep, new_keep)
old_keep = keep
new_keep = keep

# T  T -> T
# F  F -> F
# T  F -> T
# F  T -> T



k = NULL
for (x in old_keep) {
  if (x) { 
    if (new_keep[[x]]) { k = c(k,F) } else { k = c(k,T) }
    } else { k = c(k,F) }
}

onkeep = data.frame(old_keep, new_keep, final = rep(TRUE, length(old_keep)) )
onkeep[onkeep[[1]] == onkeep[[2]] & onkeep[[2]] == FALSE,][[3]] = FALSE
keep = onkeep[[3]]

unique(keep)

unique(new_keep)
unique(old_keep)


for (i in 1:length(onkeep[[1]])) {
  if (onkeep[[3]][i]) {
    if (!onkeep[[1]][i]) {
      onkeep[[3]][i] = FALSE
    }
  }
}

onkeep[onkeep[[1]] == onkeep[[3]],][onkeep[[1]] == FALSE,]


t = data.frame(x = c(T,F,T,F), y = c(T,F,F,T), z = c(F,F,T,T))
t

t2 = t[t[[1]] == t[[3]],]
t2[t2[[1]] == FALSE,]

t[t[[1]] != t[[2]] & t[[1]] == TRUE,][[3]] = TRUE
t

onkeep[[3]][400] = TRUE
onkeep[[3]][400]
keep = onkeep[[3]]

old_keep
new_keep

onkeep[[3]]
unique(onkeep[[3]])


unique(keep_)

old_keep != new_keep


  

length(k)
length(old_keep)



################################################################################
############################################################################ END









#### load old pmt3 functions ###################################################
#install.packages("sm")
require(readtext)
pmtPath = "H:/R_Package_Devel/prePackageVersion"
# get FLAGG specific styles and filter expressions:
eval(parse(text = readtext(paste0(pmtPath,"/","FLAGGStyles/FLAGGSpecific.R"),verbosity = 0)[[2]]))
temp<-readtext(paste0(pmtPath,"/","filterExpressions/*.R"), verbosity = 0);for (i in 1:length(temp[,2])){eval(parse(text=temp[i,2]))};rm(temp)
# ... and specific functions:
temp<-readtext(paste0(pmtPath,"/","FLAGGStyles/*.R"), verbosity = 0);for (i in 1:length(temp[,2])){eval(parse(text=temp[i,2]))};rm(temp)
# Laden des PMT pt.2 Toolsets und aller noetigen R Packages
temp<-readtext(paste0(pmtPath,"/","Functions.R/pt2/*.R"), verbosity = 0);for (i in 1:length(temp[,2])){eval(parse(text=temp[i,2]))};rm(temp)
pmt.packages()
eval(parse(text = readtext(paste0(pmtPath,"/","Functions.R/p3d/xloc_yloc.R"),verbosity = 0)[[2]]))
# load list of DS terrace ids for filtering purposes
input.path <- paste0(pmtPath,"/","FLAGGStyles/TerraceIds_exportFromQGIS")
load(paste0(input.path,"/","dsIds.Rdata"));load(paste0(input.path,"/","htIds.Rdata"));load(paste0(input.path,"/","ntIds.Rdata"))




#### plot alternatives #########################################################
# leaflet plot (with terra)
# terra::plet requires a devel version of leaflet
# remotes::install_github("rstudio/leaflet")
m = plet(ras$hillshade, col = grey.colors(256, rev = T) )
m = lines(m, profile_line, lwd=2, col= "black")
m = lines(m, profile_buffer, lwd=1, col="black")
points(m, profile_metering, cex=2, col="white")




# create a 3d plot
#install.packages("rgl")
require("rgl")
d = cbind( as.data.frame( terra::geom(terra::as.points(ras[[1]]))[,c(3,4)] ), z = terra::as.points(ras[[1]]))
rgl::plot3d(d, size = 0.01)
r = cbind( as.data.frame( terra::geom(rivLidar)[,c(3,4)] ), z = rivLidar$lidar)
rgl::points3d(r,col="blue",size=7)
i = cbind( as.data.frame( terra::geom(iceLidar)[,c(3,4)] ), z = iceLidar$lidar)
rgl::points3d(i,col="steelblue",size=7)
n = cbind( as.data.frame( terra::geom(nam)[,c(3,4)] ), z = nam$lidar)
rgl::points3d(n,col="darkgreen",size=20)




rm(list=ls())


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
################################################################################


setwd("H:/TEMP/MAMOT")
setwd("H:/R_Package_Devel/MAMOTerrace")

# install MAMOTerrace
library(roxygen2)
library(devtools)
roxygenise()
load_all(".")
install_github("pollhammerth/MAMOTerrace")


require("MAMOTerrace")
require("terra")
require("s2")
require("sf")
require("rgl")

require("raster") # for mm_sampleLines() methods::as("SpatVector", "Spatial")

rm(para)
para$p360
# NAF set parameters for data preparation and projection #######################
para = list(
  ar = 40,                                                                                                        # ar = Analysis resolution
  sr = 8000,                                                                                                      # sr = Search radius (radius of buffer around profile line)
  pp = "notInPackage/helperline.gpkg",                                               # pp = path to profile line
  pr = NA,                                                                                                        # pr = path to one or more rasters
  pm = list(m1 = "H:/GIS/PhD/maps/flagg/terraces.gpkg", m2 = "H:/GIS/Coops/Ewelina/maps/mapV3/terraces_CH25_BW50_noLake_inclSchrotz.gpkg", m3 = "notInPackage/helperolygon.gpkg"),# m3 = "notInPackage/Qmap.gpkg"), # pm = path(s) to map(s)
  mf = list(m1 = "NAME_KURZ", m2 = "Strat"),# m3 = "names"),                                             # mf = field for each map
  pl = list(l1 = "H:/GIS/PhD/maps/flagg/Ice/MaxIceExtent.gpkg", l2 = "H:/GIS/PhD/maps/hydro/rivers_OPAL.gpkg"),   # pl = path(s) to line(s)
  po = list(p1 = "H:/GIS/PhD/maps/literatureFigs/haeuselmann2007/Haeuselmann2007_locations.gpkg", p2 = "notInPackage/helperoint.gpkg"),                 # po = path(s) to point(s)
  p360 = list(X = if(exists("para")){para$p360$X}else{780707}, 
              Y = if(exists("para")){para$p360$Y}else{5409520}, 
              radius = 13000, 
              count = 48)
)
# choose alpine lidar path suiting analysis resolution (ar) and add it in front of raster paths in para$pr (comment out, if you want to specify lidar in para$pr[1])
if ( para$ar <  10 | para$ar == 15         ) { lidar = c("H:/GIS/DEMs/5m/alps_5m.tif")   } else if (
     para$ar <  20 | para$ar == 30         ) { lidar = c("H:/GIS/DEMs/10m/alps_10m.tif") } else if (
     para$ar <  50 | para$ar %in% c(60, 80)) { lidar = c("H:/GIS/DEMs/20m/alps_20m.tif") } else if (
     para$ar >= 50                         ) { lidar = c("H:/GIS/DEMs/50m/alps_50m.tif") }
if ( is.na(para$pr) ) { para$pr = lidar } else { para$pr = c(lidar, para$pr) }; rm(lidar)



#### long-profile projection ###################################################
#rm(lp)
if(!exists("lp")) { lp = list() } # list, to store all longProfile data

# load and prepare (buffer, metering) profile
lp[["pro"]] = list( line = vect(para$pp), 
            buffer = buffer(x = vect(para$pp), width = para$sr, capstyle = "flat", joinstyle = "round"), 
            labels = mm_metering(profile_line = vect(para$pp), spacing = 5000, label = T, labelUnit = "km") ) # --------------------------------------------- optional: edit spacing [m]

# load clip resample rasters
lp[["ras"]] = mm_prepRas(para$pr, para$ar, lp$pro$buffer, makeSlope = T, makeShade = T)

# stage objects for plotting
pro = lp$pro; ras = lp$ras
# test plot to see if data makes sense so far, using terra::plot or alternatively terra::plet
plot(ras$hillshade, col=grey.colors(256,rev=T)); polys(pro$buffer, lty=3); lines(pro$line, lwd=1); points(pro$labels, cex=2, col="white"); text(pro$labels, labels=pro$labels$label, halo=T, cex=0.5); north(); sbar()
plet(ras$hillshade, col=grey.colors(256,rev=T)) %>% lines(pro$line, lwd=2, col="black") %>% lines(pro$buffer, lwd=1, col="black") %>% points(pro$labels, cex=2, col="white")

# project rasters on to profile line
st=Sys.time(); lp$ras = mm_linRef(p = lp$ras, l = lp$pro$line, addz = T, asVector = F); et=Sys.time(); et-st; rm(list = c("et","st"))

# load clip rasterize map(s)
lp[["maps"]] = list(); for (i in 1:length(para$pm)) { 
  lp$maps[[paste0("m",i)]] = mm_prepMap(map = para$pm[[i]], field = para$mf[[i]], cropper = lp$pro$buffer, aligner = lp$ras, asVector = F) }

# load sample project lines
lp[["lns"]] = list(); for (i in 1:length(para$pl)) { # load and sample
  lp$lns[[paste0("l",i)]] = mm_sampleLines(l = para$pl[[i]], cropper = lp$pro$buffer, density = 0.1, raster = lp$ras[[1]]) }
for (i in 1:length(lp$lns)) { lp$lns[[i]] = mm_linRef(p = lp$lns[[i]], l = lp$pro$line, addz = T, asVector = T) } # project sampled lines

# load, add elevation and project points
lp[["pts"]] = list(); for (i in 1:length(para$po)) { lp$pts[[paste0("p",i)]] = crop(vect(para$po[[i]]), lp$pro$buffer) } # load points (if multipart points create problems, add terra::disagg(), to convert to singlepart)
for (i in 1:length(lp$pts)) { lp$pts[[i]][["rastValu"]] = extract(lp$ras[[1]], lp$pts[[i]], method = "bilinear")[,2] } # extract raster values
for (i in 1:length(lp$pts)) { lp$pts[[i]] = if (length(lp$pts[[i]]) == 0) { NA } else { mm_linRef(p = lp$pts[[i]], l = lp$pro$line, addz = T, asVector = T) } } # project points



#### 360 profile projection ####################################################
#rm(p360)
if(!exists("p360")) { p360 = list() } # list, to store all 360 profile data

# load and prepare (buffer, labels) profile
crs = crs(rast(para$pr))
p360$pro = list(line = mm_360lines(X = para$p360$X, Y = para$p360$Y, radius = para$p360$radius, count = para$p360$count, crs = crs),
              buffer = mm_360buffer(X = para$p360$X, Y = para$p360$Y, radius = para$p360$radius, crs = crs),
              labels = mm_360ids(X = para$p360$X, Y = para$p360$Y, radius = para$p360$radius, count = para$p360$count, crs = crs))

# load clip resample rasters
p360$ras = mm_prepRas(para$pr, para$ar, p360$pro$buffer, makeSlope = T, makeShade = T)


# project rasters on to 360 profiles
# convert to sf for parallel processing (SpatRaster and SpatVector cannot be exported to parallel workers! But sf can.)
ras360_sf = terra::as.points(p360$ras) %>% sf::st_as_sf(); pro360_sf = st_as_sf(p360$pro$line)
# ids = sub('^.','',as.character(seq(1001,(1000+length(p360$pro$line))))) # vector of ids for 360 profiles. Will be added in parallel output.

# register parallel computing
require(parallel)
parallel::detectCores()
numCores = 14
require(doParallel)
cl = parallel::makeCluster(numCores)
doParallel::registerDoParallel(cl)
# require(iterators); require(foreach) # are loaded with require(doParallel)

# project 360 profiles, using parallel computing
st=Sys.time(); vec360p = foreach (i = 1:48, .packages = c("terra","s2","MAMOTerrace", "sf"), .combine = 'list', .multicombine = T) %dopar% { 
  x = s2::s2_project(pro360_sf[pro360_sf$id == i,], ras360_sf)
  # z = s2::s2_distance(pro360_sf[pro360_sf$id == i,], ras360_sf) # Takes a little processing time. Terra is faster!
  z = as.vector( terra::distance( vect(pro360_sf[pro360_sf$id == i,]), vect(ras360_sf) ) )
  sf::st_as_sf( cbind(vect(ras360_sf), as.data.frame(x), as.data.frame(z)) )
#  r = vect(ras360_sf); p = vect(pro360_sf[pro360_sf$id == i,])
#  mm_linRef(p = r, l = p, addz = T, asVector = T) %>% st_as_sf()
}; et=Sys.time(); et-st; rm(list = c("et","st"))
p360$vec = vec360p; rm(list = c("ras360_sf", "pro360_sf", "vec360p"))

# unregister parallel computing
parallel::stopCluster(cl)
foreach::registerDoSEQ()


# load clip rasterize map(s)
p360$maps = list(); for (i in 1:length(para$pm)) { 
  p360$maps[[paste0("m",i)]] = mm_prepMap(map = para$pm[[i]], field = para$mf[[i]], cropper = p360$pro$buffer, aligner = p360$ras, asVector = F) }

# load sample project lines
p360_lns = list(); for (i in 1:length(para$pl)) { # load and sample
  p360_lns[[paste0("l",i)]] = mm_sampleLines(l = para$pl[[i]], cropper = p360$pro$buffer, density = 0.1, raster = p360$ras[[1]]) }
for (j in 1:length(p360$pro$line)) {
  for (i in 1:length(p360_lns)) { p360$lns[[p360$pro$line$id_chr[j]]][[paste0("l",i)]] = mm_linRef(p = p360_lns[[i]], l = p360$pro$line[p360$pro$line$id == j], addz = T, asVector = T) } # project sampled lines
}; rm('p360_lns')

# load, add elevation and project points
p360_pts = list(); for (i in 1:length(para$po)) { p360_pts[[paste0("p",i)]] = crop(vect(para$po[[i]]), p360$pro$buffer )} # load points (if multipart points create problems, add terra::disagg(), to convert to singlepart)
for (i in 1:length(p360_pts)) { p360_pts[[i]][["rastValu"]] = extract(p360$ras[[1]], p360_pts[[i]], method = "bilinear")[,2] } # extract raster values
for (j in 1:length(p360$pro$line)) {
  for (i in 1:length(p360_pts)) { p360$pts[[p360$pro$line$id_chr[j]]][[paste0("p",i)]] = if (length(p360_pts[[i]]) == 0) { NA } else { mm_linRef(p = p360_pts[[i]], l = p360$pro$line[p360$pro$line$id == j], addz = T, asVector = T) } } # project points
}; rm('p360_pts')



#### staging single 360 profile or long profile for evaluation #################
mm_stage = function (choose360) { # provide 360 id or nothing for longProfile
  pro <<- list(); ras <<- list(); maps <<- list(); lns <<- list(); pts <<- list()
  if (missing(choose360)) { # stage longProfile
    pro <<- lp$pro; ras <<- lp$ras; maps <<- lp$maps; lns <<- lp$lns; pts <<- lp$pts
  } else { # stage 360
    pro$line <<- p360$pro$line[p360$pro$line$id == choose360]; pro$buffer <<- p360$pro$buffer; pro$labels <<- p360$pro$labels
    ras <<- terra::rasterize(p360$vec[[choose360]], p360$ras, c(names(p360$ras),"x","z"))
    maps <<- p360$maps
    lns <<- p360$lns[[p360$pro$line$id_chr[choose360]]]
    pts <<- p360$pts[[p360$pro$line$id_chr[choose360]]]
  }
}
mm_stage(1)



# mm_f() is used to filter projected data and prepare it for use with old pmt pt.2 functions, but can also be used for prep for mm_map3d(). It requires stds list.
#?mm_f()
# set standard values for mm_f() (also mm_bab(), which is made to be used within mm_f())
stds = list( # v, cr
  projectedRaster = "ras", # x
  yValue = names(ras)[1], # y
  rasterizedMaps = "maps", # m
  mapNumber = 1, # n
  mapField = para$mf$m1, # f
  orographicSide = 3
)


# 3D plotting and terrace mapping ##############################################
clear3d()
d = mm_f(n=0); d = d[d$slope <= 90,] # convert raster to data.frame
d = mm_f(n = 0, s = 2)
d = mm_f("02_HT", s = 1)
#d = data.frame( geom(as.points(ras))[,c(3,4)], as.points(ras)[[1]] ) # if you do not want to use mm_f()
#d = d[d$y<360,]
mapped = mm_map3d(x=d$X, y=d$Y, z=d$y, r=ras$hillshade, type="polygons")
#### space for map editing ############################################### START

map = cbind(mapped, data.frame(names = "T1"))
map = disagg(map)
writeVector(vect("notInPackage/Qmap.gpkg"), "notInPackage/Qmap_backup.gpkg", overwrite = T)
map = rbind(vect("notInPackage/Qmap.gpkg"), map)
plet(map)
writeVector(map, "notInPackage/Qmap.gpkg", overwrite = T)

writeVector(map, "H:/TBA/ArcticDEM/Iceland/MAMU/mapout.gpkg", overwrite = T)
?writeVector()

############################################################################ END

# plot data with pmt3 pt.2
# example using pmt functions with mm_f()
dev.new()
dev.cur()
dev.set(which = dev.prev())

# 25k map CH
d = mm_f("NT"); d[d$slope <= 2,] %>% pmt.plot(col="#98c872", cex=20)
d = mm_f("HT"); d[d$slope <= 90,] %>% pmt.plot(col="#5b8cbb", cex=20)
d = mm_f("TDS"); d[d$slope <= 90,] %>% pmt.plot(col="#907a58", cex=20)
d = mm_f("HDS"); d[d$slope <= 90,] %>% pmt.plot(col="#dd4243", cex=20)

# 50 - 200k compiled map NAF
d = mm_f("03c_TDS_Mindel_Guenz_1800_780_ka"); d[d$slope <= 90,] %>% pmt.plot(col="#907a58", cex=20)
d = mm_f("06a_HDS_Donau_Biber_1800_780_ka"); d[d$slope <= 90,] %>% pmt.plot(col="#dd4243", cex=20)
d = mm_f("10a_Hoehenschotter"); d[d$slope <= 90,] %>% pmt.plot(col="#bbed26", cex=20)
d = mm_f("10_Altplei_Plio"); d[d$slope <= 90,] %>% pmt.plot(col="#398017", cex=20)
d = mm_f("06_HADS"); d[d$slope <= 90,] %>% pmt.plot(col="#ad3a01", cex=20)
d = mm_f("05_TADS"); d[d$slope <= 90,] %>% pmt.plot(col="#ff0101", cex=20)
d = mm_f("03_JDS"); d[d$slope <= 90,] %>% pmt.plot(col="#ffaf01", cex=20)
d = mm_f("02_HT"); d[d$slope <= 90,] %>% pmt.plot(col="#5b8cbb", cex=20)
d = mm_f("01_NT"); d[d$slope <= 90,] %>% pmt.plot(col="#98c872", cex=20)

# full DEM
mm_f(n=0) %>% pmt.plot(col="#00000033", cex=20)

# modelling
d = mm_f("01_NT"); d = d[d$slope <= 2,]
b = pmt.bin(d, interval = 200, value = "median", mode = "bin", cth = NA, sth = NA)
m = pmt.model( b, deg = 1)
pmt.plotModel(m, col = "blue",conf=F)


#### stage a profile, plot and evaluate it #####################################
mm_stage()
rap = as.points(ras); d = data.frame(rap$x, rap[[1]], rap$z); names(d) = c("x","y","z"); extent = pmt.extent(d); rm(rap); rm(d) # auto set plot extent for pmt3

gr=F;px=T

plot01 <- function(gr = T, px=T){
  pmt.empty(grid=gr,main="")
  if(px){
    s=40
#  d = mm_f("10a_Hoehenschotter"); d[d$slope <= 90,] %>% pmt.plot(col="#bbed26", cex=s)
  d = mm_f("10_Altplei_Plio"); d[d$slope <= 90,] %>% pmt.plot(col="#398017", cex=s)
#  d = mm_f("06_HADS"); d[d$slope <= 90,] %>% pmt.plot(col="#ad3a01", cex=s)
  d = mm_f("05_TADS"); d[d$slope <= 90,] %>% pmt.plot(col="#ff0101", cex=s)
  d = mm_f("03_JDS"); d[d$slope <= 90,] %>% pmt.plot(col="#ffaf01", cex=s)
  d = mm_f("02_HT"); d[d$slope <= 90,] %>% pmt.plot(col="#5b8cbb", cex=s)
  d = mm_f("01_NT"); d[d$slope <= 90,] %>% pmt.plot(col="#98c872", cex=s)
  
#  points(pts$p1[[c("x","elev")]], pch=21, bg = "yellow", col = "black", lwd = 2, cex = 2) # outcrops
  
  points(lns$l1$x,lns$l1$rastValu,type="p",col = "#99f1ff66", cex = s/100, pch = 16) # ice extent
  
  polys(b, col = "#99112288", border = NA)
  
  mm_f(n=0) %>% pmt.plot(col="#00000011", cex=s/2)
  
  }  
  
}



#### 2D mapping
l = pmt.drawLine()
#l <- data.frame(x = c(5035,5808,6796,8306,9520,10983,11911,12779,13445,14825,16181,17359,18680,19833,21023,21915,23295,24877,26269,27958,29957,30968,32645,33978,35441,36405,38415,39783,41377), y = c(514,508,500,488,479,469,462,457,453,448,442,437,432,427,424,422,419,414,412,408,404,403,402,400,399,398,397,397,396))
#l <- data.frame(x = c(4845,5713,7521,8949,10483,11792,13219,15170,16919,18346,20000,21356,23390,25080,26091,27685,29445,31016,33062,34537,36131,38546,39700,40937,41389), y = c(516,509,495,483,472,462,454,446,439,433,426,421,415,410,408,406,405,403,401,400,398,397,397,396,395))
l <- data.frame(x = c(5059,6522,8651,10507,12339,13636,15265,17050,18537,20012,21297,22403,23652,25270,26674,28422,30147,32110,33490,35167,36345,38106,39521,40794,41389), y = c(514,503,485,472,458,453,446,438,433,426,421,416,412,408,406,405,404,402,400,399,398,396,396,396,395))
b = buffer(mm_lv(l), width=2, capstyle = "flat", joinstyle = "round")

d = mm_f(n=0)
s = mm_d2s(d)
s = crop(s,b)
s = mm_s2d(s)

pmt.empty(grid=F); d %>% pmt.plot(col="#00000033", cex=20)
s[s$slope <= 0.2,] %>% pmt.plot(col="red", cex=20, add = T)

clear3d()
exagg = 30
sf = s[s$slope <= 0.2,]
points3d(sf$X,sf$Y,sf$y*exagg, size = 10, col = "#000000")
rgl::grid3d(side = c("x","y","z"))
rgl::axes3d()

p = mm_pm(mm_d2s(s[s$slope <= 90,]))

png(paste0("notInPackage/output/",proName,"_mapping_01.png"), width = 27*2/2.54, height = 19*2/2.54, res = 400, units = "in")
plot(ras$hillshade, col=grey.colors(256,rev=T))
polys(p, col = "#99112266", border = NA)
polys(lp$pro$buffer, lty=3); lines(lp$pro$line, lwd=1); points(lp$pro$labels, cex=2, col="white"); text(lp$pro$labels, labels=pro$labels$label, halo=T, cex=0.5); north(); sbar()
dev.off(); dev.set(which = dev.prev())




#### export plot and map #######################################################
# define export name
proName = "Inn_NT_Long_Section_1_optimized_mapping_01_pro"

# currently staged profile
rap = as.points(ras); d = data.frame(rap$x, rap[[1]], rap$z); names(d) = c("x","y","z"); extent = pmt.extent(d); rm(rap); rm(d) # auto set plot extent for pmt3
png(paste0("notInPackage/output/",proName,".png"), width = 20*2/2.54, height = 14*2/2.54, res = 400, units = "in"); plot01(gr=F,px=T); dev.off(); dev.set(which = dev.prev())
pdf(paste0("notInPackage/output/",proName,".pdf"), width = 10/2.54, height = 10/2.54); plot01(gr=T,px=F); dev.off(); dev.set(which = dev.prev())

# longProfile map
png(paste0("notInPackage/output/",proName,"_map.png"), width = 27*2/2.54, height = 19*2/2.54, res = 400, units = "in")
plot(ras$hillshade, col=grey.colors(256,rev=T))
polys(pro$buffer, lty=3); lines(pro$line, lwd=1, col = "#00000055"); points(pro$labels, cex=2, col="#ffffff44"); text(pro$labels, labels=pro$labels$label, halo=T, cex=0.7); north(); sbar()
dev.off(); dev.set(which = dev.prev())

# 360 profiles, all of them
if ( !dir.exists(paste0("notInPackage/output/",proName,"_360")) ) {
dir.create(paste0("notInPackage/output/",proName,"_360")) }
rap = as.points(ras); d = data.frame(rap$x, rap[[1]], rap$z); names(d) = c("x","y","z"); extent = pmt.extent(d); rm(rap); rm(d) # auto set plot extent for pmt3
for (i in seq(1,48,by=1)) {
  mm_stage(i)
  png(paste0("notInPackage/output/",proName,"_360/",p360$pro$line$id_chr[i],".png"), width = 20/2.54, height = 14/2.54, res = 400, units = "in"); plot01(gr=F,px=T); dev.off(); dev.set(which = dev.prev())
}

# 360 profiles map
png(paste0("notInPackage/output/",proName,"_360/_map360.png"), width = 27*2/2.54, height = 19*2/2.54, res = 400, units = "in")
plot(ras$hillshade, col=grey.colors(256,rev=T))
lines(p360$pro$line, col = "#00000022"); polys(p360$pro$buffer); points(p360$pro$labels, cex=2, col = "white"); text(p360$pro$labels, labels=as.numeric(p360$pro$labels$id), halo=T, cex=1); north(); sbar()
polys(lp$pro$buffer, lty=3); lines(lp$pro$line, lwd=1, col = "#00000055"); points(lp$pro$labels, cex=2, col="#ffffff44"); text(lp$pro$labels, labels=lp$pro$labels$label, halo=T, cex=0.7)
dev.off(); dev.set(which = dev.prev())


# Save data that you want to keep
#save.image(file= paste0("notInPackage/output/",proName,"_workspace_5m.Rdata" ) )
writeVector(pro$line,filename = paste0("notInPackage/output/",proName,"_profile.gpkg") )
writeVector(pro$buffer,filename = paste0("notInPackage/output/",proName,"_swath.gpkg") )
writeVector(pro$labels,filename = paste0("notInPackage/output/",proName,"_labels.gpkg") )
write.csv( t(as.data.frame(para)), file = paste0("notInPackage/output/",proName,"_para.csv" ) )



# some project specific map plot adds
# terrace map
m = terra::as.polygons(maps$m1)
polys(m[m$NAME_KURZ == "01_NT"], col = "#98c87288", border = NA)
polys(m[m$NAME_KURZ == "02_HT"], col = "#5b8cbb88", border = NA)
polys(m[m$NAME_KURZ == "03_JDS"], col = "#ffaf0188", border = NA)
polys(m[m$NAME_KURZ == "05_TADS"], col = "#ff010188", border = NA)
polys(m[m$NAME_KURZ %in% c("10_Altplei_Plio","06b_Altplei_Reu_Schn_Aich_Federn_Geier_u_m","11_Aeltere_TerrScho_Plio")], col = "#39801788", border = NA)
# LGM and penultimate glaciation ice extent
lines(l[l$name == unique(l$name)[4]], col = "#99f1ff66", lwd = 3); lines(l[l$name == unique(l$name)[5]], col = "#4ba8ff66", lwd = 3, lty = 2)


mm_loc360 = function() {
  para_OLD <<- para
  loc = locator()  
  para$p360$X <<- loc$x
  para$p360$Y <<- loc$y
}

mm_stage()
mm_loc360()

# write 360 orientations (either append to existing shapefile or if not existing, write new)
id = 8
if ( file.exists("notInPackage/output/_360orientations.gpkg") ) { writeVector( rbind( vect("notInPackage/output/_360orientations.gpkg"), p360$pro$line[p360$pro$line$id == id] ), filename = "notInPackage/output/_360orientations.gpkg", overwrite = T ) 
  } else { writeVector(p360$pro$line[p360$pro$line$id == id], filename = "notInPackage/output/_360orientations.gpkg" ) }



######## DEVEL #################################################################
# color coded 3d plot

stds$orographicSide = 3

dem = mm_f(n=0); dem = dem[dem$slope <= 90,]
y_upper_limit = 4000
dem = dem[dem$y <= y_upper_limit,]

clear3d()

exagg = 100

points3d(dem$X,dem$Y,dem$y*exagg, size = 0.01, col = "#000000")

d = mm_f("01_NT"); d = d[d$slope <= 1,]; d = d[d$y <=y_upper_limit,]; points3d(d$X,d$Y,d$y*exagg, size = 2, col = "#98c872")
d = mm_f("02_HT"); d = d[d$slope <= 1,]; d = d[d$y <=y_upper_limit,]; points3d(d$X,d$Y,d$y*exagg, size = 2, col = "#5b8cbb")
d = mm_f("03_JDS"); d = d[d$slope <= 90,]; d = d[d$y <=y_upper_limit,]; points3d(d$X,d$Y,d$y*exagg, size = 2, col = "#ffaf01")
d = mm_f("05_TADS"); d = d[d$slope <= 90,]; d = d[d$y <=y_upper_limit,]; points3d(d$X,d$Y,d$y*exagg, size = 2, col = "#ff0101")
#d = mm_f("05_TADS"); d = d[d$slope <= 90,]; d = d[d$y <=y_upper_limit,]; points3d(d$X,d$Y,d$y*exagg, size = 2, col = "#ff0101")
d = mm_f("10_Altplei_Plio"); d = d[d$slope <= 90,]; d = d[d$y <=y_upper_limit,]; points3d(d$X,d$Y,d$y*exagg, size = 2, col = "#398017")


rgl::grid3d(side = c("x","y","z"))
rgl::axes3d()

# plot dem as 3d surface #######################################################
open3d()

t = gsub(".*/", para$pr, replacement = ""); t = gsub("*.tif", t, replacement = ""); t # get dem name without path and extension

vals = 5 * as.vector(ras[[t]])
nc = ncol(ras)
nr = nrow(ras)
z = matrix(vals, nrow = nr, ncol = nc, byrow = T)
x <- 10 * (1:nrow(z))   # 10 meter spacing (S to N)
y <- 10 * (1:ncol(z))   # 10 meter spacing (E to W)

id <- persp3d(x, y, z, aspect = "iso", axes = FALSE, box = FALSE, polygon_offset = 1)
contourLines3d(id)     # "z" is the default function
filledContour3d(id, polygon_offset = 1, nlevels = 10, replace = TRUE)
################################################################################


# modelling - nls ##############################################################

unique(maps$m1$NAME_KURZ)
d = mm_f("03_JDS")
ms = 1
d[d$slope <= ms,] %>% pmt.plot(col="#5b8cbb", cex=20, add=F)
#d = d[d$x >= 45000,]
b = pmt.bin(d[d$slope <= ms,], interval = 50, value = "max", mode = "bin", idfield = "map.terrace", cth = 50, sth = 3)
pmt.plotBin(b, pcex = 1.5, fill = "blue")
m = pmt.nls(b, intercept = NA)
pmt.plotNls(m)
mc = pmt.nlsConf(b,bdata = c(0,1), bextra = c(27000,30000), alpha = 0.05)
pmt.plotNlsConf(mc, border = "black", fill ="#5b8cbb44")


save(list = c("b","m","mc"), file = paste0("notInPackage/output/",proName,"_model.Rdata"))


locator()


################################################################################


################################################################################







pmt.empty(grid=T,main="")
d = mm_f("SCH"); d[d$slope <= 4,] %>% pmt.plot(col="#98c872", cex=20)
d = as.data.frame(ras)[c("x",names(ras)[1])]; names(d)[2] = "y"; d %>% pmt.plot(col="#00000033", cex=1)
d = mm_f("SCH"); d = d[d$slope <= 4,]
b = pmt.bin(d, interval = 200, value = "median", mode = "bin", cth = NA, sth = NA)
m = pmt.model( b, deg = 1)
pmt.plotModel(m, col = "blue",conf=F)




pmt.measureSlope()

pmt.measureSpacing()

# Limmat  36 m; 6 promille
# Reuss   72 - 81 m; 15.4 promille (local, 1km length); 7.3 promille
# Buenz   46 m; 18.9 permil
# Aabach  34 m; 13.8 permil


rainbow(n=1, start = rnorm(n=1,mean=0.5, sd=0.15))
rnorm(n=1,mean=0.5, sd=0.15)


tc = function(x){ st=Sys.time(); eval(parse(text=x)); et=Sys.time(); et-st }







require(ggplot2)

p = ggplot(data=d, mapping = aes(x=x,y=y))
#p <- p + theme_bw() 
#p <- p + geom_point(size = 0.25)
#p = p + geom_density_2d_filled(adjust = 0.25)
#p = p + geom_bin_2d()
#p = p + geom_hex()
#p = p + geom_jitter()
#p = p + geom_smooth()
p <- p + geom_point(data=d, aes(x=x,y=y), col="#00000099", size = 0.01, pch= 46)
print(p)


dev.new()



?geom_density_2d_filled()
?ggplot2::geom_bin_2d()
?ggplot2::geom_blank()
?ggplot2::geom_dotplot()
?ggplot
?ggplot2::geom_function()
?ggplot2::geom_hex()
?ggplot2::geom_hline()
?ggplot2::geom_jitter()
?ggplot2::geom_qq()
?ggplot2::geom_raster()
?ggplot2::geom_smooth()



install.packages("plotly")
require(plotly)
plot_ly(data = d, x = ~x, y = ~y, size = I(0.00001), type = "scatter", color = I("black"), symbols = "46")

?plot_ly




#--- start plot 
p <- ggplot(data=A, aes(x=x,y=y))
p <- p + theme_bw() 
p <- p + geom_point(size = 0.25)
p <- p + geom_point(data=A, aes(x=x,y=y), col="red", size = 0.5)
#-- Plot
p <- p + geom_vline(xintercept=confl[1], col='grey35', linetype = "dashed")
p <- p + geom_vline(xintercept=confl[2], col='grey35', linetype = "dashed")
p <- p + geom_vline(xintercept=upr_data, col='blue', linetype = "dashed")
p <- p + geom_hline(yintercept=elev, col='grey35', linetype = "dashed")
p <- p + geom_line(data=A_PI, aes(x=x, y=lwr), col = coLor[54], linetype = "dashed")
p <- p + geom_line(data=A_PI, aes(x=x, y=upr), col = coLor[54], linetype = "dashed")
p <- p + geom_line(data=A_PI, aes(x=x, y=boot.lwr), col = coLor[13], linetype = "dashed")
p <- p + geom_line(data=A_PI, aes(x=x, y=boot.upr), col = coLor[13], linetype = "dashed")
p <- p + geom_line(data=A_PI, aes(x=x, y=Function), col = 'magenta', linetype = "dashed")
p <- p + labs(x="Terrace", y="height", title = paste("Prediction Interval for Terrace ", name))
p <- p + xlim(lwr_vis,upr_vis) + ylim(elev-10,max(A[A$x>=lwr_vis & A$x<=upr_vis,]$y))
print(p)





library(shiny)
library(plotly)

shinyApp(
  ui = shinyUI(fluidPage(
    titlePanel("Plotly test"),
    sidebarLayout(sidebarPanel(
      selectInput(
        "nb",
        "Number of points",
        choices = c(
          "1K"   = 1000,
          "10K"  = 10000,
          "100K" = 100000,
          "1M"   = 1000000,
          "10M"  = 10000000
        )
      )
    ),
    mainPanel(plotlyOutput("plot")))
  )),
  server = function(input, output, session) {
    output$plot <- renderPlotly(plot_ly(
      data.frame(x = 1:input$nb,
                 y = rnorm(input$nb)),
      x =  ~ x,
      y =  ~ y
    ) %>%
      add_lines())
  }
)







########################################################################## START
#### testing rgl package #######################################################


install.packages("mapedit")
install.packages("leafpm")


lidar = rast("H:/GIS/DEMs/5m/alps_5m.tif")
lidar = rast("H:/GIS/DEMs/10m/alps_10m.tif")
lidar = rast("H:/GIS/DEMs/20m/alps_20m.tif")
lidar = rast("H:/GIS/DEMs/50m/alps_50m.tif")

#m = plet(lidar, col = grey.colors(256, rev = T) )
plet(ras$hillshade, col = grey.colors(256, rev = T) ) %>% lines(profile$line, lwd=2, col= "black") %>%
lines(profile$buffer, lwd=1, col="black") %>% points(profile$labels, cex=2, col="white")





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





################################################################################
############################################################################ END













#### plot alternatives #########################################################
# leaflet plot (with terra)
# terra::plet requires a devel version of leaflet
# remotes::install_github("rstudio/leaflet")
m = plet(ras$hillshade, col = grey.colors(256, rev = T) )
m = lines(m, profile$line, lwd=2, col= "black")
m = lines(m, profile$buffer, lwd=1, col="black")
points(m, profile$labels, cex=2, col="white")





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




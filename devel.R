
########################### 360 profiles #######################################



# create 360 degree profile lines

#require(terra)

mm_360lines = function(X,Y,radius,count=48,crs='EPSG:32632') {
  # convert profile number to geographic angles [degrees, 0 = N, clockwise]
  geoangle = c(seq(0,180,by=180/count))[c(1:count)]
  # convert geographical angles to mathematical angles [pi, 0 = E, counterclockwise]
  mathangle = c(geoangle[geoangle >= 0 & geoangle <= 90]*(-1)+90, 
                360-geoangle[geoangle < 360 & geoangle > 90]+90)*pi/180
  # calculate coordinates of individual line start and end points with center at given map coordinates (~X+Y)
  XStart = cos(mathangle)*radius+X
  YStart = sin(mathangle)*radius+Y
  XEnd = cos(mathangle)*radius*(-1)+X
  YEnd = sin(mathangle)*radius*(-1)+Y
  # create a matrix of start and endpoints, that terra package can covert to SpatVector
  startpoints = data.frame(object = c(1:48), part = 1, x = XStart, y = YStart)
  endpoints = data.frame(object = c(1:48), part = 1, x = XEnd, y = YEnd)
  linecoordinates = rbind(startpoints[1,], endpoints[1,])
  for ( i in 2:count ) { linecoordinates = as.matrix( rbind(linecoordinates, startpoints[i,], endpoints[i,]) ) }
  # create SpatVector object with all profile lines
  pro360 = terra::vect(linecoordinates, "lines", crs = crs)
  # add ids to pro360 (also as character for backwards compatibility with pmt3)
  id_chr = sub('^.','', as.character( c(1001:(1000+48)) ) )
  id = as.data.frame(as.numeric(id_chr)); names(id) = "id"
  id_chr = as.data.frame(id_chr)
  pro360 = cbind(pro360,id_chr,id)
  return(pro360)
}


mm_360buffer = function(X,Y,radius,crs='EPSG:32632') {
  centerpoint = vect( x = as.matrix( data.frame( object = 1, part = 1, x = X, y = Y ) ), 'points', crs = crs )
  buffer = buffer(x = centerpoint, width = radius)
  return(buffer)
}


mm_360ids = function(X,Y,radius,count=48,crs='EPSG:32632') {
  # convert profile number to geographic angles [degrees, 0 = N, clockwise]
  geoangle = c(seq(0,180,by=180/count))[c(1:count)]
  # convert geographical angles to mathematical angles [pi, 0 = E, counterclockwise]
  mathangle = c(geoangle[geoangle >= 0 & geoangle <= 90]*(-1)+90, 
                360-geoangle[geoangle < 360 & geoangle > 90]+90)*pi/180
  # calculate coordinates of individual line start and end points with center at given map coordinates (~X+Y)
  XStart = cos(mathangle)*radius+X
  YStart = sin(mathangle)*radius+Y
  # create a matrix of start and endpoints, that terra package can covert to SpatVector
  startpoints = data.frame(object = c(1:48), part = 1, x = XStart, y = YStart)
  startpoints = as.matrix(startpoints)
  # create SpatVector object with all profile lines
  ids360 = terra::vect(startpoints, "points", crs = crs)
  # add ids to ids360 (for backwards compatibility with pmt3)
  id = sub('^.','', as.character( c(1001:(1000+count)) ) ); id = as.data.frame(id)
  ids360 = cbind(ids360,id)
  return(ids360)
}







############################ 2D mapping from profiles ##########################
# # prepare data
# d = mm_f(n=0)
# d = d[d$x <= 60000,]
# d = d[d$x >= 5000,]
# extent$xlim = c(5000,60000)
# extent$ylim = c(400,530)
# pmt.empty(grid=F)
# d %>% pmt.plot(col="#00000033", cex=20)

#pmt.drawLine()
#l = data.frame(x = c(9995,10196,10412,10532,10682,10845,10957), y = c(459,459,459,460,459,459,459))

# convert pmt.drawLine() output to SpatVect
mm_lv = function(l) {
  ml = as.matrix( cbind(object = 1, part = 1, l) )
  vml = vect(ml, type = "lines")
  return(vml)
}
#mm_lv(l)

#bvml = buffer(mm_lv(l), width=5, capstyle = "flat", joinstyle = "round")

# convert profile data to SpatVector of points
mm_d2s = function(d) {
sp::coordinates(d) = c("x", "y")
d = vect(d)
return(d)
}
#s = crop(mm_d2s(d), bvml)

# convert profile data to map polygon
mm_pm = function(s) {
  sdf = cbind(as.data.frame(s), crds(s)) # convert to data.frame
  # convert to SpatPoints - SpatRaster - SpatPolygons
  map_points = terra::vect(data.frame(x=sdf$X,y=sdf$Y), geom=c("x","y"), crs = crs(ras$hillshade,proj=T) )
  map_raster = terra::rasterize(map_points, ras$hillshade) # align raster to given raster r
  map_polygons = terra::as.polygons(map_raster)
  return(map_polygons)
}
#mm_pm(s)

# put the above in one function, to make it handier
mm_map2d = function(l, d, buffer) {
  s = crop(
    mm_d2s(d), 
    buffer(mm_lv(l), width=buffer, capstyle = "flat", joinstyle = "round") 
           )
  return( mm_pm(s) )
}
#p = pmt.drawLine() %>% mm_map2d(d,10)

# helper - object conversion
mm_s2d = function(x) {
  cbind(crds(x), as.data.frame(x))
}
#s = mm_s2d(s)
#s = mm_d2s(s)


# ########################## application testing #################################
# 
# #### 2D mapping
# l = pmt.drawLine()
# l <- data.frame(x = c(47507,49451,52200,55281,57335,59073), y = c(415,411,406,402,398,397))
# b = buffer(mm_lv(l), width=2, capstyle = "flat", joinstyle = "round")
# 
# s = mm_d2s(d)
# s = crop(s,b)
# s = mm_s2d(s)
# 
# pmt.empty(grid=F); d %>% pmt.plot(col="#00000033", cex=20)
# s[s$slope <= 0.2,] %>% pmt.plot(col="red", cex=20, add = T)
# 
# clear3d()
# exagg = 30
# sf = s[s$slope <= 0.2,]
# points3d(sf$X,sf$Y,sf$y*exagg, size = 10, col = "#000000")
# rgl::grid3d(side = c("x","y","z"))
# rgl::axes3d()
# 
# p = mm_pm(mm_d2s(s[s$slope <= 0.2,]))
# 
# plot(ras$hillshade, col=grey.colors(256,rev=T)); polys(pro$buffer, lty=3); lines(pro$line, lwd=1); points(pro$metering, cex=2, col="white"); text(pro$metering, labels=pro$metering$label, halo=T, cex=0.5); north(); sbar()
# polys(p, col = "#99112266")
# 

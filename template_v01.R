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

# NAF set parameters for data preparation and projection
para = list(
  ar =20,                                                                                                        # ar = Analysis resolution
  sr = 4000,                                                                                                      # sr = Search radius (radius of buffer around profile line)
  pp = "notInPackage/helperline.gpkg",                                               # pp = path to profile line
  pr = NA,                                                                                                        # pr = path to one or more rasters
  pm = list(m1 = "H:/GIS/PhD/maps/flagg/terraces.gpkg", m2 = "H:/GIS/Coops/Ewelina/maps/mapV3/terraces_CH25_BW50_noLake_inclSchrotz.gpkg"),# m3 = "notInPackage/Qmap.gpkg"), # pm = path(s) to map(s)
  mf = list(m1 = "NAME_KURZ", m2 = "Strat"),# m3 = "names"),                                             # mf = field for each map
  pl = list(l1 = "H:/GIS/PhD/maps/flagg/Ice/MaxIceExtent.gpkg", l2 = "H:/GIS/PhD/maps/hydro/rivers_OPAL.gpkg"),   # pl = path(s) to line(s)
  po = list(p1 = "H:/GIS/PhD/maps/literatureFigs/haeuselmann2007/Haeuselmann2007_locations.gpkg", p2 = "H:/GIS/Coops/Ewelina/ages/DS_sites_coordinates_epsg32632.gpkg")                 # po = path(s) to point(s)
)
# choose alpine lidar path suiting analysis resolution (ar) and add it in front of raster paths in para$pr (comment out, if you want to specify lidar in para$pr[1])
if ( para$ar <  10 | para$ar == 15         ) { lidar = c("H:/GIS/DEMs/5m/alps_5m.tif")   } else if (
     para$ar <  20 | para$ar == 30         ) { lidar = c("H:/GIS/DEMs/10m/alps_10m.tif") } else if (
     para$ar <  50 | para$ar %in% c(60, 80)) { lidar = c("H:/GIS/DEMs/20m/alps_20m.tif") } else if (
     para$ar >= 50                         ) { lidar = c("H:/GIS/DEMs/50m/alps_50m.tif") }
if ( is.na(para$pr) ) { para$pr = lidar } else { para$pr = c(lidar, para$pr) }; rm(lidar)



# ICELAND set parameters for data preparation and projection
para = list(
  ar = 8,                                                                                                        # ar = Analysis resolution
  sr = 600,                                                                                                      # sr = Search radius (radius of buffer around profile line)
  pp = "H:/TBA/ArcticDEM/Iceland/MAMU/helperline.gpkg",                                               # pp = path to profile line
  pr = "H:/TBA/ArcticDEM/Iceland/merged/15_54.tif",                                                                                                        # pr = path to one or more rasters
  pm = list(m1 = "H:/TBA/ArcticDEM/Iceland/MAMU/helpermap.gpkg"),# m3 = "notInPackage/Qmap.gpkg"), # pm = path(s) to map(s)
  mf = list(m1 = "name"),# m3 = "names"),                                             # mf = field for each map
  pl = list(l1 = "H:/GIS/PhD/maps/flagg/Ice/MaxIceExtent.gpkg", l2 = "H:/GIS/PhD/maps/hydro/rivers_OPAL.gpkg"),   # pl = path(s) to line(s)
  po = list(p1 = "H:/GIS/PhD/maps/literatureFigs/haeuselmann2007/Haeuselmann2007_locations.gpkg", p2 = "H:/GIS/Coops/Ewelina/ages/DS_sites_coordinates_epsg32632.gpkg")                 # po = path(s) to point(s)
)



# load and prepare (buffer, metering) profile
pro = list( line = vect(para$pp), 
            buffer = buffer(x = vect(para$pp), width = para$sr, capstyle = "flat", joinstyle = "round"), 
            metering = mm_metering(profile_line = vect(para$pp), spacing = 1000, label = T, labelUnit = "km") ) # --------------------------------------------- optional: edit spacing [m]

# load clip resample rasters
ras = mm_prepRas(profile=pro$line, para$sr, para$pr, para$ar, makeSlope = T, makeShade = T)

# test plot to see if data makes sense so far, using terra::plot or alternatively terra::plet
plot(ras$hillshade, col=grey.colors(256,rev=T)); polys(pro$buffer, lty=3); lines(pro$line, lwd=1); points(pro$metering, cex=2, col="white"); text(pro$metering, labels=pro$metering$label, halo=T, cex=0.5); north(); sbar()
plet(ras$hillshade, col=grey.colors(256,rev=T)) %>% lines(pro$line, lwd=2, col="black") %>% lines(pro$buffer, lwd=1, col="black") %>% points(pro$metering, cex=2, col="white")


# project rasters on to profile line
st=Sys.time(); ras = mm_linRef(p = ras, l = pro$line, addz = T, asVector = F); et=Sys.time(); et-st; rm(list = c("et","st"))

# load clip rasterize map(s)
maps = list(); for (i in 1:length(para$pm)) { 
  maps[[paste0("m",i)]] = mm_prepMap(map = para$pm[[i]], field = para$mf[[i]], cropper = pro$buffer, aligner = ras, asVector = F) }

#### not needed anymore. included in mm_f()
# add map, indicating (profile-)orographic left/right
# mm_bab = function(x = pro$line, width = para$sr, aligner = ras, side = 3){
#   oroA = terra::buffer(pro$line, capstyle = "flat", singlesided = T, width = para$sr, joinstyle = "round")
#   fullbuffer = buffer(x = x, width = width, capstyle = "flat", joinstyle = "round")
#   oroB = terra::erase(fullbuffer, oroA)
#   oroA[["side"]] = 1; oroB[["side"]] = 2
#   oro = terra::union(oroA,oroB)
#   if (side == 3) { return(oro) } else
#     if (side == 1) { return(oroA) } else
#       if (side == 2) { return(oroB) }
# }
# oro = mm_bab(); maps[["oro"]] = mm_prepMap(map = oro, field = "side", cropper = pro$buffer, aligner = ras, asVector = F)


# mm_f() is used to filter projected data and prepare it for use with old pmt pt.2 functions, and mm_map3d().
?mm_f()
# set standard values for mm_f() (also mm_bab())
stds = list( # v, cr
  projectedRaster = "ras", # x
  yValue = names(ras)[1], # y
  rasterizedMaps = "maps", # m
  mapNumber = 1, # n
  mapField = para$mf$m1, # f
  orographicSide = 3
)


# 3D plotting and terrace mapping
clear3d()
d = mm_f(n=0); d = d[d$slope <= 90,] # convert raster to data.frame
d = mm_f(n = 0, s = 2)
d = mm_f("02_HT", s = 1)
#d = data.frame( geom(as.points(ras))[,c(3,4)], as.points(ras)[[1]] )
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


# load sample project lines
lns = list(); for (i in 1:length(para$pl)) { # load and sample
  lns[[paste0("l",i)]] = mm_sampleLines(l = para$pl[[i]], cropper = pro$buffer, density = 0.1, raster = ras[[1]]) }
for (i in 1:length(lns)) { lns[[i]] = mm_linRef(p = lns[[i]], l = pro$line, addz = T, asVector = T) } # project sampled lines


# load, add elevation and project points
pts = list(); for (i in 1:length(para$po)) { pts[[paste0("p",i)]] = vect(para$po[[i]]) } # load points (if multipart points create problems, add terra::disagg(), to convert to singlepart)
for (i in 1:length(pts)) { pts[[i]][["rastValu"]] = extract(ras[[1]], pts[[i]], method = "bilinear")[,2] } # extract raster values
for (i in 1:length(pts)) { pts[[i]] = mm_linRef(p = pts[[i]], l = pro$line, addz = T, asVector = T) } # project points


# plot data with pmt3 pt.2
# example using pmt functions with mm_f()
dev.new()
dev.cur()
dev.set(which = dev.prev())
rap = as.points(ras); d = data.frame(rap$x, rap[[1]], rap$z); names(d) = c("x","y","z"); extent = pmt.extent(d); rm(rap); rm(d) # auto set plot extent for pmt3
pmt.empty(grid=T,main="")

d = mm_f("01_NT"); d[d$slope <= 2,] %>% pmt.plot(col="#98c872", cex=20)
d = mm_f("02_HT"); d[d$slope <= 90,] %>% pmt.plot(col="#5b8cbb", cex=20)
d = mm_f("03c_TDS_Mindel_Guenz_1800_780_ka"); d[d$slope <= 90,] %>% pmt.plot(col="#907a58", cex=20)
d = mm_f("06a_HDS_Donau_Biber_1800_780_ka"); d[d$slope <= 90,] %>% pmt.plot(col="#dd4243", cex=20)

d = mm_f("NT"); d[d$slope <= 2,] %>% pmt.plot(col="#98c872", cex=20)
d = mm_f("HT"); d[d$slope <= 90,] %>% pmt.plot(col="#5b8cbb", cex=20)
d = mm_f("TDS"); d[d$slope <= 90,] %>% pmt.plot(col="#907a58", cex=20)
d = mm_f("HDS"); d[d$slope <= 90,] %>% pmt.plot(col="#dd4243", cex=20)


d = mm_f("3 IP1"); d[d$slope <= 90,] %>% pmt.plot(col="#78cab7", cex=20)
d = mm_f("T2"); d[d$slope <= 90,] %>% pmt.plot(col="#e76787", cex=20)
d = mm_f("T1"); d[d$slope <= 90,] %>% pmt.plot(col="#bbed26", cex=20)


d = mm_f("10a_Hoehenschotter"); d[d$slope <= 90,] %>% pmt.plot(col="#bbed26", cex=20)
d = mm_f("10_Altplei_Plio"); d[d$slope <= 90,] %>% pmt.plot(col="#398017", cex=20)
d = mm_f("06_HADS"); d[d$slope <= 90,] %>% pmt.plot(col="#ad3a01", cex=20)
d = mm_f("05_TADS"); d[d$slope <= 90,] %>% pmt.plot(col="#ff0101", cex=20)
d = mm_f("03_JDS"); d[d$slope <= 90,] %>% pmt.plot(col="#ffaf01", cex=20)
d = mm_f("02_HT"); d[d$slope <= 90,] %>% pmt.plot(col="#5b8cbb", cex=20)
d = mm_f("01_NT"); d[d$slope <= 90,] %>% pmt.plot(col="#98c872", cex=20)


mm_f(n=0) %>% pmt.plot(col="#00000033", cex=20)




unique(maps$m1$NAME_KURZ)


d = mm_f("01_NT"); d = d[d$slope <= 2,]
b = pmt.bin(d, interval = 200, value = "median", mode = "bin", cth = NA, sth = NA)
m = pmt.model( b, deg = 1)
pmt.plotModel(m, col = "blue",conf=F)

points(pts$p1[[c("x","elev")]], pch=21, bg = "yellow", col = "blue", lwd = 2, cex = 2) # outcrops
points(as.numeric(pts$p2$x),as.numeric(pts$p2$elevation), pch=21, bg = "yellow", col = "blue", lwd = 2, cex = 2) # outcrops

points(lns$l1[[c("x","rastValu")]], pch = 46, col = "steelblue", cex = 2) # ice
points(lns$l2[[c("x","rastValu")]], pch = 46, col = "blue", cex = 2) # rivers






plot01 <- function(gr = T, px=T){
  pmt.empty(grid=gr,main="")
  if(px){
  s=10
#  d = mm_f("10a_Hoehenschotter"); d[d$slope <= 90,] %>% pmt.plot(col="#bbed26", cex=s)
  d = mm_f("10_Altplei_Plio"); d[d$slope <= 90,] %>% pmt.plot(col="#398017", cex=s)
#  d = mm_f("06_HADS"); d[d$slope <= 90,] %>% pmt.plot(col="#ad3a01", cex=s)
  d = mm_f("05_TADS"); d[d$slope <= 90,] %>% pmt.plot(col="#ff0101", cex=s)
  d = mm_f("03_JDS"); d[d$slope <= 90,] %>% pmt.plot(col="#ffaf01", cex=s)
  d = mm_f("02_HT"); d[d$slope <= 90,] %>% pmt.plot(col="#5b8cbb", cex=s)
  d = mm_f("01_NT"); d[d$slope <= 90,] %>% pmt.plot(col="#98c872", cex=s)
  
#  points(pts$p1[[c("x","elev")]], pch=21, bg = "yellow", col = "black", lwd = 2, cex = 2) # outcrops

  mm_f(n=0) %>% pmt.plot(col="#00000011", cex=5)
  }  
  
}

# define export name
proName = "Traun_HT"

# export plot and map
png(paste0("notInPackage/output/",proName,".png"), width = 27/2.54, height = 19/2.54, res = 400, units = "in"); plot01(gr=F,px=T); dev.off(); dev.set(which = dev.prev())
pdf(paste0("notInPackage/output/",proName,".pdf"), width = 10/2.54, height = 10/2.54); plot01(gr=T,px=F); dev.off(); dev.set(which = dev.prev())

png(paste0("notInPackage/output/",proName,"_map.png"), width = 27*2/2.54, height = 19*2/2.54, res = 400, units = "in")
plot(ras$hillshade, col=grey.colors(256,rev=T)); polys(pro$buffer, lty=3); lines(pro$line, lwd=1); points(pro$metering, cex=2, col="white"); text(pro$metering, labels=pro$metering$label, halo=T, cex=0.5); north(); sbar()
dev.off(); dev.set(which = dev.prev())

# Save data that you want to keep
#save.image(file= paste0("notInPackage/output/",proName,"_workspace_5m.Rdata" ) )
writeVector(pro$line,filename = paste0("notInPackage/output/",proName,"_profile.gpkg") )
writeVector(pro$buffer,filename = paste0("notInPackage/output/",proName,"_swath.gpkg") )
writeVector(pro$metering,filename = paste0("notInPackage/output/",proName,"_metering.gpkg") )
write.csv( t(as.data.frame(para)), file = paste0("notInPackage/output/",proName,"_para.csv" ) )




######## DEVEL #################################################################
# color coded 3d plot
dem = mm_f(n=0); dem = dem[dem$slope <= 90,]

clear3d()

exagg = 100

points3d(dem$X,dem$Y,dem$y*exagg, size = 0.01, col = "#000000")

d = mm_f("01_NT"); d = d[d$slope <= 1,]; points3d(d$X,d$Y,d$y*exagg, size = 2, col = "#98c872")
d = mm_f("02_HT"); d = d[d$slope <= 1,]; points3d(d$X,d$Y,d$y*exagg, size = 2, col = "#5b8cbb")
d = mm_f("03_JDS"); d = d[d$slope <= 90,]; points3d(d$X,d$Y,d$y*exagg, size = 2, col = "#ffaf01")
d = mm_f("05_TADS"); d = d[d$slope <= 90,]; points3d(d$X,d$Y,d$y*exagg, size = 2, col = "#ff0101")
d = mm_f("10_Altplei_Plio"); d = d[d$slope <= 90,]; points3d(d$X,d$Y,d$y*exagg, size = 2, col = "#398017")

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
lines(profile$buffer, lwd=1, col="black") %>% points(profile$metering, cex=2, col="white")





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
points(m, profile$metering, cex=2, col="white")





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




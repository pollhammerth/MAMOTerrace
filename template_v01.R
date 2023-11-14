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

# set parameters for data preparation and projection
para = list(
  ar = 40,                                                                                                        # ar = Analysis resolution
  sr = 5000,                                                                                                      # sr = Search radius (radius of buffer around profile line)
  pp = "H:/GIS/PhD/profiles/Iller/parallel_01/profile/profil.gpkg",                                               # pp = path to profile line
  pr = NA,                                                                                                        # pr = path to one or more rasters
  pm = list(m1 = "H:/GIS/PhD/maps/flagg/terraces.gpkg", m2 = "H:/GIS/PhD/maps/hydro/rivers_OPAL_buffer200.gpkg"), # pm = path(s) to map(s)
  mf = list(m1 = "NAME_KURZ",                           m2 = "name"),                                             # mf = field for each map
  pl = list(l1 = "H:/GIS/PhD/maps/flagg/Ice/MaxIceExtent.gpkg", l2 = "H:/GIS/PhD/maps/hydro/rivers_OPAL.gpkg"),   # pl = path(s) to line(s)
  po = list(p1 = "H:/GIS/PhD/maps/literatureFigs/haeuselmann2007/Haeuselmann2007_locations.gpkg")                 # po = path(s) to point(s)
)

# choose alpine lidar path suiting analysis resolution (ar) and add it in front of raster paths in para$pr (comment out, if you want to specify lidar in para$pr[1])
if ( para$ar <  10 | para$ar == 15         ) { lidar = c("H:/GIS/DEMs/5m/alps_5m.tif")   } else if (
     para$ar <  20 | para$ar == 30         ) { lidar = c("H:/GIS/DEMs/10m/alps_10m.tif") } else if (
     para$ar <  50 | para$ar %in% c(60, 80)) { lidar = c("H:/GIS/DEMs/20m/alps_20m.tif") } else if (
     para$ar >= 50                         ) { lidar = c("H:/GIS/DEMs/50m/alps_50m.tif") }
if ( is.na(para$pr) ) { para$pr = lidar } else { para$pr = c(lidar, para$pr) }; rm(lidar)

# load and prepare (buffer, metering) profile
pro = list( line = vect(para$pp), 
            buffer = buffer(x = vect(para$pp), width = para$sr, capstyle = "flat", joinstyle = "round"), 
            metering = mm_metering(profile_line = vect(para$pp), spacing = 5000, label = T, labelUnit = "km") ) # --------------------------------------------- optional: edit spacing [m]

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


# 3D plotting and terrace mapping
# d = mm_f(n=0) # convert raster to data.frame
d = data.frame( geom(as.points(ras))[,c(3,4)], as.points(ras)[[1]] )
mapped = mm_map3d(x=d[,1], y=d[,2], z=d[,3], r=ras$hillshade, type="polygons")


# load sample project lines
lns = list(); for (i in 1:length(para$pl)) { # load and sample
  lns[[paste0("l",i)]] = mm_sampleLines(l = para$pl[[i]], cropper = pro$buffer, density = 0.1, raster = ras[[1]]) }
for (i in 1:length(lns)) { lns[[i]] = mm_linRef(p = lns[[i]], l = pro$line, addz = T, asVector = T) } # project sampled lines


# load, add elevation and project points
pts = list(); for (i in 1:length(para$po)) { pts[[paste0("p",i)]] = vect(para$po[[i]]) } # load points (if multipart points create problems, add terra::disagg(), to convert to singlepart)
for (i in 1:length(pts)) { pts[[i]][["rastValu"]] = extract(ras[[1]], pts[[i]], method = "bilinear")[,2] } # extract raster values
for (i in 1:length(pts)) { pts[[i]] = mm_linRef(p = pts[[i]], l = pro$line, addz = T, asVector = T) } # project points


# plot data with pmt3 pt.2
# mm_f() is used to filter projected data and prepare it for use with old pmt pt.2 functions
?mm_f()
# set standard values for mm_f()
stds = list(
  projectedRaster = "ras",
  yValue = names(ras)[1],
  rasterizedMaps = "maps",
  mapNumber = 1,
  mapField = para$mf$m1
)
# example using pmt functions with mm_f()
dev.new()
rap = as.points(ras); d = data.frame(rap$x, rap[[1]], rap$z); names(d) = c("x","y","z"); extent = pmt.extent(d); rm(rap); rm(d) # auto set plot extent for pmt3
pmt.empty(grid=T,main="")
d = mm_f("01_NT"); d[d$slope <= 2,] %>% pmt.plot(col="#98c872", cex=20)
mm_f(m=NA) %>% pmt.plot(col="#00000033", cex=1)
b = pmt.bin(d[d$x > 50000 & d$x < 80000,], interval = 200, value = "median", mode = "bin", cth = NA, sth = NA)
m = pmt.model( b, deg = 1)
pmt.plotModel(m, col = "blue",conf=F)

points(pts$p1[[c("x","elev")]], pch=21, bg = "yellow", col = "blue", lwd = 2, cex = 2) # outcrops

points(lns$l1[[c("x","rastValu")]], pch = 46, col = "steelblue", cex = 2) # ice
points(lns$l2[[c("x","rastValu")]], pch = 46, col = "blue", cex = 2) # rivers

















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




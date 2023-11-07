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
maps = list(      m1 = mm_prepMap(map="H:/GIS/PhD/maps/flagg/terraces.gpkg", field = "NAME_KURZ", cropper = profile_buffer, aligner = ras, asVector = F) ) # -------------- map; field
maps = append( maps, mm_prepMap(map="H:/GIS/PhD/maps/hydro/rivers_OPAL_buffer200.gpkg", field = "name", cropper = profile_buffer, aligner = ras, asVector = F) ); names(maps)[length(maps)] = "m2" # -------------- map; field; names(map) <-


# -> prepare data for pt.2
# mm_f() is used to filter projected data and prepare it for use with old pmt pt.2 functions
?mm_f()
# set standard values for mm_f()
stds = list(
  projectedRaster = "rasp",
  yValue = names(rasp)[1],
  rasterizedMaps = "maps",
  mapNumber = 1,
  mapField = "NAME_KURZ"
)
# example using pmt functions with mm_f()
rapp = as.points(rasp); d = data.frame(rapp$x, rapp[[1]], rapp$z); names(d) = c("x","y","z"); extent = pmt.extent(d); rm(rapp); rm(d) # set plot extent
pmt.empty()
d = mm_f("01_NT"); d[d$slope <= 2,] %>% pmt.plot()
b = pmt.bin(d[d$x > 50000 & d$x < 80000,], interval = 200, value = "median", mode = "bin", cth = NA, sth = NA)
m = pmt.model( b, deg = 1)
pmt.plotModel(m, col = "blue")



# 3D plotting and terrace mapping
# d = mm_f(n=0) # convert raster to data.frame
d = data.frame( geom(as.points(ras))[,c(3,4)], as.points(ras)[[1]] )
mapped = mm_map3d(x=d[,1], y=d[,2], z=d[,3], r=rasp$hillshade, type="polygons")






















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





################################################################################
############################################################################ END













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




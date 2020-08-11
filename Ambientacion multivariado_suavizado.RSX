##Shape=vector Points
##Zona=field Shape
##Reducir = string Ingresa el grado de reduccion (1 o 2 es recomendable)
##Poligono=vector polygon
##Area_ha = number 1
##Zonificacion=output vector

gdal_polygonizeR <- function(x, outshape=NULL, gdalformat = 'ESRI Shapefile',
                             pypath=NULL, readpoly=TRUE, quiet=TRUE) {
  if (isTRUE(readpoly)) require(rgdal)
  if (is.null(pypath)) {
    pypath <- Sys.which('gdal_polygonize.py')
  }
  if (!file.exists(pypath)) stop("Can't find gdal_polygonize.py on your system.")
  owd <- getwd()
  on.exit(setwd(owd))
  setwd(dirname(pypath))
  if (!is.null(outshape)) {
    outshape <- sub('\\.shp$', '', outshape)
    f.exists <- file.exists(paste(outshape, c('shp', 'shx', 'dbf'), sep='.'))
    if (any(f.exists))
      stop(sprintf('File already exists: %s',
                   toString(paste(outshape, c('shp', 'shx', 'dbf'),
                                  sep='.')[f.exists])), call.=FALSE)
  } else outshape <- tempfile()
  if (is(x, 'Raster')) {
    require(raster)
    writeRaster(x, {f <- tempfile(fileext='.tif')})
    rastpath <- normalizePath(f)
  } else if (is.character(x)) {
    rastpath <- normalizePath(x)
  } else stop('x must be a file path (character string), or a Raster object.')
  system2('python', args=(sprintf('"%1$s" "%2$s" -f "%3$s" "%4$s.shp"',
                                  pypath, rastpath, gdalformat, outshape)))
  if (isTRUE(readpoly)) {
    shp <- readOGR(dirname(outshape), layer = basename(outshape), verbose=!quiet)
    return(shp)
  }
  return(NULL)
}

require(psych)
require(e1071)
require(rgeos)

if("smoothr" %in% rownames(installed.packages()) == FALSE) {install.packages("smoothr")} else {print("smoothr ya esta..")}
if("e1071" %in% rownames(installed.packages()) == FALSE) {install.packages("e1071")} else {print("e1071 ya esta..")}
if("rgeos" %in% rownames(installed.packages()) == FALSE) {install.packages("rgeos")} else {print("rgeos ya esta..")}

require(sp)
require(spdep)
require(gstat)
require(raster)
require(rgdal)
require(smoothr)
require(units)

Pred<- as_Spatial(Shape)

Pred@data$ZM2<-as.numeric(Pred@data$ZM2)
Pred@data$ZM3<-as.integer(Pred@data$ZM3)
Pred@data$ZM4<-as.integer(Pred@data$ZM4)

Reducir<-as.integer(Reducir)

if (is.integer(Reducir) == FALSE && Reducir<100) {stop("parametros fuera de rangos normales")}
data<-Pred@data[,Zona]
r <- rasterFromXYZ(data.frame(Pred@coords[,1], Pred@coords[,2],data))

#vectorizado<-rasterToPolygons(r ,na.rm=TRUE,dissolve=TRUE)
vectorizado <- gdal_polygonizeR(r)

vectorizado1 <- smooth(vectorizado, method = "ksmooth", smoothness = Reducir)



#REMUEVO POLIGONOS CON UN AREA MENOR A 1 ha
vectorizado1@data$area<-(raster::area(vectorizado1,na.rm=TRUE))/10000
vectorizado2 <- vectorizado1[vectorizado1$area > Area_ha,]


r <- raster(vectorizado2, res=5)
r <- rasterize(vectorizado2, r,field = vectorizado2$DN, background=NA)

poligono <- as_Spatial(Poligono)
rpoly <- raster(poligono, res=5)
rpoligono <- rasterize(poligono, rpoly,background=0)
rpoligono[rpoligono] <- 0



r <- resample(r, rpoligono)
dif <- r - rpoligono
dif[dif == 0] <- NA



fill.na <- function(x) {
  center = 0.5 + (width*width/2) 
  if( is.na(x)[center] ) {
    return( round(modal(x, na.rm=TRUE),0) )
  } else {
    return( round(x[center],0) )
  }
}  

width = 13
r2 <- focal(r, w = matrix(1,width,width), fun = fill.na, 
            pad = TRUE, na.rm = FALSE)
			
print("elimino bordes incorrectos")
ventana<-matrix(1,nrow=9,ncol=9)

r2 <-focal(r2,w=ventana,fun=modal,na.rm=TRUE)

vectorizado <- gdal_polygonizeR(r2)

print("generando buffer para solucionar problemas de geometria")

vectorizado <- gBuffer(vectorizado, width=0, byid = T)
vectorizado<-crop(vectorizado,poligono)
   
 
Zonificacion <- sf::st_as_sf(vectorizado)




      
 


  


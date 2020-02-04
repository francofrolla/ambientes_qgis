##Shape=vector Points
##Zona=field Shape
##Reducir = string Ingresa el grado de reduccion (1 o 2 es recomendable)

##salida_vector=output vector

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


r_poly_smooth <- smooth(vectorizado, method = "ksmooth", smoothness = Reducir)

      
 
salida_vector <- sf::st_as_sf(r_poly_smooth)


  

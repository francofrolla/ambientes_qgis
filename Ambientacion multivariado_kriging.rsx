##Shape=vector point
##enestacolumnaestaelrinde=field Shape
##Poligono=vector polygon
##Distancia = number 40
##output=output raster

#ruta<-choose.files()
##output_plots_to_html
##showplots


library(sp)
library(spdep)
library(gstat)

crs_imagen<-st_crs(Shape)
datos <- as_Spatial(Shape)
poligono <- as_Spatial(Poligono)

proj4string(datos) <- CRS(proj4string(datos))


>print("ARMO GRILLA DE MUESTREO")
#grdpts <- makegrid(poligono, cellsize = 5)
#spgrd <- SpatialPoints(grdpts, proj4string = CRS(proj4string(poligono)))
#spgrdWithin <- SpatialPixels(spgrd[poligono,])
#par(mfrow= c(1,1))
#plot(spgrdWithin,col = "red", pch = 10, cex = 0.2,xlab="X",ylab="Y")
#gri<-spgrdWithin
#print(str(gri))


grd <- sf::st_make_grid(poligono, cellsize = 5, what = "centers")
#grd1 <- st_intersection(grd, st_as_sf(poligono))
#grd2 <- st_centroid(grd1)
gri<-as_Spatial(grd)
gri <- gri[poligono,]
gri<-SpatialPixels(gri)


>print("ARMO VARIOGRAMA")
require(gstat)

semivariograma <- variogram(get(enestacolumnaestaelrinde)~1, datos, cutoff=250)


sill<-max(semivariograma$gamma)
nugget<-semivariograma[1,3]
distancia<-max(semivariograma$dist)

modelovgm<- fit.variogram(semivariograma, fit.method=1, vgm(sill,"Sph",distancia,nugget))
error1<-attr(modelovgm , 'SSErr')
modelo_final = "Sph"


seleccion_modelo<-function(){
	modelos<-c("Sph","Exp","Lin","Gau","Ste","Mat")
		for (i in 1:length(modelos)){
                print(i)
		modelovgm<- fit.variogram(semivariograma, fit.method=1, vgm(sill,modelos[i],distancia,nugget))
	 	error<-(attr(modelovgm , 'SSErr'))
			print(modelos[i])
			print(error)
			if(error < error1){
                        modelo_final <<- modelos[i]
		        error1<-error
 			>print(paste("Por ahora el mejor modelo es",modelo_final))	
				
                        }
            }
}

suppressWarnings(seleccion_modelo())
>print(paste("Modelo final",modelo_final))
modelovgm<- fit.variogram(semivariograma, fit.method=1, vgm(sill,modelo_final,distancia,nugget))
#plot(semivariograma,modelovgm ,main="",xlab="Distancia",ylab="Semivarianza")

data2<-remove.duplicates(datos)
>print("Datos originales")
>print(nrow(datos))
>print("Datos filtrados")
>print(nrow(data2))

#crs(data2)<-NA
#crs(gri)<-NA


Kg_wls <- krige(get(enestacolumnaestaelrinde)~1, data2, gri, model = modelovgm, debug.level=-1,maxdist=Distancia, block = c(40,40))

print("Armamos el Raster para ver en QGIS")
raster<- raster(Kg_wls,layer=1)
crs(raster)<-crs(poligono)
output <- raster
##Zonificacion sobre mapas=group
##Lista=multiple raster
##Poligono=vector polygon
##Estabilidad=output vector
##output_plots_to_html


#27/07/2020- Franco Frolla - Derechos reservados. 
require(psych)
require(e1071)

>print("Franco Frolla - 2020") 
>print("Metodologia extraida de:")
>print("Melchiori, R.J.M., & Albarenque, S.M. (2012). Variabilidad espacio temporal de rendimiento y margen bruto para la delimitación de zonas de manejo. En actas del 11° Curso de Agricultura de Precisión, Manfredi, Córdoba, Argentina.")


lista<-Lista

#Genero grilla para hacer los clusters
poligono <- as_Spatial(Poligono)
grid <- makegrid(poligono, cellsize = 5)
coordinates(grid)<-c("x1","x2")
proj4string(grid) <- crs(poligono)
recortegrilla <- raster::crop(grid,poligono)



#Genero un stack de imagenes raster

for (i in 1:length(lista)){
	lista[[i]]<-raster::mask(lista[[i]],poligono)
    lista[[i]]<-lista[[i]]/cellStats(lista[[i]], stat='mean', na.rm=TRUE)
    lista[[i]] <- resample(lista[[i]],lista[[1]])
    
  }


lista<-stack(lista)

#Extraigo valores de cada capa normalizada. 

nombres<-c("")
matrizdatos<-seq(1,length(recortegrilla),1)

for (i in 1:length(lista@layers)){
  print(i)
  data<-extract(lista[[i]],recortegrilla)
  matrizdatos<-cbind(matrizdatos,data)
  nombre<-lista[[i]]@data@names
  nombres<-c(nombres,nombre)

}

datos<-data.frame(matrizdatos,recortegrilla@coords[,1],recortegrilla@coords[,2])
colnames(datos)<-c(nombres,"x","y")


media<-apply(datos[,2:(ncol(datos)-2)], 1, function(x) mean(x) )
sd<-apply(datos[,2:(ncol(datos)-2)], 1, function(x) sd(x) )
datos$media<-media
datos$CV<-(sd/media)*100

h<-hist(datos$CV,plot=FALSE,breaks=40)  
h$counts <- cumsum(h$counts)/sum(h$counts) 
>plot(h)
>text(h$mids,h$counts,labels=round(h$counts,2), adj=c(0.5, -0.5))

cvcorte<-h$breaks[which(h$counts > 0.75)[1]]

#Si la valor medio normalizado es mayor a 1 y un bajo CV es zona excelente 1 (alto rinde, baja variabilidad)
#si la valor medio normalizado es menor a 1 y un alto CV es mala zona 2 (bajo rinde, alta variabilidad)
#si la valor medio normalizado es mayor a 1 y un alto CV es una condicion inestable. zona 3 (alto rinde, alta variabilidad)
#Si el valor medio normalizado es menor a 1 y un bajo CV es una zona mala consistente zona 4 (bajo rinde, baja variabilidad)

datos$zonas <- ifelse(datos$media >=1 & datos$CV <=cvcorte , "1",ifelse(datos$media < 1 & datos$CV > cvcorte , "2", ifelse(datos$media >=1 & datos$CV > cvcorte , "3", ifelse(datos$media < 1 & datos$CV <=cvcorte , "4", NA))))

>print(paste("CV acumulado",h$counts[which(h$counts > 0.75)[1]]))
>print(paste("El CV elegido fue: ",cvcorte,sep=""))
>print(head(datos))

#SACO LA VARIABLE DUMMY
datos<-datos[2:ncol(datos)]
#elinimo datos inexistentes
datos<-na.omit(datos)

coordinates(datos)<-c("x","y")

crs(datos)<-crs(recortegrilla)

>print("ARMO VARIOGRAMA")

require(gstat)

semivariograma <- variogram(zonas~1, datos, cutoff=250)


sill<-max(semivariograma$gamma)
nugget<-semivariograma[1,3]
Limite_distancia<-max(semivariograma$dist)

modelovgm<- fit.variogram(semivariograma, fit.method=1, vgm(sill,"Sph",Limite_distancia,nugget))
error1<-attr(modelovgm , 'SSErr')
>print(paste("Error inicial",error1))
modelo_final = "Sph"


seleccion_modelo<-function(){
  modelos<-c("Sph","Exp","Lin","Gau","Ste","Mat")
  for (i in 1:length(modelos)){
    print(i)
    modelovgm<- fit.variogram(semivariograma, fit.method=1, vgm(sill,modelos[i],Limite_distancia,nugget))
    error<-(attr(modelovgm , 'SSErr'))
    print(modelos[i])
    print(error)
    if(error < error1){
      modelo_final <<- modelos[i]
      error1<-error
      print(paste("Por ahora el mejor modelo es",modelo_final))	
      
    }
  }
}

suppressWarnings(seleccion_modelo())
>print(paste("Modelo final",modelo_final))
modelovgm<- fit.variogram(semivariograma, fit.method=1, vgm(sill,modelo_final,Limite_distancia,nugget))
plot(semivariograma,modelovgm ,main="",xlab="Distancia",ylab="Semivarianza")

data2<-remove.duplicates(datos)
>print("Datos originales")
>print(nrow(datos))
>print("Datos filtrados")
>print(nrow(data2))

crs(datos)<-crs(recortegrilla)



Kg_wls <- krige(zonas~1, datos, recortegrilla, model = modelovgm, debug.level=-1,maxdist=20,block=c(50,50))
spplot(Kg_wls["var1.pred"])

gridded(Kg_wls) <- TRUE 

crs(Kg_wls)<-crs(poligono)

raster<- raster(Kg_wls,layer=1)
celda <- 3
ventana<-matrix(1,nrow=celda,ncol=celda)
suaveraster<-focal(raster[[1]],w=ventana,fun=mean,na.rm=TRUE)

cols <- c("#d73027","#fee08b","#d9ef8b","#1a9850")
values_tg<-c(1.5,2.5,3.5,4)

lamatrix<-matrix(nrow=4,ncol=3)
lamatrix[1,1]<--Inf
lamatrix[1,2]<-1.5
lamatrix[1,3]<-1
lamatrix[4,1]<-3.5
lamatrix[4,2]<-Inf
lamatrix[4,3]<-4


for (i in 2:4){
  a<-i-1
  lamatrix[i,1]<-values_tg[a]
  b<-i
  lamatrix[i,2]<-values_tg[b]
  a<-i
  lamatrix[i,3]<-values_tg[a]
}

lamatrix[2,3]<-2
lamatrix[3,3]<-3
lamatrix[4,3]<-4


rc <- reclassify(suaveraster, lamatrix,include.lowest=TRUE)
print("ACA ESTA LA VECTORIZACION")
vectorizado<-rasterToPolygons(rc,na.rm=TRUE,dissolve=TRUE)
vectorizado<-vectorizado[order(vectorizado$layer),]

vectorizado@data$zonas<-c(paste("alto rinde, ","CV <",cvcorte,"%",sep=""),paste("bajo rinde, ","CV >",cvcorte,"%",sep=""),paste("alto rinde, ","CV >",cvcorte,"%",sep=""),paste("bajo rinde, ","CV <",cvcorte,"%",sep=""))




Estabilidad<-sf::st_as_sf(vectorizado,crs= crs(poligono))








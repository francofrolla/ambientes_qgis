##Zonificacion sobre mapas=group
##Lista=multiple raster
##Poligono=vector polygon
##Blackmore=output vector

#27/07/2020- Franco Frolla - Derechos reservados. 
require(psych)
require(e1071)

lista<-Lista

#Genero grilla para hacer los clusters
poligono <- as_Spatial(Poligono)
grid <- makegrid(poligono, cellsize = 5)
coordinates(grid)<-c("x1","x2")
proj4string(grid) <- crs(poligono)
recortegrilla <- raster::crop(grid,poligono)

#Genero un stack de imagenes raster

for (i in 1:length(lista)){
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

h<-hist(datos$CV,plot=FALSE)  
h$counts <- cumsum(h$counts)/sum(h$counts) 
>plot(h)
>text(h$mids,h$counts,labels=round(h$counts,2), adj=c(0.5, -0.5))

cvcorte<-h$breaks[which(h$counts > 0.75)[1]]

#Si la valor medio normalizado es mayor a 1 y un bajo CV es zona excelente 1 (alto rinde, baja variabilidad)
#si la valor medio normalizado es menor a 1 y un alto CV es mala zona 2 (bajo rinde, alta variabilidad)
#si la valor medio normalizado es mayor a 1 y un alto CV es una condicion inestable. zona 3 (alto rinde, alta variabilidad)
#Si el valor medio normalizado es menor a 1 y un bajo CV es una zona mala consistente zona 4 (bajo rinde, baja variabilidad)

datos$zonas <- ifelse(datos$media >=1 & datos$CV <=cvcorte , "1",ifelse(datos$media < 1 & datos$CV > cvcorte , "2", ifelse(datos$media >=1 & datos$CV > cvcorte , "3", ifelse(datos$media < 1 & datos$CV <=cvcorte , "4", NA))))


>print(paste("El CV elegido fue: ",cvcorte,sep=""))
>print(head(datos))

#SACO LA VARIABLE DUMMY
datos<-datos[2:ncol(datos)]
#elinimo datos inexistentes
datos<-na.omit(datos)

coordinates(datos)<-c("x","y")


#if(Zonas==0){r <- rasterFromXYZ(data.frame(datos1@coords[,1], datos1@coords[,2],datos1@data["zonas2"]))}
#if(Zonas==1){r <- rasterFromXYZ(data.frame(datos1@coords[,1], datos1@coords[,2],datos1@data["zonas3"]))}
#if(Zonas==2){r <- rasterFromXYZ(data.frame(datos1@coords[,1], datos1@coords[,2],datos1@data["zonas4"]))}

r <- rasterFromXYZ(data.frame(datos@coords[,1], datos@coords[,2],datos@data["zonas"]))
crs(r)<-crs(poligono)

#suavizo y genero el vectorizado.
celda <- 5
ventana<-matrix(1,nrow=celda,ncol=celda)
suaveraster<-focal(r[[1]],w=ventana,fun=modal,na.rm=TRUE)

vectorizado<-rasterToPolygons(suaveraster,na.rm=TRUE,dissolve=TRUE)
vectorizado<-vectorizado[order(vectorizado$layer),]
vectorizado@data$zonas<-c(paste("alto rinde, ","CV <",cvcorte,"%",sep=""),paste("bajo rinde, ","CV >",cvcorte,"%",sep=""),paste("alto rinde, ","CV >",cvcorte,"%",sep=""),paste("bajo rinde, ","CV <",cvcorte,"%",sep=""))

Blackmore<-sf::st_as_sf(vectorizado,crs= crs(poligono))








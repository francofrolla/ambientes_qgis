##Zonificacion sobre mapas=group
##lista=multiple raster
##Poligono=vector polygon
##Output=output raster
##Zonas=selection 2;3;4

#27/07/2020- Franco Frolla - Derechos reservados. 
require(psych)
require(e1071)

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

#SACO LA VARIABLE DUMMY
datos<-datos[2:ncol(datos)]
#elinimo datos inexistentes
datos<-na.omit(datos)

#Genero los clusters
MC_2<-cmeans(datos[,2:ncol(datos)-2],2,100,method="cmeans",m=1.3)
MC_3<-cmeans(datos[,2:ncol(datos)-2],3,100,method="cmeans",m=1.3)
MC_4<-cmeans(datos[,2:ncol(datos)-2],4,100,method="cmeans",m=1.3)

datos1<-cbind(datos,unname(MC_2$cluster),unname(MC_3$cluster),unname(MC_4$cluster))

colnames(datos1)<-c(colnames(datos),"zonas2","zonas3","zonas4")
coordinates(datos1)<-c("x","y")

>str(datos1)
>print(Zonas)

if(Zonas==0){r <- rasterFromXYZ(data.frame(datos1@coords[,1], datos1@coords[,2],datos1@data["zonas2"]))}
if(Zonas==1){r <- rasterFromXYZ(data.frame(datos1@coords[,1], datos1@coords[,2],datos1@data["zonas3"]))}
if(Zonas==2){r <- rasterFromXYZ(data.frame(datos1@coords[,1], datos1@coords[,2],datos1@data["zonas4"]))}


crs(r)<-crs(poligono)

Output <- r
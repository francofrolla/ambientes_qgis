##Shape=vector point
##Rinde=field Shape
##output_plots_to_html
##showplots
##inliers=output vector


library(sp)
library(spdep)

crs_imagen<-st_crs(Shape)
filtro1 <- as_Spatial(Shape)

cord <- filtro1@coords
gri <- dnearneigh(cord,0,30)
lw <- nb2listw(gri, style = "W",zero.policy=TRUE)
par(mfrow=c(1,1))
 
ML <- localmoran (filtro1@data[,Rinde], lw, p.adjust.method="bonferroni",alternative ="less",zero.policy=TRUE)
MP <- moran.plot(filtro1@data[,Rinde],lw,quiet=T,labels=F,col=3,zero.policy=T,xlab="Rendimiento", ylab="Rendimiento Spatially Lagged")
Influ <- MP$is.inf ;
datos0 <- data.frame(filtro1@data,filtro1@coords,ML,Influ)
 
#eliminacion de datos con ?ndice de Moran Local negativo y estad?sticamente significativos (p<0.05).
datos1 <- subset(datos0,datos0$Ii > 0 | datos0$Pr.z...0.>0.05)
myshp.inlier<- subset(datos0,datos0$Ii < 0 | datos0$Pr.z...0.<0.05)

datos2 <- datos1[datos1$dfb.1_ == FALSE & datos1$dfb.x == FALSE
                   & datos1$dffit == FALSE & datos1$cov.r == FALSE & datos1$cook.d
                   == FALSE & datos1$hat == FALSE, ]

>print("--------------------------------------------------------")
>str(datos2)

datos3<-data.frame(datos2[Rinde],datos2["coords.x1"],datos2["coords.x2"])  
coordinates(datos2)<-c("coords.x1","coords.x2")
output <-sf::st_as_sf(datos3, coords = c("coords.x1", "coords.x2"),crs= crs_imagen)

n<-nrow(filtro1@data[Rinde])-nrow(datos3[Rinde])
>print(paste("Se filtraron",n,"puntos"))


inliers = output

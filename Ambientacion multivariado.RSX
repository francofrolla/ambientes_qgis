##Shape=vector point
##output_plots_to_html
##showplots
##cluster=output vector



require(spdep)
require(rgdal)
require(gstat)
require(ade4)
require(e1071)
require(sf)


#Hago el multivariado
Pred<- as_Spatial(Shape)
pca <- dudi.pca(Pred@data, center=T,scannf = FALSE,  nf = 5)


cord_1 <- coordinates(Pred@coords)
gri_1 <- dnearneigh(cord_1,0,25)
lw_1 <- nb2listw(gri_1, style = "W")

ms <- multispati(pca, lw_1, scannf = F, nfposi = 5)
s.arrow(ms$c1,xax = 1, yax = 2, clabel = 1)

# Extraction of spatial principal components
sPC <- ms$li[,1:length(ms$li)]
Pred@data <- cbind(Pred@data,sPC)

indicadorCS1<-grep("CS1", colnames(Pred@data))
indicadorCS2<-indicadorCS1 + 1

#  Fuzzy k-means cluster analysis
MC_2<-cmeans(Pred@data[,indicadorCS1:indicadorCS2],2,100,method="cmeans",m=1.3)
MC_3<-cmeans(Pred@data[,indicadorCS1:indicadorCS2],3,100,method="cmeans",m=1.3)
MC_4<-cmeans(Pred@data[,indicadorCS1:indicadorCS2],4,100,method="cmeans",m=1.3)

# Indices for selecting the number of classes: two (I2MC), three (I3MC) and four (I4MC)
I2MC <- fclustIndex(MC_2,Pred@data[,indicadorCS1:indicadorCS2], index=c("xie.beni", "fukuyama.sugeno",
"partition.coefficient", "partition.entropy"))

I3MC <- fclustIndex(MC_3,Pred@data[,indicadorCS1:indicadorCS2], index=c("xie.beni", "fukuyama.sugeno",
"partition.coefficient", "partition.entropy"))

I4MC <- fclustIndex(MC_4,Pred@data[,indicadorCS1:indicadorCS2], index=c("xie.beni", "fukuyama.sugeno",
"partition.coefficient", "partition.entropy"))

Indices0 <- cbind(I2MC,I3MC,I4MC)

XieBeni <-Indices0[1,]
FukSug <-Indices0[2,]
PartCoef_1 <-Indices0[3,]
PartCoef <- 1/PartCoef_1
PartEntr <-Indices0[4,]

Indices <- as.data.frame(rbind(XieBeni,FukSug,PartCoef,PartEntr))
Indices

# Summary indices
XieBeniMax<-max(Indices[1,])
FukSugMax<-max(Indices[2,])
PartCoefMax<-max(Indices[3,])
PartEntrMax<-max(Indices[4,])

XieBeniN<- XieBeni/XieBeniMax
FukSugN<- FukSug/FukSugMax
PartCoefN<- PartCoef/PartCoefMax
PartEntrN<-PartEntr/PartEntrMax

IndicesN <- as.data.frame(rbind(XieBeniN,FukSugN,PartCoefN,PartEntrN))
IndicesN2 <- (IndicesN)^2

Indice2MC <- sqrt(sum(IndicesN2[,1]))
Indice3MC <- sqrt(sum(IndicesN2[,2]))
Indice4MC<- sqrt(sum(IndicesN2[,3]))

# Summary indices for selection of two, three or four management zones
Indice2MC; Indice3MC; Indice4MC

>print("El menor valor indica la mejor separación posible")
>print(paste("Indice para 2 zonas",Indice2MC))
>print(paste("Indice para 3 zonas",Indice3MC))
>print(paste("Indice para 4 zonas",Indice4MC))

# Maps with management classes delimited
ZM2<-MC_2$cluster
ZM3<-MC_3$cluster
ZM4<-MC_4$cluster
Pred@data <-cbind(Pred@data,ZM2)
Pred@data  <-cbind(Pred@data,ZM3)
Pred@data <-cbind(Pred@data,ZM4)

>print(str(Pred))

cluster<-st_as_sf(Pred)

##Poligonos=vector polygon
##Mapa_de_Rendimiento=vector point
##Rendimiento=field Mapa_de_Rendimiento
##Reducir = string Tamaño de submuestra (min 250)
##output_plots_to_html
##showplots

require(sf)
require(nlme)
require(multcomp)
require(multcompView)
require(lsmeans)

rendimiento = as_Spatial(Mapa_de_Rendimiento)
ambientes = as_Spatial(Poligonos)


crs<-proj4string(rendimiento) 
z<-rendimiento@data[,Rendimiento]
x<-rendimiento@coords[,1]
y<-rendimiento@coords[,2]


rendimiento <- data.frame(x,y,z)
rendimiento <-rendimiento[sample(nrow(rendimiento),as.integer(Reducir)),]
coordinates(rendimiento)<-c("x","y") 



proj4string(rendimiento) <- CRS(crs)
proj4string(ambientes) <- CRS(crs)

SampleWY<-cbind(rendimiento@data,over(rendimiento,ambientes))
SampleWY<-cbind(SampleWY,rendimiento@coords[,1],rendimiento@coords[,2])
SampleWY<-data.frame(SampleWY)


if(length(colnames(SampleWY)) == 5){
colnames(SampleWY)<-c("Wy","fid","Zone","X","Y")
}
if(length(colnames(SampleWY)) == 4){
colnames(SampleWY)<-c("Wy","Zone","X","Y")
}

>str(SampleWY)
>colnames(SampleWY)



SampleWY$Zone<-as.factor(SampleWY$Zone)

SampleWY<-na.omit(SampleWY)

# Model with exponencial spatial correlation
mod1_Wy <-gls(Wy~1+Zone
,correlation=corExp(form=~as.numeric(as.character(X))+as.numeric(as.character(Y))
,metric="euclidean"
,nugget=FALSE)
,method="REML"
,na.action=na.omit
,data=SampleWY)

# Model with exponential spatial correlation and nugget effect
mod2_Wy <-gls(Wy~1+Zone
,correlation=corExp(form=~as.numeric(as.character(X))+as.numeric(as.character(Y))
,metric="euclidean"
,nugget=TRUE)
,method="REML"
,na.action=na.omit
,data=SampleWY)

# AModel with spherical spatial correlation
mod3_Wy <-gls(Wy~1+Zone
,correlation=corSpher(form=~as.numeric(as.character(X))+as.numeric(as.character(Y))
,metric="euclidean"
,nugget=FALSE)
,method="REML"
,na.action=na.omit
,data=SampleWY)

# Model with spherical spatial correlation and nugget effect
mod4_Wy <-gls(Wy~1+Zone
,correlation=corSpher(form=~as.numeric(as.character(X))+as.numeric(as.character(Y))
,metric="euclidean"
,nugget=TRUE)
,method="REML"
,na.action=na.omit
,data=SampleWY)

# Model of independent errors
mod5_Wy <-gls(Wy~1+Zone
,method="REML"
,na.action=na.omit
,data=SampleWY)

# Selecting spatial correlation model using the Akaike information criterion
AICmod1_Wy <- AIC(mod1_Wy)
AICmod2_Wy <- AIC(mod2_Wy)
AICmod3_Wy <- AIC(mod3_Wy)
AICmod4_Wy <- AIC(mod4_Wy)
AICmod5_Wy <- AIC(mod5_Wy)

modelos<-c("mod1_Wy","mod2_Wy","mod3_Wy","mod4_Wy","mod5_Wy")
criterio<-c(AICmod1_Wy,AICmod2_Wy,AICmod3_Wy,AICmod4_Wy,AICmod5_Wy)


modelofinal<-AICmod1_Wy



indicefinal <- 1

for (i in 1:5){
	if(criterio[i] < modelofinal){indicefinal = i}
}

>print("El mejor modelo es")
>print(modelos[indicefinal])

# Summary of selected model (Yield)
summary(get(modelos[indicefinal]))
#Wymeans <- summary(lsmeans(get(modelos[indicefinal]),"Zone")); Wymeans

modelo.lm<-lsmeans(get(modelos[indicefinal]),"Zone")
Wymeans <- summary(modelo.lm)
>print(Wymeans)

#

valores<-cld(modelo.lm, Letters = c("a","b","c","d","e"),sort=TRUE,reversed=TRUE)
letras<- c(valores$.group)
#letras<-letras[length(letras):1]

>print(valores)

Wymean <-by(Wymeans$lsmean,Wymeans$Zone,mean)
Wyse<-by(Wymeans$SE,Wymeans$Zone,mean)
wy <- barplot(Wymean,xlab="Zonas de manejo", ylab="Rendimiento",
,ylim=c(0,max(max(Wymean)+max(Wymean)*0.1)),xpd=F)

letters = letras
text(x=wy,y=Wymean+Wymean*0.05,label=letters,cex = 1)







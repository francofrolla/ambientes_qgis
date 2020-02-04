##Shape=vector
##Rinde=field Shape
##output_plots_to_html
##showplots
##outliers=output vector

sp.na.omit <- function(x, col.name = NULL, margin = 1) {
              if (!inherits(x, "SpatialPointsDataFrame") & 
                  !inherits(x, "SpatialPolygonsDataFrame") & 
                  !inherits(x, "SpatialLinesDataFrame") )
                stop("MUST BE sp SpatialPointsDataFrame OR SpatialPolygonsDataFrame class object")
              if(!is.null(col.name)) {
                if(is.na(match(col.name, names(x)))) stop(col.name, "does not exist in data") 
                return( x[-which(is.na(x@data[,col.name])),] )
              } else {    
                na.index <- unique(as.data.frame(which(is.na(x@data), arr.ind = TRUE))[, margin])
                if (margin == 1) {
                  cat("Deleting rows: ", na.index, "\n")
                  return(x[-na.index, ])
                }
                if (margin == 2) {
                  cat("Deleting columns: ", na.index, "\n")
                  return(x[, -na.index])
                }
              }
            }   

library(sp)
crs_imagen<-st_crs(Shape)

myshp <- as_Spatial(Shape)
str(myshp)

>print(myshp@coords)


Media <- mean(na.omit(myshp@data[,Rinde]))
DE <- sd(na.omit(myshp@data[,Rinde]))
LI <- Media-(2*DE)
LS <- Media+(2*DE)
>print(paste("Media:",Media))
>print(paste("DE:",DE))
>print(paste("LI",LI))
>print(paste("LS",LS))


myshp.outlier<-myshp
myshp.outlier[LS<myshp@data[,Rinde]|myshp@data[,Rinde]<LI] <-NA
>print(myshp.outlier)

myshp.outlier <- subset(sp.na.omit(myshp.outlier[,Rinde]))

n<-nrow(myshp@data[,Rinde])-nrow(myshp.outlier@data[,Rinde])
>print(paste("Se filtraron",n,"puntos"))

output <- sf::st_as_sf(myshp.outlier,crs= crs_imagen)
outliers = output

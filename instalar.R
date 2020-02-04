instalar_paquetes<-function(sistema_operativo){
          if(sistema_operativo == "linux"){
          print("Instalando paquetes para entorno Linux")
          system("sudo apt update")
          system("sudo apt-get install r-cran-spdep")
          system("sudo dpkg-query -l | grep r-cran-spdep")
          system("sudo apt install -y libudunits2-0 libudunits2-dev")
          system("sudo apt install libgdal-dev")
          system("sudo apt install gdal-bin libgdal-dev libproj-dev")
          install.packages("sp")
          install.packages("gstat")
          install.packages("maptools")
          install.packages("rgeos")
          install.packages("raster")
          install.packages("rgdal")
          install.packages("automap")
          install.packages("spdep")
          install.packages("spdep")
          install.packages("smoothr")
          }
          if(sistema_operativo == "windows"){
          print("Instalando paquetes para entorno Windows")
          install.packages("sp")
          install.packages("gstat")
          install.packages("maptools")
          install.packages("rgeos")
          install.packages("raster")
          install.packages("rgdal")
          install.packages("automap")
          install.packages("spdep")
          install.packages("spdep")
          install.packages("smoothr")
          }    
          
}

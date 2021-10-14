# Fire

# setwd("/home/marcio/Documentos/data/fogo")

library(sp)
library(rgdal)
library(raster)
library(xts)
library(ncdf4)
library(tidyverse)


x <- c(-47.75014, -47.74389, -47.76830, -47.72287, -47.73264, -47.73613, -47.68317,
       -47.66713, -47.69987, -47.70263, -47.76707, -47.71484, -47.71418, -47.67871,
       -47.67911, -47.68465, -47.63446, -47.63412, -47.63415, -47.69065, -46.97297,
       -46.98341, -46.98311, -46.98177, -47.84833, -47.84950, -47.84347, -46.97983,
       -46.98513, -46.98280, -47.63719, -47.63690, -47.63657, -47.63625, -47.63551,
       -47.63513, -47.63479, -47.63447, -47.63412, -47.63376, -47.63497, -47.63472,
       -47.63449, -47.63425, -47.63398, -47.63376, -47.63349, -47.63340, -47.63323,
       -47.63333)
y <- c(-14.14786, -14.14263, -14.13991, -14.13116, -14.12362, -14.12612, -14.12855,
       -14.12713, -14.12875, -14.12913, -14.13310, -14.13943, -14.13980, -14.11648,
       -14.11652, -14.11530, -14.09114, -14.09050, -14.08997, -14.12734, -13.92030,
       -13.88963, -13.88625, -13.88888, -14.20525, -14.20966, -14.20466, -13.89772,
       -13.88366, -13.88300, -14.10703, -14.10691, -14.10672, -14.10647, -14.10615,
       -14.10608, -14.10588, -14.10573, -14.10555, -14.10556, -14.09398, -14.09369,
       -14.09341, -14.09310, -14.09275, -14.09240, -14.09212, -14.09177, -14.09145,
       -14.09123)

xy <- cbind(x,y)

coordR <- SpatialPoints(xy, 
                        proj4string = crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


list_data1 <- list.files("/home/marcio/Documentos/data/fogo/monthly_burned_coverage", 
                         recursive = T, pattern = ".tif", full.names = T)

list_data2 <- list.files("/home/marcio/Documentos/data/fogo/fire_frequency", 
                         recursive = T, pattern = ".tif", full.names = T)

list_data3 <- list.files("/home/marcio/Documentos/data/fogo/deforestation_recovery", 
                         recursive = T, pattern = ".tif", full.names = T)


for (i in 1:length(list_data1)) {
  aux <- crop(list_data1[i]%>%raster,extent(-47.92746, -46.93437, -14.21476, -13.59438))
  if (i == 1){
    monthly_burned_coverage <- stack(aux, nlayers = 1)
  }else{
    monthly_burned_coverage <-  addLayer(monthly_burned_coverage, aux)
  }
}

for (i in 1:length(list_data2)) {
  aux <- crop(list_data2[i]%>%raster,extent(-47.92746, -46.93437, -14.21476, -13.59438))
  if (i == 1){
    fire_frequency <- stack(aux, nlayers = 1)
  }else{
    fire_frequency <-  addLayer(fire_frequency, aux)
  }
}

for (i in 1:length(list_data3)) {
  aux <- crop(list_data3[i]%>%raster,extent(-47.92746, -46.93437, -14.21476, -13.59438))
  if (i == 1){
    defor_recor <- stack(aux, nlayers = 1)
  }else{
    defor_recor <-  addLayer(defor_recor, aux)
  }
}


fire_frequency_pontos <- raster::extract(fire_frequency,coordR)
colnames(fire_frequency_pontos) <- substr(colnames(fire_frequency_pontos), 59, 67)
#write.table(fire_frequency_pontos, "fire_freq_pontos.txt")


defor_recor_pontos <- raster::extract(defor_recor,coordR)
colnames(defor_recor_pontos) <- substr(colnames(defor_recor_pontos), 59, 62)
#write.table(defor_recor_pontos, "defor_recor_pontos.txt")

fire_frequency_pontos %>% tibble # shows fire frequency for the periods 2013-2020 and 2018-2020.

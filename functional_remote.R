# Tutorial to show you how I did figures for the manuscript called "Are coexisting vegetation types in a tropical heterogeneous landscape alternative stable states?"
# Marcio Baldissera Cure

library(tidyverse)
library(patchwork)
library(RColorBrewer)
library(raster)
library(xts)

# 1. Coupling

coup <- read.table("./coup.txt", h = T) %>% rename(Vegetation_type=Fitofisionomia)

coup$Vegetation_type <- c(rep("savanna", 10),rep("gallery forest", 10),rep("dry forest", 10))
  
# 2. EVI2

  evi2 <- read.table("/home/marcio/Documentos/data/evi2 raster e txt/evi2_todaserie_LS8", h = T)
  evi2 %>% head
  
  datas <- paste0(substr(list.files("/home/marcio/Documentos/data/landsat8 2021/PNCV/ja unzipado", recursive = T),18,25)) %>% as.Date(format = "%Y%m%d")
  names(evi2) <- paste0(datas) %>% as.character()
  
  evi2.r <- matrix(NA,nrow=30, ncol = 101)
  
  for (i in 1:dim(evi2.r)[1]){
    ep <- xts::endpoints(xts::xts(t(evi2[i,]),datas), on = "months")
    aux <- xts::period.apply(as.matrix(t(evi2[i,])), INDEX = ep,
                             FUN = mean, na.action=na.pass)
    if (i == 1) {
      evi2.r <- aux
	  }else{
	    evi2.r <- cbind(evi2.r, aux)
	  }
	}


evi2.r <- evi2.r[1:61,]



##### EVI2 statistics

for (i in 1:nrow(evi2)) {
  aux <- evi2[i,] %>% sd(na.rm = T)
  if (i == 1) {
    desvio.padrão <- aux
  } else {
    desvio.padrão <- c(desvio.padrão, aux)
  }
}

for (i in 1:nrow(evi2)) {
  aux <- t(evi2)[,i] %>% mean(na.rm = T)
  if (i == 1) {
    media <- aux
  } else {
    media <- c(media, aux)
  }
}

for (i in 1:nrow(evi2)) {
  aux <- evi2[i,] %>% max(na.rm = T)
  if (i == 1) {
    maximo <- aux
  } else {
    maximo <- c(maximo, aux)
  }
}

for (i in 1:nrow(evi2)) {
  aux <- evi2[i,] %>% min(na.rm = T)
  if (i == 1) {
    minimo <- aux
  } else {
    minimo <- c(minimo, aux)
  }
}

amplitude <- maximo-minimo

EVI2par <- cbind(sd = desvio.padrão, max = maximo, mean = media, range = amplitude)

EVI2par1 <- cbind(EVI2par,`Vegetation type` = c(rep("savanna", 10),rep("gallery forest", 10),rep("dry forest", 10),rep("grassland",20))) %>% data.frame

write.table(EVI2par1, "evi2_estatistic.txt")


a2<-EVI2par1[1:30,] %>% as_tibble()%>%
  ggplot()+
  aes(x = `Vegetation type`, y = as.numeric(mean), fill = factor(`Vegetation type`))+
  geom_boxplot(show.legend = F)+
  ggtitle("A")+
  ylab("Mean")+
  xlab("Vegetation type")+ 
  scale_fill_manual(values=c("purple", "darkgrey", "orange2"))

b2<- EVI2par1[1:30,] %>% as_tibble()%>%
  ggplot()+
  aes(x = `Vegetation type`, y = as.numeric(sd), fill = factor(`Vegetation type`))+
  geom_boxplot(show.legend = F)+
  ggtitle("B")+
  ylab("Standard deviation")+
  xlab("Vegetation type")+ 
  scale_fill_manual(values=c("purple", "darkgrey", "orange2"))

c2<-EVI2par1[1:30,] %>% as_tibble()%>%
  ggplot()+
  aes(x = `Vegetation type`, y = as.numeric(max), fill = factor(`Vegetation type`))+
  geom_boxplot(show.legend = F)+
  ggtitle("C")+
  ylab("Maximum")+
  xlab("Vegetation type")+ 
  scale_fill_manual(values=c("purple", "darkgrey", "orange2"))

d2<-EVI2par1[1:30,] %>% as_tibble()%>%
  ggplot()+
  aes(x = `Vegetation type`, y = as.numeric(range), fill = factor(`Vegetation type`))+
  geom_boxplot(show.legend = F)+
  ggtitle("D")+
  ylab("Amplitude")+
  xlab("Vegetation type")+ 
  scale_fill_manual(values=c("purple", "darkgrey", "orange2"))

a2/c2 | b2/d2

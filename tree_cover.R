# Tree cover

library(tidyverse)
library(patchwork)
library(RColorBrewer)
library(raster)


# Baixa os dados vetoriais das UCs do Brasil:
url = "https://www.icmbio.gov.br/portal/images/stories/servicos/geoprocessamento/DCOL/dados_vetoriais/UC_fed_julho_2019.zip"
download.file(url = url, "/home/marcio/Documentos/data/shp chapada/UCs Fed")

UCs <- sf::st_read("/home/marcio/Documentos/data/shp chapada/UCs Fed/UC_fed_julho_2019/UC_fed_julho_2019.shp") # O PNCV é o 302

# Determine the sample plot coordinates.
## I got my own sample plots at the PNCV. I named the longitude as 'x' and latitude as 'y'.

x <- c(-47.75014, -47.74389, -47.76830, -47.72287, -47.73264, -47.73613, -47.68317, -47.66713, -47.69987, -47.70263, -47.76707, -47.71484, -47.71418, -47.67871, -47.67911, -47.68465, -47.63446, -47.63412, -47.63415, -47.69065, -46.97297, -46.98341, -46.98311, -46.98177, -47.84833, -47.84950, -47.84347, -46.97983, -46.98513, -46.98280, -47.63719, -47.63690, -47.63657, -47.63625, -47.63551, -47.63513, -47.63479, -47.63447, -47.63412, -47.63376, -47.63497, -47.63472, -47.63449, -47.63425, -47.63398, -47.63376, -47.63349, -47.63340, -47.63323, -47.63333)
y <- c(-14.14786, -14.14263, -14.13991, -14.13116, -14.12362, -14.12612, -14.12855, -14.12713, -14.12875, -14.12913, -14.13310, -14.13943, -14.13980, -14.11648, -14.11652, -14.11530, -14.09114, -14.09050, -14.08997, -14.12734, -13.92030, -13.88963, -13.88625, -13.88888, -14.20525, -14.20966, -14.20466, -13.89772, -13.88366, -13.88300, -14.10703, -14.10691, -14.10672, -14.10647, -14.10615, -14.10608, -14.10588, -14.10573, -14.10555, -14.10556, -14.09398, -14.09369, -14.09341, -14.09310, -14.09275, -14.09240, -14.09212, -14.09177, -14.09145, -14.09123)

## Then, I conbined both into a two column matrix.

xy <- cbind(x,y)

## Transforming coordinates values to SpatialPoints with the projection format. 

coordR <- SpatialPoints(xy, 
                        proj4string = crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

#Now, the actual data:

## Tree cover
#### Remote estimation by Hansen et al.
##### Download data for the region of interest
download.file("https://storage.googleapis.com/earthenginepartners-hansen/GFC-2019-v1.7/Hansen_GFC-2019-v1.7_treecover2000_10S_050W.tif")

tc_hansen <- raster("./Hansen_GFC-2019-v1.7_treecover2000_10S_050W.tif") %>% 
  crop(y = extent(UCs[302,]))

#### Extract tree covers from our sample plots
tc_hansen_our_sample <- raster::extract(tc_hansen, coordR) %>%
  as.data.frame %>%
  add_column(Vegetation_type = c(rep("savanna", 10),
                                 rep("gallery_for", 10),
                                 rep("dry_for",10),
                                 rep("grassland", 20)))

#### Extract georreferenced tree cover values
tc_hansen_dataframe <- tc_hansen %>% rasterToPoints %>% as.data.frame %>% rename("tree_cover" = "Hansen_GFC.2019.v1.7_treecover2000_10S_050W")

## Plot histogram from all Chapada dos Veadeiros National Park (UCs[302]) pixels with Tree Cover higher than 5%

tc_hans <- tc_hansen_dataframe %>% filter(tree_cover > 5) %>% ggplot() +
  aes(x = tree_cover)+
  geom_histogram(bins = 40)+
  geom_vline(xintercept = 60)+
  xlab("Tree cover (%)")+
  ggtitle("Remote (Hansen) tree cover at the PNCV", subtitle = "Without tree less environments (Tree cover > 5%)")+
  geom_text(aes(x = 68 , y = 335000), label = "Forest", show.legend = F, color = "grey2", size = 5) +
  geom_text(aes(x = 48 , y = 335000), label = "Savanna", show.legend = F, color = "grey2", size = 5) +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13),
        title=element_text(size=14))

## Plot histogram from extracted tree cover values for the coordinates corresponding with our sample plots
### You can start this part from here:
tc_hansen_our_sample<-read.table("./tc_hansen.txt",h=T)

tc_hans_our <- ggplot(tc_hansen_our_sample[1:30,])+
  geom_histogram(aes(x = ., fill = factor(Vegetation_type), alpha = 0.4), show.legend = F)+
  ggtitle("Remote data", subtitle = "Hansen measurements")+
  xlab("Tree cover (%)")+
  scale_fill_manual(values=c("purple", "darkgrey", "orange2"))+
  geom_vline(xintercept = 60)+
  geom_text(aes(x = 68 , y = 3.75), label = "Forest", show.legend = F, color = "grey2", size = 5) +
  geom_text(aes(x = 50 , y = 3.75), label = "Savanna", show.legend = F, color = "grey2", size = 5) +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13),
        title=element_text(size=14))


tc_hans/tc_hans_our
#### **in situ** tree cover measurements with a concave densiometer
tc_field <- read.table("tc_field.txt", h = T) %>%
  rename(Vegetation_type = Biome)

##### Plot tree cover histogram for the wet and dry season further the difference between seasons.
tc_a <-  ggplot(tc_field)+
  geom_histogram(aes(x = wet, fill = factor(Vegetation_type), alpha = 0.4), show.legend = F)+
  ggtitle("In situ measurements - wet season", subtitle = "Concave densiometer")+
  xlab("Tree cover (%)")+
  scale_fill_manual(values=c("purple", "darkgrey", "orange2"))+
  geom_vline(xintercept = 60)+
  geom_text(aes(x = 68 , y = 3), label = "Forest", show.legend = F, color = "grey2", size = 5) +
  geom_text(aes(x = 50 , y = 3), label = "Savanna", show.legend = F, color = "grey2", size = 5) +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13),
        title=element_text(size=14))

tc_b <-  ggplot(tc_field)+
  geom_histogram(aes(x = dry, fill = factor(Vegetation_type), alpha = 0.4), show.legend = F)+
  ggtitle("In situ measurements - dry season", subtitle = "Concave densiometer")+
  xlab("Tree cover (%)")+
  scale_fill_manual(values=c("purple", "darkgrey", "orange2"))+
  geom_vline(xintercept = 60)+
  geom_text(aes(x = 68 , y = 3), label = "Forest", show.legend = F, color = "grey2", size = 5) +
  geom_text(aes(x = 50 , y = 3), label = "Savanna", show.legend = F, color = "grey2", size = 5) +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13),
        title=element_text(size=14))

tc_c <-  ggplot(tc_field)+
  geom_histogram(aes(x = (wet-dry), fill = factor(Vegetation_type), alpha = 0.4), show.legend = F)+
  ggtitle("In situ measurements - Difference btw seasons", subtitle = "Concave densiometer")+
  xlab("Tree cover (%)")+
  scale_fill_manual(values=c("purple", "darkgrey", "orange2"))+
  geom_vline(xintercept = 60)+
  geom_text(aes(x = 68 , y = 3), label = "Forest", show.legend = F, color = "grey2", size = 5) +
  geom_text(aes(x = 50 , y = 3), label = "Savanna", show.legend = F, color = "grey2", size = 5) +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13),
        title=element_text(size=14))

(tc_a|tc_b|tc_c)

# Combining all tree cover data in a plot.
(tc_hans/tc_hans_our)/(tc_a|tc_b|tc_c)


library(tidyverse)
library(patchwork)
library(RColorBrewer)

fire <- read.table("./fire_freq_pontos.txt")

tc_field <- read.table("./tc_field.txt", h = T)
#write.table(tc_field, "tc_field.txt")

tc_hansen <- read.table("./tc_hansen.txt", h = T) 
#write.table(tc_hansen, "tc_hansen.txt")


coup <- read.table("./coup.txt", h = T) 
#write.table(coup, "coup.txt")

traits <- read.table("./traits_community_mean.txt", h = T)
#write.table(traits, "traits.txt")


evi2 <- read.table("evi2_estatistic.txt", h = T)

deciduousness <- (tc_field$wet-tc_field$dry)
data <- coup %>% add_column(freq_fire = fire[1:30,1]) %>% add_column(tree_cover_wet = tc_field$wet)%>% add_column(tree_cover_dry = tc_field$dry) %>% add_column(tree_cover_hansen = tc_hansen[1:30,1]) %>% add_column(phenology = deciduousness) %>% add_column(evi2[1:30,1:4])


a<- data %>% 
  ggplot()+
   aes(y = Coup_lag_max, fill = factor(Vegetation_type), show.legend = FALSE)+
  geom_boxplot()+
  ggtitle("Maximum coupling")+
  scale_fill_manual(c("purple","grey","orange"))+
  ylab("Coupling between EVI2 and precipitation")+
  xlab("Vegetation type")


b<-data %>% 
  ggplot()+
  aes(y = Lag, fill = factor(Vegetation_type), show.legend = FALSE)+
  geom_boxplot()+
  ggtitle("Lag in months for the maximum coupling")+
  scale_fill_manual(c("purple","grey","orange"))+
  ylab("Lag in months")+
  xlab("Vegetation type")


c<-data %>% 
  ggplot()+
  aes(y = deciduousness, fill = factor(Vegetation_type), show.legend = FALSE)+
  geom_boxplot()+
  ggtitle("Deciduousness", subtitle = "Changes in tree cover among dry and wet seasons")+
  scale_fill_manual(c("purple","grey","orange"))+
  ylab("Deciduousness")+
  xlab("Vegetation type")

d <- data %>% 
  ggplot()+
  aes(y = tree_cover_wet, fill = factor(Vegetation_type), show.legend = FALSE)+
  geom_boxplot()+
  ggtitle("Tree cover", subtitle = "densiometer")+
  scale_fill_manual(c("purple","grey","orange"))+
  ylab("Tree cover (%)")+
  xlab("Vegetation type")


(a|b)/(c|d)


relative_bark_hickness <- (traits$Bark_thickness/traits$cwm_basal_area)

a1 <- data %>% 
  ggplot()+
  aes(y = freq_fire, fill = factor(Vegetation_type), show.legend = FALSE)+
  geom_boxplot()+
  ggtitle("Frequência de fogo", subtitle = "Período de 1985-2018")+
  scale_fill_manual(c("purple","grey","orange"))+
  ylab("Times burned")+
  xlab("Vegetation type")

b1 <- traits %>% 
  ggplot()+
  aes(y = Bark_thickness, fill = factor(Vegetation_type), show.legend = FALSE)+
  geom_boxplot()+
  ggtitle("Bark thickness")+
  scale_fill_manual(c("purple","grey","orange"))+
  ylab("Bark thickness")+
  xlab("Vegetation type")


c1 <- traits %>% 
  ggplot()+
  aes(y = Tree_height, fill = factor(Vegetation_type), show.legend = FALSE)+
  geom_boxplot()+
  ggtitle("Tree height")+
  scale_fill_manual(c("purple","grey","orange"))+
  ylab("Tree height (m)")+
  xlab("Vegetation type")

d1 <- traits %>% 
  ggplot()+
  aes(y = basal_area_total, fill = factor(Vegetation_type), show.legend = FALSE)+
  geom_boxplot()+
  ggtitle("Basal area total")+
  scale_fill_manual(c("purple","grey","orange"))+
  ylab("Basal area total")+
  xlab("Vegetation type")


(a1|b1)/(c1|d1)





fire

dev.off()

par(mfrow=c(2,3))



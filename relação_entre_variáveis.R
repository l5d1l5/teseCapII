

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

#relative_bark_hickness <- (traits$Bark_thickness/traits$cwm_basal_area)

data <- coup %>% add_column(freq_fire = fire[1:30,1]) %>% add_column(tree_cover_wet = tc_field$wet)%>% add_column(tree_cover_dry = tc_field$dry) %>% add_column(tree_cover_hansen = tc_hansen[1:30,1]) %>% add_column(phenology = deciduousness) %>% add_column(evi2[1:30,1:4]) %>% rename(Vegetation_type=Fitofisionomia)


data %>% knitr::kable() %>% view

a<- data %>% 
  ggplot()+
  aes(y = Coup_lag_max, fill = factor(Vegetation_type), x = factor(Vegetation_type))+
  geom_boxplot(show.legend = FALSE)+
  ggtitle("Maximum coupling")+
  scale_fill_manual(values=c("purple", "darkgrey", "orange2"))+  ylab("Coupling")+
  xlab("Vegetation type")


b<-data %>% 
  ggplot()+
  aes(y = Lag, fill = factor(Vegetation_type), x = factor(Vegetation_type))+
  geom_boxplot(show.legend = FALSE)+
  ggtitle("Lag in months for the max coupling")+
  scale_fill_manual(values=c("purple", "darkgrey", "orange2"))+
  ylab("Lag in months")+
  xlab("Vegetation type")


c<-data %>% 
  ggplot()+
  aes(y = deciduousness, fill = factor(Vegetation_type), x = factor(Vegetation_type))+
  geom_boxplot(show.legend = FALSE)+
  ggtitle("Deciduousness", subtitle = "Differentece btw dry and wet seasons")+
  scale_fill_manual(values=c("purple", "darkgrey", "orange2"))+
  ylab("Deciduousness")+
  xlab("Vegetation type")


png("state_var.png", width = 900)
(a|b|c)
dev.off()

a3<-data %>% 
  ggplot()+
  aes(y = mean, fill = factor(Vegetation_type), x = factor(Vegetation_type))+
  geom_boxplot(show.legend = FALSE)+
  ggtitle("Mean EVI2")+
  scale_fill_manual(values=c("purple", "darkgrey", "orange2"))+
  ylab("Mean EVI2")+
  xlab("Vegetation type")

b3<-data %>% 
  ggplot()+
  aes(y = sd, fill = factor(Vegetation_type), x = factor(Vegetation_type))+
  geom_boxplot(show.legend = FALSE)+
  ggtitle("Standard deviation EVI2")+
  scale_fill_manual(values=c("purple", "darkgrey", "orange2"))+
  ylab("Standard deviation EVI2")+
  xlab("Vegetation type")

c3<-data %>% 
  ggplot()+
  aes(y = max, fill = factor(Vegetation_type), x = factor(Vegetation_type))+
  geom_boxplot(show.legend = FALSE)+
  ggtitle("Maximum EVI2")+
  scale_fill_manual(values=c("purple", "darkgrey", "orange2"))+
  ylab("Maximum EVI2")+
  xlab("Vegetation type")

d3<-data %>% 
  ggplot()+
  aes(y = range, fill = factor(Vegetation_type), x = factor(Vegetation_type))+
  geom_boxplot(show.legend = FALSE)+
  ggtitle("Amplitude EVI2")+
  scale_fill_manual(values=c("purple", "darkgrey", "orange2"))+
  ylab("Amplitude EVI2")+
  xlab("Vegetation type")


png("EVI2_vars.png", width = 900)
(a3|b3)/(c3|d3)
dev.off()

a4 <- data %>%  ggplot()+
  aes(x= tree_cover_wet, fill = factor(Vegetation_type))+
  geom_histogram(show.legend = FALSE)+
  ggtitle("Tree cover (densiometer)")+
  geom_v(x_intercept=60)+
  scale_fill_manual(values=c("purple", "darkgrey", "orange2"))+
  xlab("Tree cover (%)")+
  ylab("Count")

b4 <- data %>% ggplot()+
  aes(x = tree_cover_hansen, fill = factor(Vegetation_type))+
  geom_histogram(show.legend = FALSE)+
  geom_v(x_intercept=60)+
  ggtitle("Tree cover (Remote estimation)")+
  scale_fill_manual(values=c("purple", "darkgrey", "orange2"))+
  xlab("Tree cover (%)")+
  ylab("Count")

png("tree_covers.png", width = 900)
a4|b4
dev.off()

a5 <- traits %>% 
  ggplot()+
  aes(y = Tree_height, fill = factor(Vegetation_type), x = factor(Vegetation_type))+
  geom_boxplot(show.legend = FALSE)+
  ggtitle("Tree height")+
  scale_fill_manual(values=c("purple", "darkgrey", "orange2"))+
  ylab("Tree height (m)")+
  xlab("Vegetation type")

b5 <- traits %>%
  ggplot()+
  aes(y = basal_area_total, fill = factor(Vegetation_type), x = factor(Vegetation_type))+
  geom_boxplot(show.legend = FALSE)+
  ggtitle("Basal area total")+
  scale_fill_manual(values=c("purple", "darkgrey", "orange2"))+
  ylab("Basal area total")+
  xlab("Vegetation type")

c5 <- data %>% mutate(Vegetation_type=traits$Vegetation_type) %>% 
  ggplot()+
  aes(y = tree_cover_wet, fill = factor(Vegetation_type), x = factor(Vegetation_type))+
  geom_boxplot(show.legend = FALSE)+
  ggtitle("Tree cover (densiometer)")+
  scale_fill_manual(values=c("purple", "darkgrey", "orange2"))+
  ylab("Tree cover")+
  xlab("Vegetation type")


d5 <- data %>% mutate(Vegetation_type=traits$Vegetation_type) %>% 
  ggplot()+
  aes(y = freq_fire, fill = factor(Vegetation_type), x = factor(Vegetation_type))+
  geom_boxplot(show.legend = FALSE)+
  ggtitle("Frequência de fogo", subtitle = "Período de 1985-2018")+
  scale_fill_manual(values=c("purple", "darkgrey", "orange2"))+  ylab("Times burned")+
  xlab("Vegetation type")

png("./traits_e_fogo.png", width = 900)
(a5|b5)/(c5|d5)
dev.off()

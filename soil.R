# Soil
# Figures + principal component analysis.

library(tidyverse)
library(patchwork)
library(RColorBrewer)
library(vegan)


solo <- read.table("/home/marcio/Documentos/data/solo/solo-20210202T190903Z-001/solo/fis_quim_solo.txt", h = T)

solo$`Vegetation type` <- c(rep("savanna", 10),rep("gallery forest", 10),rep("dry forest", 10))
solo$Areia <- (solo$Areia_Fina + solo$Areia_Grossa)

# Soil granulometry

b3 <- solo %>% ggplot() +
  aes(x = `Vegetation type`, y = Areia, fill = factor(`Vegetation type`)) +
  geom_boxplot(show.legend = F) +
  ggtitle("A")+
  ylab("Sand")+
  xlab("Vegetation Type")+ 
  scale_fill_manual(values=c("purple", "darkgrey", "orange2"))
c3 <- solo %>% ggplot() +
  aes(x = `Vegetation type`, y = Silte, fill = factor(`Vegetation type`)) +
  geom_boxplot(show.legend = F) +
  ggtitle("B")+
  ylab("Silt")+
  xlab("Vegetation Type")+ 
  scale_fill_manual(values=c("purple", "darkgrey", "orange2"))
d3 <- solo %>% ggplot() +
  aes(x = `Vegetation type`, y = Argila, fill = factor(`Vegetation type`)) +
  geom_boxplot(show.legend = F) +
  ggtitle("C")+
  ylab("Clay")+
  xlab("Vegetation Type")+ 
  scale_fill_manual(values=c("purple", "darkgrey", "orange2"))

b3|c3|d3

# Principal component analysis of nutrient availability on granulometry

solo_pca <- solo %>% select(P, Ca2., K, Mg2., pH_H2O,t, Cu, Zn, SB, Mn, Al3._Alumínio_trocável, N,H.nbsp_..nbsp_Al_Acidez.nbsp_potencial_Al3., Fe, N, m, V, Areia, Silte, Argila) %>% drop_na() %>% decostand(method = "standard") %>%rda()
solo_pca %>% summary

plot(solo_pca, xlab = "PC1 (68.2 %)", ylab = "PC2 (1.28%)")

soloPC1 <- solo_pca$CA$u[,1]
#soloPC2 <- solo_pca$CA$u[,2]
m_t <- lm(Coupling_abs ~ t, c(solo,coup))
slopea <- m_t$coefficients[2] 
intercepta <- m_t$coefficients[1] 

c(coup,solo) %>% as_tibble() %>% 
  ggplot()+
  aes(x = t, y = Coupling_abs)+
  geom_jitter(show.legend = F, aes(color = factor(`Vegetation type`)))+
  geom_smooth(method = "lm")+ 
  scale_color_manual(values=c("purple", "darkgrey", "orange2"))+
  geom_text(aes(x = 8, y = 0.5), label = paste0("Coupling = ", intercepta %>% substr(1,5), " + ", slopea %>% substr(1,5), " * Cation exchange capacity"), show.legend = F, color = "black", size = 3.3)+
  geom_text(aes(x = 8, y = 0.47), label = paste0("adj R² = ", substr(summary(m_t)[9], 1, 4), ",", "p-value < 0.001"), show.legend = F, color = "black", size = 3.3)+
  ggtitle("A")+
  xlab("Cation exchange capacity")+
  ylab("Magnitude of coupling")


m_t <- lm(Coup_lag_max ~ t, c(solo,coup))
slopea <- m_t$coefficients[2] 
intercepta <- m_t$coefficients[1] 

c(coup,solo) %>% as_tibble() %>% 
  ggplot()+
  aes(x = t, y = Coup_lag_max)+
  geom_jitter(show.legend = F, aes(color = factor(`Vegetation type`)))+
  geom_smooth(method = "lm")+ 
  scale_color_manual(values=c("purple", "darkgrey", "orange2"))+
  geom_text(aes(x = 8, y = 0.5), label = paste0("Coupling = ", intercepta %>% substr(1,5), " + ", slopea %>% substr(1,5), " * Cation exchange capacity"), show.legend = F, color = "black", size = 3.3)+
  geom_text(aes(x = 8, y = 0.47), label = paste0("adj R² = ", substr(summary(m_t)[9], 1, 4), ",", "p-value < 0.001"), show.legend = F, color = "black", size = 3.3)+
  ggtitle("B")+
  xlab("Cation exchange capacity")+
  ylab("Maximum coupling")

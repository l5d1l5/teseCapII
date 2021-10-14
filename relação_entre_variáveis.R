fire <- read.table("./fire_freq_pontos.txt")

tc_field <- read.table("./tc_field.txt", h = T)
#write.table(tc_field, "tc_field.txt")

tc_hansen <- read.table("./tc_hansen.txt", h = T) 
#write.table(tc_hansen, "tc_hansen.txt")


coup <- read.table("./coup.txt", h = T) 
#write.table(coup, "coup.txt")

traits <- read.table("./traits_community_mean.txt", h = T)
#write.table(traits, "traits.txt")

data <- coup %>% add_column(freq_fire = fire_frequency_pontos[1:30,1]) %>% add_column(tree_cover_wet = tc_field$wet)%>% add_column(tree_cover_dry = tc_field$dry) %>% add_column(tree_cover_hansen = tc_hansen[1:30,1]) %>% add_column(phenology = (tc_field$wet-tc_field$dry))


tc_field %>% head

traits %>% head()                                                         

plot(Coup_lag_max ~ basal_area_total, c(traits,coup), col = factor(Vegetation_type))

plot(Coup_lag_max ~ Tree_height, c(traits,coup), col = factor(Vegetation_type))

plot(Coup_lag_max ~ Bark_thickness, c(traits,coup), col = factor(Vegetation_type))


plot(Coupling_abs ~ basal_area_total, c(traits,coup), col = factor(Vegetation_type))

plot(Coupling_abs ~ Tree_height, c(traits,coup), col = factor(Vegetation_type))

plot(Coupling_abs ~ Bark_thickness, c(traits,coup), col = factor(Vegetation_type))


boxplot(basal_area_total ~ factor(Vegetation_type), traits)
boxplot(Tree_height ~ factor(Vegetation_type), traits)
boxplot(Bark_thickness ~ factor(Vegetation_type), traits)
traits

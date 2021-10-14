fire <- read.table("./fire_freq_pontos.txt")

tc_field <- read.table("./tc_field.txt", h = T) %>%
  rename(Vegetation_type = Biome)
write.table(tc_field, "tc_field.txt")

tc_hansen <- read.table("./tc_hansen.txt", h = T) 
#write.table(tc_hansen, "tc_hansen.txt")


coup <- read.table("./coup.txt", h = T) 
#write.table(coup, "coup.txt")

traits <- read.table("./traits.txt", h = T) %>% select(bark, Alt, CAP)
#write.table(traits, "traits.txt")

data <- coup %>% add_column(freq_fire = fire_frequency_pontos[1:30,1]) %>% add_column(tree_cover_wet = tc_field$wet)%>% add_column(tree_cover_dry = tc_field$dry) %>% add_column(tree_cover_hansen = tc_hansen[1:30,1]) %>% add_column(phenology = (tc_field$wet-tc_field$dry))


                                                                                                                                                                                                                                 
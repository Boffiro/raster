
### Script creation des temoins pour 

# Chargement JDD 

rm(list = setdiff(ls(),""))

raster.stack <- terra::rast("JDD-raster-donnees-miniriz.tif") # JDD de test

# Ici le raster represente le pourcentage de racine presente dans la surface de 1cm2 que represente un pixel.
# On a donc une mesure en cm par cm2

raster.stack.temoin.res1 <- raster.stack

raster.stack.temoin.res2 <- terra::aggregate(raster.stack, fact = 2, fun = "sum") / 2

raster.stack.temoin.res5 <- terra::aggregate(raster.stack, fact = 5, fun = "sum") / 5

#plot(raster.stack.temoin.res1) + plot(raster.stack.temoin.res2) + plot(raster.stack.temoin.res5)

RLD.temoin.res1 <- terra::as.data.frame(raster.stack.temoin.res1, xy = T)
RLD.temoin.res2 <- terra::as.data.frame(raster.stack.temoin.res2, xy = T)
RLD.temoin.res5 <- terra::as.data.frame(raster.stack.temoin.res5, xy = T)

RLD.temoin.res1 <- RLD.temoin.res1 %>%
  select(-x) %>%
  group_by(y) %>%
  summarise(across(everything(), sum))

RLD.temoin.res2 <- RLD.temoin.res2 %>%
  select(-x) %>%
  group_by(y) %>%
  summarise(across(everything(), sum))

RLD.temoin.res5 <- RLD.temoin.res5 %>%
  select(-x) %>%
  group_by(y) %>%
  summarise(across(everything(), sum))

#### Graphe ####

#ggplot(data = RLD.temoin.res1, aes(y = y)) + geom_path(aes(x = `profil_2022-07-25_154275_T015.csv`)) + scale_x_continuous(name = "RLD en cm.cm-3", limits = c(0, 5), position = "top") + scale_y_continuous(name = "Profondeur en cm", limits = c(200, 0), trans = "reverse") + theme_classic()

#ggplot(data = RLD.temoin.res2, aes(y = y)) + geom_path(aes(x = `profil_2022-07-25_154275_T015.csv`)) + scale_x_continuous(name = "RLD en cm.cm-3", limits = c(0, 5), position = "top") + scale_y_continuous(name = "Profondeur en cm", limits = c(200, 0), trans = "reverse") + theme_classic()

#ggplot(data = RLD.temoin.res5, aes(y = y)) + geom_path(aes(x = `profil_2022-07-25_154275_T015.csv`)) + scale_x_continuous(name = "RLD en cm.cm-3", limits = c(0, 5), position = "top") + scale_y_continuous(name = "Profondeur en cm", limits = c(200, 0), trans = "reverse") +  theme_classic()

rm(list = setdiff(ls(), c("RLD.temoin.res1", "RLD.temoin.res2", "RLD.temoin.res5")))

save.image("JDD-temoins-RLD-res-1-2-5.RData")


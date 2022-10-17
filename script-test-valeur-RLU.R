
#### Script Analyse Spatiale des Profils Racinaires ####

# Script de test de la valeur du RLU utilisee pour pouvoir calculer la RLD a partir d'une meseure de profil racinaires
# 
# Objectif : test une variation de la valeur de RLU pour trouver la valeur qui donnerait le meilleur résultat

### Chargement Paquets ####

source("script-chargement-paquets.R")

### Chargement donnees temoins ####

load("JDD-temoins-RLD-res-1-2-5.RData")
# Je charge en tant que temoin des dataframes contenants des mesures de RLD en fonction de la profondeur
# J'ai au cas ou trois resolution differentes pour tester le tout

### Chargement donnees test ####

raster.stack <- terra::rast("JDD-raster-donnees-miniriz.tif")
# Je charge en tant que JDD a tester un stack de raster prealablement mis en forme.
# Les donnees sont quantitatives : il s'agit du 

### Desagregation

function.desagreg <- c("mean", "max", "min", "median", "sum", "modal")

i = 5

raster.stack.res1 <- raster.stack

raster.stack.res2 <- terra::aggregate(raster.stack, fact = 2, fun = function.desagreg[i]) / 2

raster.stack.res5 <- terra::aggregate(raster.stack, fact = 5, fun = function.desagreg[i]) / 5
# J'obtiens donc des raster representant la lng

### Conversion Quanti vers Quali

raster.stack.save <- raster.stack

# Je vais convertir mon raster d'une valeur quantitative de cm par cm2 en valeur qualitative
# Pour ce faire, on va convertir les 

RLU <- 1

matrix.reclassify <- matrix(c(0 * RLU, 1 * RLU, 1,
                              1 * RLU, 2 * RLU, 2,
                              2 * RLU, 3 * RLU, 3,
                              3 * RLU, 4 * RLU, 4,
                              4 * RLU, 5 * RLU, 5,
                              5 * RLU, 6 * RLU, 6,
                              6 * RLU, 7 * RLU, 7,
                              7 * RLU, 8 * RLU, 8,
                              8 * RLU, 9 * RLU, 9), nrow = 9, ncol = 3, byrow = 3)

raster.stack.res1 <- terra::classify(raster.stack.res1, rcl = matrix.reclassify, include.lowest = F)
raster.stack.res2 <- terra::classify(raster.stack.res2, rcl = matrix.reclassify, include.lowest = F)
raster.stack.res5 <- terra::classify(raster.stack.res5, rcl = matrix.reclassify, include.lowest = F)

#plot(raster.stack.res1, type = "class") + plot(raster.stack.res2, type = "class") + plot(raster.stack.res5, type = "class")
#plot(raster.stack.res1$`profil_2022-01-31_hydro_T002.csv`, type = "class")  + plot(raster.stack.res2$`profil_2022-01-31_hydro_T002.csv`, type = "class") + plot(raster.stack.res5$`profil_2022-01-31_hydro_T002.csv`, type = "class")
#res(raster.stack.res1) + res(raster.stack.res2) + res(raster.stack.res5)

#print(sum(raster.stack.res1$`profil_2022-01-31_hydro_T002.csv`[10,])) + print(sum(raster.stack.res2$`profil_2022-01-31_hydro_T002.csv`[10,])) + print(sum(raster.stack.res5$`profil_2022-01-31_hydro_T002.csv`[10,]))

### Calcul RLD ####

RLD.res1 <- terra::as.data.frame(raster.stack.res1, xy = T)
RLD.res2 <- terra::as.data.frame(raster.stack.res2, xy = T)
RLD.res5 <- terra::as.data.frame(raster.stack.res5, xy = T)

RLD.res1 <- RLD.res1 %>%
  select(-x) %>%
  group_by(y) %>%
  summarise(across(everything(), sum))

RLD.res2 <- RLD.res2 %>%
  select(-x) %>%
  group_by(y) %>%
  summarise(across(everything(), sum))

RLD.res5 <- RLD.res5 %>%
  select(-x) %>%
  group_by(y) %>%
  summarise(across(everything(), sum))

### Graphe ####

ggplot(data = RLD.res1, aes(y = y)) + 
  geom_path(aes(x = `profil_2022-01-31_hydro_T002.csv`)) + 
  scale_x_continuous(name = "RLD en cm.cm-3", limits = c(0, 5), position = "top") + 
  scale_y_continuous(name = "Profondeur en cm", limits = c(200, 0), trans = "reverse") + 
  theme_classic()

ggplot(data = RLD.temoin.res1, aes(y = y)) + 
  geom_path(aes(x = `profil_2022-01-31_hydro_T002.csv`)) + 
  scale_x_continuous(name = "RLD en cm.cm-3", limits = c(0, 5), position = "top") + 
  scale_y_continuous(name = "Profondeur en cm", limits = c(200, 0), trans = "reverse") + 
  theme_classic()

ggplot(data = RLD.res2, aes(y = y)) + 
  geom_path(aes(x = `profil_2022-01-31_hydro_T002.csv`)) + 
  scale_x_continuous(name = "RLD en cm.cm-3", limits = c(0, 5), position = "top") + 
  scale_y_continuous(name = "Profondeur en cm", limits = c(200, 0), trans = "reverse") + 
  theme_classic()

ggplot(data = RLD.res5, aes(y = y)) +
  geom_path(aes(x = `profil_2022-01-31_hydro_T002.csv`)) +
  scale_x_continuous(name = "RLD en cm.cm-3", limits = c(0, 5), position = "top") + 
  scale_y_continuous(name = "Profondeur en cm", limits = c(200, 0), trans = "reverse") +  
  theme_classic()

### Optim ####

optim(par = ,
      fn = )


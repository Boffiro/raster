
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

terra::rast("JDD-raster-donnees-miniriz.tif")
# Je charge en tant que JDD a tester un stack de raster prealablement mis en forme.
# Les donnees sont quantitatives : il s'agit du 

### Conversion Quanti vers Quali

# Je vais convertir mon raster d'une valeur quantitative de cm par cm2 en 

raster.stack <- 


### Desagregation

raster.stack.test.res1 <- raster.stack

raster.stack.test.res2 <- terra::aggregate(raster.stack, fact = 2, fun = "sum") / 2

raster.stack.test.res5 <- terra::aggregate(raster.stack, fact = 5, fun = "sum") / 5
# J'obtiens donc des raster representant l















plot(raster.stack)


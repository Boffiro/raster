
#### Script Analyse Spatiale nottament avec des rasters ####

### Chargement Paquets

source("script-chargement-paquets.R")

### Création Fichier Test 

test <- rast(xmin = 0, xmax = 100, ymin = 0, ymax = 200, res = 1)

values(test) <- sample(x = 1:2000, replace = T, size = 13) 

test
plot(test, type = "class")

### Chargement Donnees Profils 

### Chargement Donnees Miniriz

main <- read.csv2("17 0N-VLB-2021-1-data.csv")
# Je charge mon fichier source
# Je retire les cases inutiles présentes dans le template de base servant de légendes et je ne garde que les données pures

main$X <- NULL
main$prof...largeur <- NULL
main$fichier <- NULL

main <- rast(x = as.matrix(main))
# Je crée un raster à partir de ma matrice de données stockée en .csv

terra::ext(main) <- c(0, 2 * ncol(main), 0, 2 * nrow(main))
main <- terra::disagg(x = main, fact = 2, method = "near")
# Avec ces deux lignes, j'agrandis mon raster de deux fois en taille en gardant la même résolution. 
# L'objetcif était de passer de 1 pixel = 4cm² à 1 pixel = 1cm². 
# Comme ça la résolution entre les rasters issu des données de profils seront de la même résolution que les rasters issu des données de miniriz.
# En plus ce sera plus simple pour travailler. 
# Ce que l'on obtient est donc une représentation des profils relevés avec pour chaque pixel représentant 1cm², la valeur que l'on a est 

plot(main, type = "class")

### Raster de la profondeur 

source("script-creation-raster-profondeur.R")

### Masque Présence/Absence racine

source("script-creation-mask.R")

plot(profil.deux.classes)

plot(profil.trois.classes)

### Calcul de la densité

if (isTRUE(res(prof)[1] == res(profil.deux.classes)[1]) | isTRUE(res(prof)[2] == res(profil.deux.classes)[2])) {
  
  densite.base <- main
  
  densite.base[densite.base == 1] <- ncell(densite.base[densite.base == 1]) / ncell(densite.base) 
  
}

plot(densite.base)



isTRUE(res(prof)[1] == res(profil.trois.classes)[1]) | isTRUE(res(prof)[2] == res(profil.trois.classes)[2])

if (condition) {
  
}





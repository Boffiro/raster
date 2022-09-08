
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

test <- read.csv2("17 0N-VLB-2021-1-data.csv")
# Je charge mon fichier source
# Je retire les cases inutiles présentes dans le template de base servant de légendes et je ne garde que les données pures

test$X <- NULL
test$prof...largeur <- NULL
test$fichier <- NULL

test <- rast(x = as.matrix(test))
# Je crée un raster à partir de ma matrice de données stockée en .csv

terra::ext(test) <- c(0, 2 * ncol(test), 0, 2 * nrow(test))
test <- terra::disagg(x = test, fact = 2, method = "near")
# Avec ces deux lignes, j'agrandis mon raster de deux fois en taille en gardant la même résolution. 
# L'objetcif était de passer de 1 pixel = 4cm² à 1 pixel = 1cm². 
# Comme ça la résolution entre les rasters issu des données de profils seront de la même résolution que les rasters issu des données de miniriz.
# En plus ce sera plus simple pour travailler. 

test
plot(test)










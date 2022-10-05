
### Script pour créer un raster de deux classes

library(terra)

### Convertir Raster de profil en Raster de présence de racines à deux classes (0 et 1)

profil.deux.classes <- main

profil.deux.classes[profil.deux.classes == 0] <- 0
profil.deux.classes[profil.deux.classes >= 1] <- 1

### Convertir Raster de profil en Raster de présence de racines à deux classes (0 et 1)

profil.trois.classes <- main

profil.trois.classes[profil.trois.classes == 0] <- 0
profil.trois.classes[profil.trois.classes >= 2] <- 2


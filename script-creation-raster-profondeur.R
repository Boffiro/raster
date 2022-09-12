
### Script pour créer un raster donnant la profondeur 

library(terra)

## Dimensions

larg = 100
# Rentrer la largeur en pixel que l'on veut
long = 130
# Rentrer la longeur en pixel que l'on veut

xmin = 0
xmax = 100
ymin = 0
ymax = 130
# Rentrer les coordonnées que l'on souhaite utilser pou délimiter notre 

## Création raster
test <- rast(ncol = larg, nrow = long, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)

values(test) <- 1:ncell(test)

matrix.prof <- matrix(nrow = ymax, ncol = 3)

for (i in 1:nrow(matrix.prof)) {
  
  matrix.prof[i,1] <- round(ncell(test) * (i-1) / nrow(matrix.prof))  
  matrix.prof[i,2] <- round(ncell(test) * i / nrow(matrix.prof))
  matrix.prof[i,3] <- i
  
}

prof <- classify(test, rcl = matrix.prof)

## Sauvegarde Raster

#writeRaster(x = prof, filename = "profondeur.tif")

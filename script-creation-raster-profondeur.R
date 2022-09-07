
### Script pour créer un raster donnant la profondeur 

library(terra)

## Dimensions

larg = 
# Rentrer la largeur en pixel que l'on veut
long = 
# Rentrer la longeur en pixel que l'on veut

xmin = 
xmax = 
ymin = 
ymax = 
# Rentrer les coordonnées que l'on souhaite utilser pou délimiter notre 

## Création raster
test <- rast(ncol = larg, nrow = long, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)

values(test) <- 1:ncell(test)

matrix.prof <- matrix(nrow = 200, ncol = 3)

for (i in 1:nrow(matrix.prof)) {
  
  matrix.prof[i,1] <- round(ncell(test) * (i-1) / nrow(matrix.prof))  
  matrix.prof[i,2] <- round(ncell(test) * i / nrow(matrix.prof))
  matrix.prof[i,3] <- -i
  
}

prof <- classify(test, rcl = matrix.prof)
plot(prof)

## Sauvegarde Raster

writeRaster(x = prof, filename = "profondeur.tif")

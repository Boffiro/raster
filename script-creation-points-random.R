
### Script pour créer un raster donnant la profondeur 

library(terra)

## Dimensions

larg = larg
# Rentrer la largeur en pixel que l'on veut
long = long
# Rentrer la longeur en pixel que l'on veut

xmin = 
xmax = 
ymin = 
ymax = 
# Rentrer les coordonnées que l'on souhaite utilser pou délimiter notre 
  
nb.points = 100
# Nombre de point que l'on veut 

## Création Points

coord.point <- matrix(nrow = nb.points, ncol = 2)

for (i in 1:nrow(coord.point)) {
  
  coord.point[i,1] <- sample(xmin:xmax, size = 1)  
  coord.point[i,2] <- sample(ymin:ymax, size = 1)
  
}

point <- vect(coord.point)

plot(point)

## Sauvegarde Vecteur 

writeVector(point, filename = "points.shp")

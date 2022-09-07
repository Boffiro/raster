
#### Script Analyse Spatiale nottament avec des rasters ####

### Chargement Paquets

source("script-chargement-paquets.R")

### Création Fichier Test 

test <- read.csv2("2019-Nimes-BleDur_Fourques_depthed.csv", sep = ";" )

test <- test %>%
  filter(tube == "T4", date == "30.04.19") %>% 
  select(dpi, width, height, surface0, surface1) %>% 
  mutate(surface = surface0 + surface1) %>%
  mutate(pourc = surface1 / surface)

surface.tot = sum(test$surface) 
long = sum(test$height)
larg = unique(test$width)
pourc = mean(test$pourc)

test <- rast(ncol = larg, nrow = long, xmin = 0, xmax = 100, ymin = -200, ymax = 0)

test
plot(test)

values(test) <- 1:ncell(test)
test
plot(test)

matrix.prof <- matrix(nrow = 200, ncol = 3)

for (i in 1:nrow(matrix.prof)) {

  matrix.prof[i,1] <- round(ncell(test) * (i-1) / nrow(matrix.prof))  
  matrix.prof[i,2] <- round(ncell(test) * i / nrow(matrix.prof))
  matrix.prof[i,3] <- -i
  
}

prof <- classify(test, rcl = matrix.prof)
plot(prof)

writeRaster(x = prof, filename = "profondeur.tif")



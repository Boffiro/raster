
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

values(test) <- 1

test
plot(test)

### 

### 

### Rasterisation 

raster.point <- rasterize(x = point, y = raster, background = 0)
# étape de rasterisation des vecteurs relevés















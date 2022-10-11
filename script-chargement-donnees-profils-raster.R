
### Script Chargement de données de profils racinaires en raster

raster.stack <- terra::rast()

for (i in 1:length(list.files("bdd-essais-profils-data/"))) {
  
  path <- list.files("bdd-essais-profils-data/")[i] # Changer 1 par i pour faire boucler
  
  raw <- read.csv2(file = paste0("bdd-essais-profils-data/",path))
  # Je charge mon fichier source
  
  raw$X <- NULL
  raw$prof...largeur <- NULL
  raw$fichier <- NULL
  # Je retire les cases inutiles présentes dans le template de base servant de légendes et je ne garde que les données pures
  
  raster.temp <- rast(x = as.matrix(raw))
  # Je crée le raster
  
  names(raster.temp) <- path
  # Je renome la couche du raster
  
  #print(ext(raster.temp))
  
  raster.stack <- c(raster.stack, raster.temp)
  # Je combine les raster pour créer un stack
  
}

rm(list = setdiff(ls(), "raster.stack"))
# Je ne garde que le stack crée

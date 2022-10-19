
source("script-chargement-paquets.R")

load("JDD-temoins-RLD-res-1-2-5.RData")

raster.stack <- terra::rast("JDD-raster-donnees-miniriz.tif")

### Choix des paramètres ###

i = 5 # Choix de la fonction de desaggrégation
RLU <- 1 # Choix de la valeur seuil de RLU

### ###

test <- function(data, RLU, i) {
  
  require(terra)
  require(dplyr)
  
  function.desagreg <- c("mean", "max", "min", "median", "sum", "modal")

  matrix.reclassify <- matrix(c(0 * RLU, 1 * RLU, 1,
                                1 * RLU, 2 * RLU, 2,
                                2 * RLU, 3 * RLU, 3,
                                3 * RLU, 4 * RLU, 4,
                                4 * RLU, 5 * RLU, 5,
                                5 * RLU, 6 * RLU, 6,
                                6 * RLU, 7 * RLU, 7,
                                7 * RLU, 8 * RLU, 8,
                                8 * RLU, 9 * RLU, 9), nrow = 9, ncol = 3, byrow = 3)
  
  raster.stack <- data
  
  raster.stack.res1 <- raster.stack
  
  raster.stack.res2 <- terra::aggregate(raster.stack, fact = 2, fun = function.desagreg[i]) / 2
  
  raster.stack.res5 <- terra::aggregate(raster.stack, fact = 5, fun = function.desagreg[i]) / 5
  
  raster.stack.res1 <- terra::classify(raster.stack.res1, rcl = matrix.reclassify, include.lowest = F)
  raster.stack.res2 <- terra::classify(raster.stack.res2, rcl = matrix.reclassify, include.lowest = F)
  raster.stack.res5 <- terra::classify(raster.stack.res5, rcl = matrix.reclassify, include.lowest = F)
  
  RLD.res1 <- terra::as.data.frame(raster.stack.res1, xy = T)
  RLD.res2 <- terra::as.data.frame(raster.stack.res2, xy = T)
  RLD.res5 <- terra::as.data.frame(raster.stack.res5, xy = T)
  
  RLD.res1 <- RLD.res1 %>%
    select(-x) %>%
    group_by(y) %>%
    summarise(across(everything(), sum))
  
  RLD.res2 <- RLD.res2 %>%
    select(-x) %>%
    group_by(y) %>%
    summarise(across(everything(), sum))
  
  RLD.res5 <- RLD.res5 %>%
    select(-x) %>%
    group_by(y) %>%
    summarise(across(everything(), sum))

  RLD.predict <- list(RLD.res1 = RLD.res1, RLD.res2 = RLD.res2, RLD.res5 = RLD.res5)
  
  RLD.temoin <- list(RLD.temoin.res1 = RLD.temoin.res1, RLD.temoin.res2 = RLD.temoin.res2, RLD.temoin.res5 = RLD.temoin.res5)
  
#  result <- list(RLD.res1 = RLD.res1, RLD.res2 = RLD.res2, RLD.res5 = RLD.res5, RLD.temoin.res1 = RLD.temoin.res1, RLD.temoin.res2 = RLD.temoin.res2, RLD.temoin.res5 = RLD.temoin.res5)
  
  result <- list(RLD.predict = RLD.predict, RLD.temoin = RLD.temoin)
  
#  return(RLD.predict)
#  return(RLD.temoin)

### calcul indicateurs  
  
  return(result)

}

result <- test(data = raster.stack, RLU = 0.25, i = 2)

#optim()








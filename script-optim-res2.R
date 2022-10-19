
source("script-chargement-paquets.R")

load("JDD-temoins-RLD-res-1-2-5.RData")
rm(RLD.temoin.res1)
rm(RLD.temoin.res5)

raster.stack <- terra::rast("JDD-raster-donnees-miniriz.tif")

#### Fonction a optimiser ####

test <- function(RLU, data, i) {
  
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
  
  raster.stack.res2 <- terra::aggregate(raster.stack, fact = 2, fun = function.desagreg[i]) / 2
  
  raster.stack.res2 <- terra::classify(raster.stack.res2, rcl = matrix.reclassify, include.lowest = F)
  
  RLD.res2 <- terra::as.data.frame(raster.stack.res2, xy = T)
  
  RLD.res2 <- RLD.res2 %>%
    select(-x) %>%
    group_by(y) %>%
    summarise(across(everything(), sum))
  
  RLD.predict <- list(RLD.res2 = RLD.res2)
  
  RLD.temoin <- list(RLD.temoin.res2 = RLD.temoin.res2)
  
  ### calcul indicateurs  
  
  RMSE <- rmse(actual = as.vector(unlist(RLD.temoin$RLD.temoin.res2[,2:length(RLD.predict$RLD.res2)])),
               predicted = as.vector(unlist(RLD.predict$RLD.res2[,2:length(RLD.predict$RLD.res2)])))
  
  bias <- bias(actual = as.vector(unlist(RLD.temoin$RLD.temoin.res2[,2:length(RLD.predict$RLD.res2)])),
               predicted = as.vector(unlist(RLD.predict$RLD.res2[,2:length(RLD.predict$RLD.res2)])))
  
  result <- list(RLD.predict = RLD.predict, RLD.temoin = RLD.temoin, RMSE = RMSE, bias = bias)
  
  return(RMSE)
  
}

#### Choix des parametres ####

#i <- c(1:6)# Choix de la fonction de desaggregation
#RLU <- seq(from = 0, to = 1, by = 0.1) # Choix de la valeur seuil de RLU

#### Calcul de la ####

result <- test(data = raster.stack, RLU = 1, i = 4)

test(data = raster.stack, RLU = 0.5, i = 4)
#optim(par = RLD.temoin.res2, )
#

#for (i in 1:6) {

 i = 6
 
 optim(par = 0.1, #RLU
       fn = test, # fonction objectif = "test"
       i = i,
       data = raster.stack,
       method = "L-BFGS-B",
       lower = 0, # RLU min
       upper = 5) # RLU max

#}



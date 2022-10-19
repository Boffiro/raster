
#### Script optimisation

rm(list = setdiff(ls(),""))

### Chargement paquets 

source("script-chargement-paquets.R")

### Chargement donnees

load("JDD-temoins-RLD-res-1-2-5.RData") # JDD temoin
rm(RLD.temoin.res1)
rm(RLD.temoin.res2)

raster.stack <- terra::rast("JDD-raster-donnees-miniriz.tif") # JDD de test

### Fonction calcul RMSE ####

rmse <- function(yobs, ypred) {
  r       = list(yobs=yobs, ypred=ypred, n=0, bias=0, minres=0, maxres=0, rmse=0, mse=0, mae=0, ef=0, tot=NULL, ok=FALSE)
  ecart   = yobs - ypred
  ymoy    = mean(yobs)
  yvar    = mean((yobs-ymoy)^2)
  
  r$n     = nrow(as.data.frame(ypred))
  r$moyobs = mean (yobs)
  r$moyprd = mean (ypred)
  r$bias  = mean(ecart)
  r$minres = min(ecart)
  r$maxres = max(ecart)
  
  r$mse   = mean(ecart^2)
  r$mae   = mean(abs(ecart))
  r$rmse  = sqrt(r$mse)
  
  r$ok    = (yvar != 0)
  if (r$ok) r$ef = 1 - r$mse / yvar else r$ef = 0
  
  r$tot   = cbind(total=0, n=r$n, moyobs=r$moyobs, moyprd=r$moyprd, biais=r$bias, minres=r$minres, maxres=r$maxres, rmse=r$rmse, ef=r$ef)
  rownames(r$tot) = NULL
  
  r$rmse  = sqrt(r$mse)
  
  r <- r$rmse
    
  return(r)
}

### Fonction d'ordre ####

i = 6
#RLU = 0.5

function.order <- function(RLU, data) {
  
  require(terra)
  require(dplyr)
  
  function.desagreg <- c("mean", "max", "min", "median", "sum", "modal")
  # Liste des cores possibles pour la desagregation du raster
  
  matrix.reclassify <- matrix(c(0 * RLU, 1 * RLU, 1,
                                1 * RLU, 2 * RLU, 2,
                                2 * RLU, 3 * RLU, 3,
                                3 * RLU, 4 * RLU, 4,
                                4 * RLU, 5 * RLU, 5,
                                5 * RLU, 6 * RLU, 6,
                                6 * RLU, 7 * RLU, 7,
                                7 * RLU, 8 * RLU, 8,
                                8 * RLU, 9 * RLU, 9), nrow = 9, ncol = 3, byrow = 3)
  # Matrice de raclassification basee sur la valeur de RLU
  
  raster.stack.res5 <- terra::aggregate(raster.stack, fact = 5, fun = function.desagreg[i]) / 25
  
  raster.stack.res5 <- terra::classify(raster.stack.res5, rcl = matrix.reclassify, include.lowest = F)
  
  RLD.res5 <- terra::as.data.frame(raster.stack.res5, xy = T)
  
  RLD.res5 <- RLD.res5 %>%
    select(-x) %>%
    group_by(y) %>%
    summarise(across(everything(), sum))
  
  RLD.predict <- list(RLD.res5 = RLD.res5)
  
  RLD.temoin <- list(RLD.temoin.res5 = RLD.temoin.res5)
  
  ### calcul indicateurs  
  
  RMSE <- rmse(yobs = as.vector(unlist(RLD.temoin$RLD.temoin.res5[,2:length(RLD.temoin$RLD.temoin.res5)])),
               ypred = as.vector(unlist(RLD.predict$RLD.res5[,2:length(RLD.predict$RLD.res5)])))
  # N.B. : techniquement, on retire aleatoirement des valeurs aux JDD que l'on teste pour que les deux vecteurs soient de longueurs multiples l'un de l'autre
  
  result <- list(RLD.predict = RLD.predict, RLD.temoin = RLD.temoin, RMSE = RMSE)
  
  return(RMSE)

}

### Test ####

#for (j in seq(from = 0, to = 1, by = 0.01)) { print(function.order(RLU = j, data = raster.stack)) }

### Optimisation ####

optim(par = 0.1, #RLU
      fn = function.order, # fonction objectif 
      data = raster.stack,
      method = "L-BFGS-B",
      lower = 0, # RLU min
      upper = 5) # RLU max

#test <- function.order(RLU = 0.5,data = raster.stack, i = 4)



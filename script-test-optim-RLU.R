
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

i = 4
# Choix de la fonction de desagregation que l'on utlisera

#RLU = 0.5
# Choix de la RLU (en cm)

res = 1
# Choix de la resolution du pixel (en cm) que l'on veut 

function.order <- function(RLU, data) {
  
  require(terra)
  require(dplyr)
  
  function.desagreg <- c("mean", "max", "min", "median", "sum", "modal")
  # Liste des cores possibles pour la desagregation du raster
  
  matrix.reclassify <- matrix(c(0 * RLU, 1 * RLU, 0,
                                1 * RLU, 2 * RLU, 1,
                                2 * RLU, 3 * RLU, 2,
                                3 * RLU, 4 * RLU, 3,
                                4 * RLU, 5 * RLU, 4,
                                5 * RLU, 6 * RLU, 5,
                                6 * RLU, 7 * RLU, 6,
                                7 * RLU, 8 * RLU, 7,
                                8 * RLU, 9 * RLU, 8), nrow = 9, ncol = 3, byrow = 3)
  # Matrice de raclassification basee sur la valeur de RLU.
  # Exemple : Pour une valeur de RLU de 0.5 cm , si l'on observe entre 0.5cm (soit 1 X RLU) et 1cm (soit 2 X RLU)
  #  de lineaire de racines par cm2 de surface de profil ; on donnera une note de 1  
  
  raster.stack.res5 <- terra::aggregate(raster.stack, fact = res, fun = function.desagreg[i]) / (res * res)
  # On aggregate les valeurs que l'on a avec la fonction choisie. Puis on divise le tout par la surface du pixel 
  # L'objectif est de conserver la mesure que en cm de lineaire de racine par cm2 de surface observee
  
  raster.stack.res5 <- terra::classify(raster.stack.res5, rcl = matrix.reclassify, include.lowest = F)
  # On reclassifie les mesures de lineaires de racines en cm par cm2 de surface observee en note   
  
  # N.B.: Normalement maintenant, nous avons des rasters qui sont donc le nombre de racines observees dans une case.  
  
  RLD.res5 <- terra::as.data.frame(raster.stack.res5, xy = T)
  # Je mets au format data.frame mes rasters
  
  RLD.res5 <- RLD.res5 %>%
    select(-x) %>%
    group_by(y) %>%
    summarise(across(everything(), sum))
  # On a donc la somme du nombre de racines observees par tranche de sol
  
  RLD.res5[,-1] <- RLD.res5[,-1] * RLU
  # On calcule la RLD par tranche. RLD = nb de racines observees par cm2 de surface X Longueur de Racine Unitaire
  
  RLD.predict <- list(RLD.res5 = RLD.res5)
  
  RLD.temoin <- list(RLD.temoin.res5 = RLD.temoin.res5)
  
  ### calcul indicateurs  
  
  RMSE <- rmse(yobs = as.vector(unlist(RLD.temoin$RLD.temoin.res5[,2:length(RLD.temoin$RLD.temoin.res5)])),
               ypred = as.vector(unlist(RLD.predict$RLD.res5[,2:length(RLD.predict$RLD.res5)])))
  # Calcul du RMSE
  
  result <- list(RLD.predict = RLD.predict, RLD.temoin = RLD.temoin, RMSE = RMSE)
  
  return(RMSE)

}

### Test #### /!\ A ne pas lancer si on ne veut pas /!\ ####

for (j in seq(from = 0, to = 1, by = 0.1)) { 
  
  print(function.order(RLU = j, data = raster.stack))
  }
# Variation RLU

for (j in seq(from = 1, to = 6, by = 1)) {
  i = j
  
  print(function.order(RLU = 0.5, data = raster.stack)) 
  }
# Variation fonction de desagg

for (j in c(1, 2, 5)) {
  res = j
  i = 4
  
  print(function.order(RLU = 0.5, data = raster.stack)) 
  }
# Variation res

### Optimisation ####

optim(par = 0, #RLU
      fn = function.order, # fonction d'ordre 
      data = raster.stack,
      method = "L-BFGS-B",
      control = list(maxit = 5), 
      lower = 0, # RLU min
      upper = 1) # RLU max


optim(par = 0.5, #RLU
      fn = function.order, # fonction d'ordre 
      data = raster.stack,
      method = "Brent",
      lower = 0, # RLU min
      upper = 10) # RLU max

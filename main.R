
#### Script Analyse Spatiale des Profils Racinaires ####

### Chargement Paquets

source("script-chargement-paquets.R")

### Chargement données

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

raster.stack

### Etape de désagréagation

ext(raster.stack) <- c(0, 2 * ncol(raster.stack), 0, 2 * nrow(raster.stack))
# Changement de l'extend des Raster pour permettre que la résolution soit 1:1

raster.stack.2.temoin <- raster.stack
#Je garde de coté le Stack de Raster de res 2:2 qui me servira de temoin

raster.stack <- terra::disagg(x = raster.stack, fact = 2, method = "near")
# Etape de désagrégation qui fait que la résolution est de 1:1

# N.B.: Je change la résolution pour que maintenant, chaque pixel face 1cm de coté.
# C'est la méthode que j'ai trouvé pour pouvoir obtenir ensuite une résolution de 5cm par 5cm comme je le voulais 
# J'utilise la méthode de désagrégation de "nearest neighbour" parce que celle-ci ne changera pas les données que j'ai. 
# En effet, ayant des limites bien rectilignes, les changements de valeurs doivent rester abruptes. 
# De plus, je suis avec un raster représentant des données actuellement qualitatives. 
# Cette méthode permet de conserver les classes de données déterminées. 

raster.stack.5.temoin <- aggregate(raster.stack, fact = 5)

### Etape d'agréagation

liste.test <- data.frame(fact = c(2, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5),
                         fun = c("mean", "median", "max", "min", "modal", "sum", "mean", "median", "max", "min", "modal", "sum"))

for (i in 1:length(liste.test$fact)) {
  
  assign(paste0("raster.stack.", liste.test$fact[i],".", liste.test$fun[i]), 
         value = aggregate(x = raster.stack, fact = liste.test$fact[i], fun = liste.test$fun[i]))
  
}

rm(i)

# Etape de désagrégation qui fait que la résolution est de 5:5
# N.B.: Pour le choix de la fonction que l'on utilise pour la valeur d'aggrégation, le choix n'est pas encore arrété.
# Je dois faire des test avec un JDD issues des données de Miniriz. 
# L'objectif est de trouver la méthode qui fait que la RLD que l'on calcule à partir de données de profils soit le plus cohérent par rapport à la RLD Miniriz. 
# Je dois donc faire passer le JDD Miniriz en tant que raster de 2:2cm, puis en calculer la RLD, et comparer aux résultats que Antoine et Gaethan ont obtenus.  

### Calcul du nombre de racines

liste.raster.racines <- setdiff(ls(), c("raster.stack", "liste.test"))

resultat.NB.racines <- list()

for (i in 1:length(liste.raster.racines)) {
  
  resultat.NB.racines[[i]] <- terra::as.data.frame(x = get(liste.raster.racines[i]), xy = T)

  resultat.NB.racines[[i]] <- resultat.NB.racines[[i]] %>%
    select(-x) %>%
    group_by(y) %>%
    summarise(across(everything(), sum))
  # Calcul du nombre de Racines dans chanque tranche de sol mesurée. Donc on somme chaque ligne 
  
  resultat.NB.racines[[i]]$y <- sort(resultat.NB.racines[[i]]$y, decreasing = T)
  
  resultat.NB.racines[[i]] <- resultat.NB.racines[[i]] %>% arrange(y)
  
  names(resultat.NB.racines)[[i]] <- liste.raster.racines[i]

}

### Calcul de la RLD ####

### Mise en forme résultats

resultat.RLD <- resultat.NB.racines

for (i in 1:length(resultat.RLD)) {
  
  resultat.RLD[[i]] <- resultat.RLD[[i]] %>% 
    summarise(across(), resultat.RLD[[i]] * 0.5 / 25) %>% 
    mutate(y = y / 0.5 * 25)
  
}

rm(list = setdiff(ls(), c("resultat.NB.racines", "resultat.RLD")))

### Etude des résultats

liste.res.5 <- c("raster.stack.5.max", "raster.stack.5.mean", "raster.stack.5.median", "raster.stack.5.min", "raster.stack.5.modal", "raster.stack.5.sum", "raster.stack.5.temoin")

resultat.RLD.etudie <- list()

temp.2 <- data.frame(rm = matrix(ncol = 1, nrow = 26))

for (j in 2:length(resultat.RLD$raster.stack.5.temoin)) {
  
  temp.2 <- data.frame(rm = matrix(ncol = 1, nrow = 26))
  
  for (i in liste.res.5) {
    
    temp <- resultat.RLD[[i]][j]
  
    names(temp) <- i

    temp.2 <- cbind(temp.2, temp)
  
  }
  
  temp.2$rm <- NULL

  resultat.RLD.etudie[[j-1]] <- assign(x = names(resultat.RLD[[i]][j]), value = temp.2)   
  
  names(resultat.RLD.etudie)[j-1] <- names(resultat.RLD$raster.stack.5.temoin)[j]
  
}

rm(list = setdiff(ls(), c("liste.res.5", "resultat.NB.racines", "resultat.RLD", "resultat.RLD.etudie") ))

### Calcul Etude Résultats

liste.res.5 <- setdiff(liste.res.5, "raster.stack.5.temoin")

liste.etude.resultat <- list()

for (i in 1:length(resultat.RLD.etudie)) {

#  for (j in 1:2) {

#    i = 1
    j = 1  
  
    RMSE <- rmse(actual = resultat.RLD.etudie[[i]]$raster.stack.5.temoin,
                 predicted = resultat.RLD.etudie[[i]][,j])

    liste.etude.resultat[[i]][j] <- list(RMSE)
    
#    names(liste.etude.resultat)[i] <- names(resultat.RLD.etudie[i])
    
#    names(liste.etude.resultat[i][[j]]) <- paste0(liste.res.5[j], ".RMSE")
    
#   }
  
}

### Dump ####

# Coder calcul d'indicateur de différences par rapport au Temoin :
#   Deviation
#   Ecart à la moyenne
#   RMSE
#   Efficience
#   ect.

# Extraire 
# Utiliser le package metrics

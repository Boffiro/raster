
#### Script Analyse Spatiale des Profils Racinaires ####

### Chargement Paquets

source("script-chargement-paquets.R")

### Chargement donnees

source("script-chargement-donnees-profils-raster.R")

rm(list = setdiff(ls(), "raster.stack"))
# Je ne garde que le stack cree

raster.stack

### Etape de desagreagation

ext(raster.stack) <- c(0, 2 * ncol(raster.stack), 0, 2 * nrow(raster.stack))
# Changement de l'extend des Raster pour permettre que la resolution soit 1:1

raster.stack.2.temoin <- raster.stack
#Je garde de cote le Stack de Raster de res 2:2 qui me servira de temoin

raster.stack <- terra::disagg(x = raster.stack, fact = 2, method = "near")
# Etape de desagregation qui fait que la resolution est de 1:1

# N.B.: Je change la resolution pour que maintenant, chaque pixel face 1cm de cote.
# C'est la methode que j'ai trouve pour pouvoir obtenir ensuite une resolution de 5cm par 5cm comme je le voulais 
# J'utilise la methode de desagregation de "nearest neighbour" parce que celle-ci ne changera pas les donnees que j'ai. 
# En effet, ayant des limites bien rectilignes, les changements de valeurs doivent rester abruptes. 
# De plus, je suis avec un raster representant des donnees actuellement qualitatives. 
# Cette methode permet de conserver les classes de donnees determinees. 

raster.stack.5.temoin <- aggregate(raster.stack, fact = 5)

### Etape d'agreagation

liste.test <- data.frame(fact = c(2, 2, 2, 2, 2, 2, 5, 5, 5, 5, 5, 5),
                         fun = c("mean", "median", "max", "min", "modal", "sum", "mean", "median", "max", "min", "modal", "sum"))

for (i in 1:length(liste.test$fact)) {
  
  assign(paste0("raster.stack.", liste.test$fact[i],".", liste.test$fun[i]), 
         value = aggregate(x = raster.stack, fact = liste.test$fact[i], fun = liste.test$fun[i]))
  
}

rm(i)

# Etape de desagregation qui fait que la resolution est de 5:5
# N.B.: Pour le choix de la fonction que l'on utilise pour la valeur d'aggregation, le choix n'est pas encore arrete.
# Je dois faire des test avec un JDD issues des donnees de Miniriz. 
# L'objectif est de trouver la methode qui fait que la RLD que l'on calcule a partir de donnees de profils soit le plus coherent par rapport à la RLD Miniriz. 
# Je dois donc faire passer le JDD Miniriz en tant que raster de 2:2cm, puis en calculer la RLD, et comparer aux resultats que Antoine et Gaethan ont obtenus.  

### Calcul du nombre de racines

liste.raster.racines <- setdiff(ls(), c("raster.stack", "liste.test"))

resultat.NB.racines <- list()

for (i in 1:length(liste.raster.racines)) {
  
  resultat.NB.racines[[i]] <- terra::as.data.frame(x = get(liste.raster.racines[i]), xy = T)

  resultat.NB.racines[[i]] <- resultat.NB.racines[[i]] %>%
    select(-x) %>%
    group_by(y) %>%
    summarise(across(everything(), sum))
  # Calcul du nombre de Racines dans chanque tranche de sol mesuree. Donc on somme chaque ligne 
  
  resultat.NB.racines[[i]]$y <- sort(resultat.NB.racines[[i]]$y, decreasing = T)
  
  resultat.NB.racines[[i]] <- resultat.NB.racines[[i]] %>% arrange(y)
  
  names(resultat.NB.racines)[[i]] <- liste.raster.racines[i]

}

### Calcul de la RLD ####

### Mise en forme resultats

resultat.RLD <- resultat.NB.racines

for (i in 1:length(resultat.RLD)) {
  
  resultat.RLD[[i]] <- resultat.RLD[[i]] %>% 
    summarise(across(), resultat.RLD[[i]] * 0.5 / 25) %>% 
    mutate(y = y / 0.5 * 25)
  
}

rm(list = setdiff(ls(), c("resultat.NB.racines", "resultat.RLD")))

### Etude des resultats

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

rm(list = setdiff(ls(), c("liste.res.5", "resultat.NB.racines", "resultat.RLD", "resultat.RLD.etudie")))

### Calcul Etude Resultats

liste.res.5 <- setdiff(liste.res.5, "raster.stack.5.temoin")

liste.etude.resultat <- list()

for (i in 1:length(resultat.RLD.etudie)) {

  for (j in 1:6) {

    RMSE <- rmse(actual = resultat.RLD.etudie[[i]]$raster.stack.5.temoin,
                 predicted = resultat.RLD.etudie[[i]][,j])

    Accuracy <- accuracy(actual = resultat.RLD.etudie[[i]]$raster.stack.5.temoin,
                         predicted = resultat.RLD.etudie[[i]][,j])
    
    temp <- list(RMSE = RMSE, Accuracy = Accuracy)
      
    assign(x = paste0(names(resultat.RLD.etudie[i]), ".", names(resultat.RLD.etudie[[i]])[j]) , value = temp)

   }

}

test <- mget(setdiff(ls(),c("i", "j", "Accuracy", "liste.etude.resultat", 
                            "liste.res.5", "resultat.NB.racines", 
                            "resultat.RLD", "resultat.RLD.etudie", "RMSE", "temp"))
                     )

### Dump ####

# Coder calcul d'indicateur de differences par rapport au Temoin :
#   Deviation
#   Ecart a la moyenne
#   RMSE
#   Efficience
#   ect.

# Extraire 
# Utiliser le package metrics

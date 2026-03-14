library(sf)
library(terra)
library(dplyr)

station <- read_sf("C:/Users/alexi/Downloads/DATA_SEANCE_7/DATA_SEANCE_7/stations_meteorologiques.shp")
altitude_r <- rast("C:/Users/alexi/Downloads/DATA_SEANCE_7/DATA_SEANCE_7/mnt.tif")
lon_r <- rast("C:/Users/alexi/Downloads/DATA_SEANCE_7/DATA_SEANCE_7/longitude.tif")
lat_r <- rast("C:/Users/alexi/Downloads/DATA_SEANCE_7/DATA_SEANCE_7/latitude.tif")
dist_mer_r <- rast("C:/Users/alexi/Downloads/DATA_SEANCE_7/DATA_SEANCE_7/distance_mer.tif")
#Extraction des variables environnementales 

station$altitude <- extract(altitude_r, station)[,2]
station$dist_mer <- extract(dist_mer_r, station)[,2]
station$lon <- extract(lon_r , station)[,2]
station$lat <- extract(lat_r , station)[,2]

station <- na.omit(station)

#Export d'une première couche enrichie
st_write(station,
        "C:/Users/alexi/Downloads/DATA_SEANCE_7/DATA_SEANCE_7/stations_variables_extraites.shp",
        delete_dsn = TRUE  )

#Corrélations entre la température et les variables explicatives 

vars_num <- station %>%
  st_drop_geometry() %>%
  select(Tmin,altitude,dist_mer,lon,lat)

#Matrice de corrélation 
cor_matrix <- cor(vars_num, use = "complete.obs")
cor_matrix         
#Test de corrélation simple avec l'altirude 
cor.test(station$Tmin , station$altitude)

#Visualisation de la relation température / Altitude 

plot(station$altitude,
     station$Tmin,
     xlab = "Altitude",
     ylab = "Température minimale",
     main = "Relation entre altitude et température minimale")
         
#Régression linéaire simple 

model1 <- lm(Tmin ~ altitude, data =  station)
summary(model1)

#Droite de régression 
plot(station$altitude,
     station$Tmin,
     xlab = "Altitude",
     ylab = "Température minimale",
     main = "Régression linéaire simple : Tmin~ altitude")
abline(model1 , col ="red", lwd = 2)
#Calcul des prédictions et des résidus mu modèle 1
#Calcul des prédictions et des résidus du modéle 1

station$pred1 <- predict(model1, station)
station$resid1 <- station$Tmin - station$pred1
# Evaluation du modèle 1 

MAE1 <- mean(abs(station$resid1), na.rm = TRUE)
RMSE1 <- sqrt(mean((station$resid1)^2, na.rm=TRUE))
R2_1 <- summary(model1)$r.squared

scores <- data.frame(
  modele = "Modèle 1 : Tmin ~ altitude",
  variables = "altitude",
  R2 = R2_1,
  MAE = MAE1,
  RMSE = RMSE1
)
scores

#Première cartographie des résidus 
#Bornes communes pour comparer toures les cartes de résidus 
max_abs_resid <- max(
  abs(station$resid1),
  na.rm = TRUE
)
cex1 <- 0.4 + 2 * abs(station$resid1) / max_abs_resid
breaks_resid <- seq(-max_abs_resid , max_abs_resid, length.out = 8)

plot(station["resid1"],
     pal = hcl.colors(7, "Blue-red 3"),
     breaks = breaks_resid <- seq(-max_abs_resid, max_abs_resid, length.out = 8), 
     pch = 16,
     cex = cex1,
     main = "résidus du modèle 1")
#Export des stations avec les résultats du modèle 1

st_write(station,
         "C:/Users/alexi/Downloads/DATA_SEANCE_7/DATA_SEANCE_7/stations_modele1.shp",
         delete_dsn = TRUE)
#Ajout de la distance à la mer 

model2 <- lm(station$Tmin ~ station$altitude + station$dist_mer , data = station)

summary(model2)
#Calcul des prédictions et des résidus du modèle 2 

station$pred2 <- predict(model2 , station)
station$resid2 <- station$Tmin - station$pred2

#évaluation du modèle 2
MAE2 <- mean(abs(station$resid2), na.rm = TRUE)
RMSE2 <- sqrt(mean((station$resid2)^2 , na.rm = TRUE))
R2_2 <- summary(model2)$r.squared


scores
#Export des stations avec les résultats du modèle 2 
st_write(station, 
         "C:/Users/alexi/Downloads/DATA_SEANCE_7/DATA_SEANCE_7/stations_modele2.shp",
         delete_dsn = TRUE)

#Ajout le latitude 
model3 <- lm(Tmin ~ altitude + dist_mer + lat, data = station)
summary(model3)
#Calcul des prédictions et des résidus du modèle 3 

station$pred3 <- predict(model3, station)
station$resid3 <- station$Tmin - station$pred3

#Evaluation du modèle 2
MAE3 <- mean(abs(station$resid3), na.rm = TRUE)
RMSE3 <- sqrt(mean((station$resid3)^2, na.rm=TRUE))
R2_3 <- summary(model3)$r.squared


scores

#Export des stations avec les résultats du modèle 3
st_write(station,
         "C:/Users/alexi/Downloads/DATA_SEANCE_7/DATA_SEANCE_7/stations_modele3.shp",
         delete_dsn =  TRUE)
scores
plot(scores)
#Export du raster final 
library(raster)
#Modèle1
env_stack1 <- c(altitude_r)
temp_model_1 <- predict(env_stack1,model1)
names(env_stack1) <- c("altitude")
writeRaster(temp_model_1,
            "C:/Users/alexi/Downloads/DATA_SEANCE_7/DATA_SEANCE_7/TMIN_modele_1.tif", 
            overwrite = TRUE)
plot(temp_model_1,
     main = "Carte modèle 1")
#Modèle 2
env_stack2 <- c(altitude_r ,dist_mer_r)
names(env_stack2) <- c("altitude", "dist_mer")
temp_model2 <- predict(env_stack2 , model2)
#Modèle 3
env_stack <- c(altitude_r , dist_mer_r,lat_r)
names(env_stack) <- c("altitude","dist_mer","lat")
temp_model3 <- predict(env_stack , model3)
temp_model3 
  
writeRaster(temp_model3,
            "C:/Users/alexi/Downloads/DATA_SEANCE_7/DATA_SEANCE_7/TMIN_modele_3.tif", 
            overwrite = TRUE)

plot(temp_model3 , main = "Carte du modèle 3")
#Export du tableau des scores 
write.csv(scores,
          "C:/Users/alexi/Downloads/DATA_SEANCE_7/DATA_SEANCE_7/scores_modeles_temperature.csv",
          row.names = FALSE)

#séparaiton aléatoire des stations (70/30)
set.seed(123)
id_train <- sample(1:nrow(station), size = 0.7 * nrow(station))
train <- station[id_train, ]
test <- station[-id_train,]

#Cartographie du split
plot(st_geometry(station), col = "grey80", pch = 16 , main = "Répartition des stations")
plot(st_geometry(train), col = "blue", pch = 16 , add = TRUE)  
plot(st_geometry(test), col = "red", pch = 16, add= TRUE)

legend("bottomleft",
       legend = c("Entraînement(70%)","Validation(30%)"),
       col = c("blue", "red"),
       pch = 16)
  

#Prédiction sur les stations de validation 
test$pred <- predict(model3, newdata = test)
test$resid <- test$Tmin - test$pred

#Evaluation du modèle 
MAE <- mean(abs(test$resid))
RMSE <- sqrt(mean(test$resid^2))
R2 <- cor(test$Tmin,test$pred)^2

data.frame(R2, MAE,RMSE)

#Cartographie des résidus de validation
#Bornes communes pour comparer toutes les cartes de résidus 
max_abs_resid <- max(
  abs(test$resid),
  na.rm = TRUE
)
cex1 <- 0.4 + 2* abs(test$resid) / max_abs_resid
breaks_resid <- seq(-max_abs_resid, max_abs_resid,length.out = 8)

plot(test["resid"],
     pal = hcl.colors(7, "Blue-red 3"),
     breaks = breaks_resid <- seq(-max_abs_resid,max_abs_resid, length.out = 8),
     pch = 16,
     cex = cex1,
     main = "Résidus du modèle sur le jeu de validation")

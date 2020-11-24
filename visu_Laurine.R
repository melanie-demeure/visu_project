library(dplyr)
library(ggplot2)
library(sf)
library(tidyverse)
library(dplyr)

setwd("/Users/laurineallard/Documents/Agrocampus Ouest/Visualisation de données/Projet")

#Importation jeux de donn?es (DOM TOM et autres enlev? ? la main sur excel)
puborga<-read.csv("./publicorga.csv", sep=";",dec=".", header=TRUE)
puborga<-as.data.frame(puborga)

#Conversion des variables en facteurs
for (i in 1:12){
  puborga[,i] <- as.factor(puborga[,i])
}

# SUMMARY DES DONNEES 
summary(puborga)

#Fond de carte France par région 2014
reg<-st_read(dsn="/Users/laurineallard/Documents/Agrocampus Ouest/Visualisation de données/Projet/regions-20140306-100m-shp/regions-20140306-100m.shp")
reg$nom

#On enlève les DOM TOM
guadeloupe<-reg[11,]
guyane<-reg[12,]
lareunion<-reg[16,]
martinique<-reg[19,]
mayotte<-reg[20,]

reg<-reg[-c(11:12,16,19:20),]

# Visualisation du fond de carte avec changement du CRS avec UTM31N
reg %>%
  st_transform( crs = 32631 )%>% 
  ggplot() + geom_sf()+
  theme_light()

#Filtrer en fonction de l'annee, du type d'organisme, et du code indicateur
puborga %>%
  filter(annee==2013)%>%
  filter(type_d_organisme=="Entreprises")%>%
  filter(code_indicateur=="dird")%>%
  arrange(code_region)

#On merge les donnees et le fond de carte
test<-merge(x=reg,y=puborga,by.x="code_insee",by.y="code_region",all.x=T)

# graph avec evolution des chercherus h/F dans le temps au niveau national. + 2 dates éloignées, au sein des régions comme ça évolue en termes de proportion (camembert par région)





# Differents types d'administration

# on filtre
# puis on merge sur ce quon a filtré


#Test Graphique 1
test %>% ggplot2::ggplot() +
  geom_sf( aes(fill = valeur) )
test

#Test Graphique 2
test %>%
  ggplot() +
  geom_sf(aes(fill = as.numeric(valeur))) +
  scale_fill_gradient(low = "#75c9c5", high = "#fb1c05") +
  labs(fill = 'valeur')
test


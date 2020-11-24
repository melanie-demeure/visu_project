library(dplyr)
library(ggplot2)
library(sf)
library(tidyverse)



#Importation jeux de donn�es (DOM TOM et autres enlev� � la main sur excel)
pubpriv<-read.csv("C:/Users/home/OneDrive - etu.unistra.fr (1)/M2 Data Sciences/Visualisation Ordi OK/pubpriv.csv", sep=";",dec=".", header=TRUE)
pubpriv<-as.data.frame(pubpriv)

#Conversion des variables en facteurs
pubpriv$�..code_zone_geo <- as.factor(pubpriv$�..code_zone_geo)
pubpriv$zone_geographique <- as.factor(pubpriv$zone_geographique)
pubpriv$code_type_org <- as.factor(pubpriv$code_type_org)
pubpriv$type_d_organisme <- as.factor(pubpriv$type_d_organisme)
pubpriv$code_indicateur <- as.factor(pubpriv$code_indicateur)
pubpriv$indicateur <- as.factor(pubpriv$indicateur)
pubpriv$code_type_pers <- as.factor(pubpriv$code_type_pers)
pubpriv$type_de_personnel <- as.factor(pubpriv$type_de_personnel)
pubpriv$secret <- as.factor(pubpriv$secret)
pubpriv$non_disponible <- as.factor(pubpriv$non_disponible)
pubpriv$etat_des_donnees <- as.factor(pubpriv$etat_des_donnees)


#Filtrer en fonction de l'ann�e, du type d'organisme, et du code indicateur
pubpriv %>%
  filter(annee==2013)%>%
  filter(type_d_organisme=="Entreprises")%>%
  filter(code_indicateur=="dird")%>%
  arrange(�..code_zone_geo)
  

#Fond de carte France par r�gion 2014
reg<-st_read(dsn="C:/Users/home/OneDrive - etu.unistra.fr (1)/M2 Data Sciences/Visualisation Ordi OK/regions-20140306-100m-shp/regions-20140306-100m.shp")
reg$nom
#On enl�ve les DOM TOM
reg<-reg[-(11:12),]
reg<-reg[-(14),]
reg<-reg[-(16:17),]

#V�rification du fond de carte
reg %>%
  ggplot() + geom_sf()

#On merge les donn�es et le fond de carte
test<-merge(x=reg,y=pubpriv,by.x="code_insee",by.y="�..code_zone_geo",all.x=T)


#Test Graphique 1
test %>% ggplot2::ggplot() +
  geom_sf( aes(fill = valeur) )


#Test Graphique 2
test %>%
  ggplot() +
  geom_sf(aes(fill = as.numeric(valeur))) +
  scale_fill_gradient(low = "#75c9c5", high = "#fb1c05") +
  labs(fill = 'valeur')

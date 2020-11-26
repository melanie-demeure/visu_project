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

reg$nom

# Visualisation du fond de carte avec changement du CRS avec UTM31N
reg %>%
  st_transform( crs = 32631 )%>% 
  ggplot() + geom_sf()+
  theme_light()

#On merge les donnees et le fond de carte
test<-merge(x=reg,y=puborga,by.x="code_insee",by.y="code_region",all.x=T)

##################################
# Graphique de l'evolution de l'effectif des types d'administration au niveau national de 2001 à 2013
df1 <- puborga %>%
  filter(indicateur=="Effectifs de R&D")%>%
  filter(code_indicateur=="pers")%>%
  #arrange(type_d_administration) %>%
  group_by(type_d_administration,annee) %>%
  summarise(total_type_admin=sum(valeur))
df1

df1 %>% 
  ggplot(aes(x = annee, y = total_type_admin,group=type_d_administration,color=type_d_administration))+
  geom_line() + theme_light() + xlab("Année") + ylab("Effectif") + ggtitle ("Evolution de l'effectif des types d'administration au niveau national") +
  theme(axis.text.x = element_text(angle=45, vjust = 0.5)) +
  labs(color="Types d'administration") +
  theme(legend.position="bottom") +
  theme(legend.title.align=0.5) +
  guides(colour = guide_colourbar(title.vjust = 0.9)) +
  guides(colour = guide_legend(title.position = "top", nrow = 2)) +
  theme(legend.title = element_text(face="bold")) +
  theme(plot.title = element_text(hjust = 1, vjust = 1.8, face = "bold"))


##################################
# Graphique de l'evolution de des depenses des types d'administration au niveau national de 2001 à 2013
df2 <- puborga %>%
  filter(indicateur=="Depense interieure de R&D")%>%
  filter(code_indicateur=="dird")%>%
  #arrange(type_d_administration) %>%
  group_by(type_d_administration,annee) %>%
  summarise(total_type_admin=sum(valeur))
df2

df2 %>% 
  ggplot(aes(x = annee, y = total_type_admin,group=type_d_administration,color=type_d_administration))+
  geom_line() + theme_light() + xlab("Année") + ylab("Dépenses (millions d'€)") + ggtitle ("Evolution des dépenses des types d'administration au niveau national") +
  theme(axis.text.x = element_text(angle=45, vjust = 0.5)) +
  labs(color="Types d'administration") +
  theme(legend.position="bottom") +
  theme(legend.title.align=0.5) +
  guides(colour = guide_colourbar(title.vjust = 0.9)) +
  guides(colour = guide_legend(title.position = "top", nrow = 2)) +
  theme(legend.title = element_text(face="bold")) +
  theme(plot.title = element_text(size = 12, hjust = 1, vjust = 1.8, face = "bold"))


#2001 ----------------------------------------------------

#Préparation du jeu de données pour 2001
dep_tot<-puborga %>%
  filter(annee==2001) %>%
  filter(indicateur=="Depense interieure de R&D")%>%
  group_by(code_region,region)%>%
  summarise(Depense_Total=sum(valeur))
dep_tot<-as.data.frame(dep_tot)

eff_tot<-puborga %>%
  filter(annee==2001) %>%
  filter(indicateur=="Effectifs de R&D")%>%
  group_by(code_region,region)%>%
  summarise(Effectif_Total=sum(valeur))
eff_tot<-as.data.frame(eff_tot)


dep_eff_tot<-cbind(dep_tot,eff_tot)
dep_eff_tot<-dep_eff_tot[-1,]
dep_eff_tot<-dep_eff_tot[-23,]
dep_eff_tot<-dep_eff_tot[,-(4:5)]

#IMPORTANT jeu de données final 2001
filter_dep_eff_tot_2001<-
  dep_eff_tot%>%
  mutate(Depense_Effectif=Depense_Total/Effectif_Total)


#2013 ----------------------------------------------------

#Préparation du jeu de données pour 2013
dep_tot_2013<-
  rdFR %>%
  filter(annee==2013) %>%
  filter(indicateur=="Depense interieure de R&D")%>%
  group_by(code_region,region)%>%
  summarise(Depense_Total=sum(valeur))
dep_tot_2013<-as.data.frame(dep_tot_2013)
dep_tot_2013<-dep_tot_2013[-(1:6),]
dep_tot_2013<-dep_tot_2013[-23,]

eff_tot_2013<-
  rdFR %>%
  filter(annee==2001) %>%
  filter(indicateur=="Effectifs de R&D")%>%
  group_by(code_region,region)%>%
  summarise(Effectif_Total=sum(valeur))
eff_tot_2013<-as.data.frame(eff_tot_2013)
eff_tot_2013<-eff_tot_2013[-1,]
eff_tot_2013<-eff_tot_2013[-23,]

dep_eff_tot_2013<-cbind(dep_tot_2013,eff_tot_2013)
dep_eff_tot_2013<-dep_eff_tot_2013[,-(4:5)]


#IMPORTANT jeu de données final 2013
filter_dep_eff_tot_2013<-
  dep_eff_tot_2013%>%
  mutate(Depense_Effectif=Depense_Total/Effectif_Total)


#LIBRARY
library(tidyverse)


#IMPORTATION DU JEU DE DONNEES
public_prive <- read.csv("./admin_entrep.csv", sep = ";")

#AS FACTOR
public_prive$code_zone_geo <- as.factor(public_prive$code_zone_geo)
public_prive$zone_geographique <- as.factor(public_prive$zone_geographique)
public_prive$code_type_org <- as.factor(public_prive$code_type_org)
public_prive$type_d_organisme <- as.factor(public_prive$type_d_organisme)
public_prive$code_indicateur <- as.factor(public_prive$code_indicateur)
public_prive$indicateur <- as.factor(public_prive$indicateur)
public_prive$code_type_pers <- as.factor(public_prive$code_type_pers)
public_prive$type_de_personnel <- as.factor(public_prive$type_de_personnel)
public_prive$secret <- as.factor(public_prive$secret)
public_prive$non_disponible <- as.factor(public_prive$non_disponible)
public_prive$etat_des_donnees <- as.factor(public_prive$etat_des_donnees)

# RESUME
summary(public_prive)


data %>%
  arrange(data)
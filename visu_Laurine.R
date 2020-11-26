library(dplyr)
library(ggplot2)
library(sf)
library(tidyverse)
library(dplyr)

setwd("/Users/laurineallard/Documents/Agrocampus Ouest/Visualisation de données/Projet")

#Importation jeux de donn?es (DOM TOM et autres enlev? ? la main sur excel)
rdFR<-read.csv("./publicorga.csv", sep=";",dec=".", header=TRUE)
rdFR<-as.data.frame(rdFR)

#Conversion des variables en facteurs
for (i in 1:12){
  rdFR[,i] <- as.factor(rdFR[,i])
}

# SUMMARY DES DONNEES 
summary(rdFR)

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
test<-merge(x=reg,y=rdFR,by.x="code_insee",by.y="code_region",all.x=T)

##################################
# Graphique de l'evolution de l'effectif des types d'administration au niveau national de 2001 à 2013
df1 <- rdFR %>%
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
df2 <- rdFR %>%
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


# Proportions des types d'administration par région en 2013, à partir des dépenses/effects
#2013 ----------------------------------------------------

#Préparation du jeu de données pour 2013
dep_tot_2013<-
  rdFR %>%
  filter(annee==2013) %>%
  filter(indicateur=="Depense interieure de R&D")%>%
  group_by(code_region,region,type_d_administration)%>%
  summarise(Depense_Total=sum(valeur))

eff_tot_2013<-
  rdFR %>%
  filter(annee==2013) %>%
  filter(indicateur=="Effectifs de R&D")%>%
  group_by(code_region,region,type_d_administration)%>%
  summarise(Effectif_Total=sum(valeur))

dep_eff_tot_2013<-merge(x=dep_tot_2013,y=eff_tot_2013, by = c("type_d_administration", "region", "code_region"))
dep_eff_tot_2013 <- dep_eff_tot_2013[-c(6,9,13,25,26,29,33,34,41,53,54,57,61,62,69,81,82,85,89,96),]

#IMPORTANT jeu de donnees final 2013
filter_dep_eff_tot_2013 <-
  dep_eff_tot_2013 %>%
  mutate(Depense_Effectif=(Depense_Total/Effectif_Total)*10^6) %>% 
  arrange(desc(Depense_Effectif))

# Graphique
filter_dep_eff_tot_2013 %>%
  ggplot(aes(x = region, y = Depense_Effectif, group = type_d_administration, fill = type_d_administration, width = 0.6)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99", "#E69F00")) +
  theme_minimal() +
  coord_flip() + theme_light() + xlab("Régions") +
  theme(legend.position="bottom") +
  guides(fill = guide_legend(title = "Types d'administration", title.position = "top", title.hjust = 0.5, nrow = 2)) +
  theme(legend.title = element_text(face="bold", size = 10)) +
  labs(caption="Source: data.gouv.fr") +
  scale_y_continuous(name="Dépenses par ETP (en euros)",labels = scales::comma) + 
  theme(axis.text.x = element_text(face = "bold"), axis.text.y = element_text(face = "bold"))



##Code Melanie!!!
### Dépenses par ETP au niveau national, en 2013 en fonction des administration
dep_tot<-
  rdFR %>%
  filter(annee==2013) %>%
  filter(code_indicateur=="dird")%>%
  filter(code_region!=7) %>% 
  group_by(type_d_administration) %>% 
  summarise(depense_admin=sum(valeur))

eff_tot<-
  rdFR %>%
  filter(annee==2001) %>%
  filter(code_indicateur=="pers")%>%
  filter(code_region!=7) %>% 
  group_by(type_d_administration) %>% 
  summarise(eff_admin=sum(valeur))

dep_eff_tot<-merge(x=dep_tot,y=eff_tot,by.x = "type_d_administration",by.y = "type_d_administration")

filter_dep_eff_tot_2013<-
  dep_eff_tot%>%
  mutate(dep_eff_admin=depense_admin/eff_admin*10^6) %>% 
  arrange(desc(dep_eff_admin))

filter_dep_eff_tot_2013$type_d_administration<-factor(filter_dep_eff_tot_2013$type_d_administration,levels=filter_dep_eff_tot_2013$type_d_administration[order(filter_dep_eff_tot_2013$dep_eff_admin,decreasing = FALSE)])

#Premier graph
filter_dep_eff_tot_2013 %>% 
  ggplot()+
  geom_bar(aes(x=type_d_administration,y = dep_eff_admin, fill=type_d_administration),stat = "identity")+
  scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99", "#E69F00")) +
  scale_x_discrete(name="")+
  scale_y_continuous(name="Dépenses par ETP (en euros)",labels = scales::comma)+
  theme(axis.text.x = element_text(face="bold",size=14, angle=45))+
  theme_minimal()+
  coord_flip()+
  theme(legend.position = "none") +
  labs(caption="Source: data.gouv.fr") + 
  theme(axis.text.x = element_text(face = "bold"), axis.text.y = element_text(face = "bold"))

#DEuxieme graph (NE FONCTIONNE PAS)
filter_dep_eff_tot_2013 %>%
  ggplot( aes(x=region, y=dep_eff_admin, group=type_d_administration, fill=type_d_administration)) +
  geom_bar(stat="identity") +
  ggtitle("Depenses par ETP") +
  theme(axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"))+
  coord_flip()+
  scale_y_continuous(name="Dépenses par ETP (euros)",labels = scales::comma)


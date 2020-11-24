#LIBRARY
library(tidyverse)


#IMPORTATION DU JEU DE DONNEES
rdFR <- read.csv("./rd-moyens-administrations.csv", sep = ";")

#AS FACTOR
for (i in 1:12){
  rdFR[,i] <- as.factor(rdFR[,i])
}

# RESUME
summary(rdFR)


#Fond de carte France par region 2014
reg<-st_read(dsn="regions-20140306-100m-shp/regions-20140306-100m.shp")

reg$nom
#On enl?ve les DOM TOM
guadeloupe<-reg[11,]
guyane<-reg[12,]
lareunion<-reg[16,]
martinique<-reg[19,]
mayotte<-reg[20,]

reg<-reg[-c(11:12,16,19:20),]

reg %>%
  st_transform( crs = 32631 )%>% 
  ggplot() + geom_sf()+
  theme_light()


# nombre de chercheurs ----------------------------------------------------

#Filtrer en fonction de l'annee, du type d'organisme, et du code indicateur
rdFR %>%
  filter(annee==2013)%>%
  filter(type_d_organisme=="Entreprises")%>%
  filter(code_indicateur=="pers")%>%
  arrange(ï..code_zone_geo)

#On merge les donn?es et le fond de carte
test<-merge(x=reg,y=pubpriv,by.x="code_insee",by.y="ï..code_zone_geo",all.x=T)




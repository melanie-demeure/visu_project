library(sf)
library(tidyverse)

#Importation
rdFR<-read.csv("C:/Users/home/Documents/GitHub/visu_project/rd-moyens-administrations.csv",dec=".",sep=";")


#As factor
for(i in 1:12){
  rdFR[,i]<-as.factor(rdFR[,i])
}

#Fond de carte France Métropolitaine
reg<-st_read(dsn="C:/Users/home/Documents/GitHub/visu_project/regions-20140306-100m-shp/regions-20140306-100m.shp")

#On enleve les DOM TOM
reg<-reg[-(11:12),]
reg<-reg[-(14),]
reg<-reg[-(16:17),]

reg %>%
  st_transform(crs=32631) %>%
  ggplot() + geom_sf()











#EVOLUTION DES DEPENSES
# Valeurs des dépenses 2001 ----------------------------------------------------

#Filtrer en fonction de l'annee 2001 et des dépenses
filter_dep_2001<-
  rdFR %>%
  filter(annee==2001) %>%
  filter (indicateur=="Depense interieure de R&D")%>%
  group_by(code_region,region)%>%
  summarise(Depense_Total=sum(valeur))

#On merge les donnees et le fond de carte
merge_2001<-merge(x=reg,y=filter_dep_2001,by.x="code_insee",by.y="code_region",all.x=T)

#Cartographie des des dépenses intérieures en R&D tout type d'administration confondu en 2001
  merge_2001%>%
  st_transform(crs=32631)%>%
  ggplot() +
  geom_sf(aes(fill = as.numeric(Depense_Total))) +
  scale_fill_gradient(low="#ffffff",high="#048B9A") +
  labs(fill = "Dépenses (millions d'euros)") +
  labs(title = "Dépenses totales par région en R&D en 2001",caption="Source: data.gouv.fr")
  



# Valeurs des dépenses 2013 ----------------------------------------------------

#Filtrer en fonction de l'annee 2013 et des dépenses
filter_dep_2013<-
  rdFR %>%
  filter(annee==2013) %>%
  filter (indicateur=="Depense interieure de R&D")%>%
  group_by(code_region,region)%>%
  summarise(Depense_Total=sum(valeur))

#On merge les donnees et le fond de carte
merge_2013<-merge(x=reg,y=filter_dep_2013,by.x="code_insee",by.y="code_region",all.x=T)

#Cartographie des des dépenses intérieures en R&D tout type d'administration confondu en 2013
merge_2013%>%
  st_transform(crs=32631)%>%
  ggplot() +
  geom_sf(aes(fill = as.numeric(Depense_Total))) +
  scale_fill_gradient(low="#ffffff",high="#048B9A") +
  labs(fill = "Dépenses (millions d'euros)") +
  labs(title = "Dépenses totales par région en R&D en 2013",caption="Source: data.gouv.fr")




#EVOLUTION DU PERSONNEL

# Effectif du personnel en 2001 ----------------------------------------------------

#Filtrer en fonction de l'annee 2001 et des dépenses
filter_pers_2001<-
  rdFR %>%
  filter(annee==2001) %>%
  filter (indicateur=="Effectifs de R&D")%>%
  group_by(code_region,region)%>%
  summarise(Effectif_Total=sum(valeur))

#On merge les donnees et le fond de carte
merge_pers_2001<-merge(x=reg,y=filter_pers_2001,by.x="code_insee",by.y="code_region",all.x=T)


merge_pers_2001%>%
  st_transform(crs=32631)%>%
  ggplot() +
  geom_sf(aes(fill = as.numeric(Effectif_Total))) +
  scale_fill_gradient(low = "#ffffff", high = "#66CC33")  +
  labs(fill = "Effectifs") +
  labs(title = "Effectifs totaux par région en R&D en 2001",caption="Source: data.gouv.fr")



# Effectif du personnel en 2013 ----------------------------------------------------

#Filtrer en fonction de l'annee 2013 et des dépenses
filter_pers_2013<-
  rdFR %>%
  filter(annee==2013) %>%
  filter (indicateur=="Effectifs de R&D")%>%
  group_by(code_region,region)%>%
  summarise(Effectif_Total=sum(valeur))

#On merge les donnees et le fond de carte
merge_pers_2013<-merge(x=reg,y=filter_pers_2013,by.x="code_insee",by.y="code_region",all.x=T)


merge_pers_2013%>%
  st_transform(crs=32631)%>%
  ggplot() +
  geom_sf(aes(fill = as.numeric(Effectif_Total))) +
  scale_fill_gradient(low = "#ffffff", high = "#66CC33")   +
  labs(fill = "Effectifs") +
  labs(title = "Effectifs totaux par région en R&D en 2013",caption="Source: data.gouv.fr")







#Filter Barplot

filter_barplot<-
  rdFR %>%
  filter(!region%in% c("Non ventile","a‰tranger","Regions d'outre-mer","Guadeloupe","Martinique","Guyane","La Reunion","Mayotte"))
  


###DEPENSE BARPLOT######

#Barplot des région en 2001 des dépenses en R&D
filter_barplot_dep_2001<-  
  filter_barplot %>%
  filter(annee==2001)%>%
  filter(indicateur=="Depense interieure de R&D")%>%
  group_by(code_region,region)%>%
  summarise(Depense_Total=sum(valeur))

color3<-c("grey50","grey50","grey50","grey50","grey50","blue","grey50","grey50","grey50","grey50","grey50","blue","grey50","blue","grey50","grey50","grey50","grey50","grey50","grey50","grey50","grey50")


filter_barplot_dep_2001$region<-factor(filter_barplot_dep_2001$region,levels=filter_barplot_dep_2001$region[order(filter_barplot_dep_2001$Depense_Total)])
ggplot(data=filter_barplot_dep_2001,aes(x=region,y=Depense_Total,fill=region))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none")+
  geom_bar(stat="identity",alpha=0.8)+
  geom_text(aes(label=Depense_Total,hjust=-0.6,vjust=0.40))+
  scale_fill_manual(values=color3)+
  scale_y_continuous(breaks=seq(0,6500,1000),limits=c(0,6500))+
  xlab("Régions")+
  ylab("Dépenses (millions d'euros)")+
  labs(title = "Dépenses totales par région en R&D en 2001",caption="Source: data.gouv.fr")+
  coord_flip()


#Barplot des région en 2013 des dépenses en R&D    

filter_barplot_dep_2013<-  
  filter_barplot %>%
  filter(annee==2013)%>%
  filter(indicateur=="Depense interieure de R&D")%>%
  group_by(code_region,region)%>%
  summarise(Depense_Total=sum(valeur))

color4<-c("grey50","grey50","grey50","grey50","grey50","grey50","grey50","blue","grey50","grey50","grey50","grey50","grey50","blue","blue","grey50","grey50","grey50","grey50","grey50","grey50","grey50")


filter_barplot_dep_2013$region<-factor(filter_barplot_dep_2013$region,levels=filter_barplot_dep_2013$region[order(filter_barplot_dep_2013$Depense_Total)])
ggplot(data=filter_barplot_dep_2013,aes(x=region,y=Depense_Total,fill=region))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none")+
  geom_bar(stat="identity",alpha=0.8)+
  geom_text(aes(label=Depense_Total,hjust=-0.2,vjust=0.40))+
  scale_fill_manual(values=color4)+
  scale_y_continuous(breaks=seq(0,6500,1000),limits=c(0,6500))+
  xlab("Régions")+
  ylab("Dépenses (millions d'euros)")+
  labs(title = "Dépenses totales par région en R&D en 2013",caption="Source: data.gouv.fr")+
  coord_flip()






####EFFECTIF BARPLOT########

#Barplot des région en 2001 de l'effectif du personnel
filter_barplot_pers_2001<-
  filter_barplot %>%
  filter(annee==2001)%>%
  filter(indicateur=="Effectifs de R&D")%>%
  group_by(code_region,region)%>%
  summarise(Effectif_Total=sum(valeur))

color<-c("grey50","grey50","grey50","#66CC33","grey50","grey50","#66CC33","grey50","grey50","grey50","grey50","grey50","#66CC33","grey50","grey50","grey50","grey50","grey50","grey50","grey50","grey50","grey50")

filter_barplot_pers_2001$region<-factor(filter_barplot_pers_2001$region,levels=filter_barplot_pers_2001$region[order(filter_barplot_pers_2001$Effectif_Total)])
ggplot(data=filter_barplot_pers_2001,aes(x=region,y=Effectif_Total,fill=region))+
theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none")+
geom_bar(stat="identity",alpha=0.8)+
geom_text(aes(label=Effectif_Total,hjust=-0.2,vjust=0.40))+
scale_fill_manual(values=color)+
scale_y_continuous(breaks=seq(0,60000,10000),limits=c(0,60000))+
labs(title = "Effectifs totales par région en R&D en 2001",caption="Source: data.gouv.fr")+
xlab("Régions")+
ylab("Effectifs")+
coord_flip()



#Barplot des région en 2013 de l'effectif du personnel
filter_barplot_pers_2013<-
    filter_barplot %>%
    filter(annee==2013)%>%
    filter(indicateur=="Effectifs de R&D")%>%
    group_by(code_region,region)%>%
    summarise(Effectif_Total=sum(valeur))

color2<-c("grey50","grey50","grey50","grey50","#66CC33","grey50","grey50","#66CC33","grey50","grey50","grey50","grey50","grey50","#66CC33","grey50","grey50","grey50","grey50","grey50","grey50","grey50","grey50")

filter_barplot_pers_2013$region<-factor(filter_barplot_pers_2013$region,levels=filter_barplot_pers_2013$region[order(filter_barplot_pers_2013$Effectif_Total)])
ggplot(data=filter_barplot_pers_2013,aes(x=region,y=Effectif_Total,fill=region))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none")+
  geom_text(aes(label=Effectif_Total,hjust=-0.05,vjust=0.40))+
  geom_bar(stat="identity",alpha=0.8)+
  scale_fill_manual(values=color2)+
  scale_y_continuous(breaks=seq(0,60000,10000),limits=c(0,60000))+
  xlab("Régions")+
  ylab("Effectifs")+
  labs(title = "Effectifs totales par région en R&D en 2013",caption="Source: data.gouv.fr")+
  coord_flip()
    

filter_dep_eff_2001<-
  rdFR %>%
  filter(annee==2001) %>%
  filter(indicateur=="Effectifs de R&D")%>%
  group_by(code_region,region)%>%
  summarise(Effectif_Total=sum(valeur))%>%
  filter(indicateur=="Depense interieure de R&D")%>%
  summarise(Depense_Total=sum(valeur))%>%
  mutate(Dep_Eff=Effectif_Total/Depense_Total)


filter_eff_2001<-
  rdFR %>%
  filter(code_indicateur=="pers") %>% 
  filter(annee==2001) %>%
  group_by(code_region,region) %>% 
  summarise(Effectif=sum(valeur)) 

filter_dep_2001<-
  rdFR %>%
  filter(code_indicateur=="dird") %>% 
  filter(annee==2001) %>%
  group_by(code_region,region) %>% 
  summarise(Depenses=sum(valeur)) 

filter_dep_eff_2001 <- merge(x=filter_eff_2001,y=filter_dep_2001,by.x="code_region",by.y="code_region",all.x=T)

  
view(filter_dep_eff_2001)

names(filter_dep_eff_2001)
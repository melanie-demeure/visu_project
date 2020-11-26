
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
reg<-reg[-c(11:12,16,19:20),]


#PrÃ©paration du jeu de donnÃ©es pour 2001
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

filter_dep_eff_tot_2013 %>%
  ggplot(aes(x=region, y=dep_eff_admin, group=type_d_administration, fill=type_d_administration)) +
  geom_bar(stat="identity") +
  ggtitle("Depenses par ETP") +
  theme(axis.text.x = element_text(face = "bold"),
          axis.text.y = element_text(face = "bold"))+
  coord_flip()+
  scale_y_continuous(name="Dépenses par ETP (euros)",labels = scales::comma)

filter_dep_eff_tot_2013$type_d_administration<-factor(filter_dep_eff_tot_2013$type_d_administration,levels=filter_dep_eff_tot_2013$type_d_administration[order(filter_dep_eff_tot_2013$dep_eff_admin,decreasing = FALSE)])

require(scales)
filter_dep_eff_tot_2013 %>% 
  ggplot()+
  geom_bar(aes(x=type_d_administration,y = dep_eff_admin, fill=type_d_administration),stat = "identity")+
  scale_x_discrete(name="")+
  scale_y_continuous(name="Dépenses par ETP (euros)",labels = scales::comma)+
  theme(axis.text.x = element_text(face="bold", 
                                   size=14, angle=45))+
  theme_minimal()+
  coord_flip()+
  theme(legend.position = "none")


unique(rdFR$region)

domtom <- c("Guadeloupe","Martinique","Guyane","La Reunion","Mayotte")

admin1 <-  rdFR %>%
  filter(code_indicateur=="pers") %>%
  filter(annee==2013) %>% 
  group_by(code_region,region,type_d_administration) %>% 
  summarise(eff_tot=sum(valeur)) %>% 
  group_by(region) %>% 
  mutate(percentage=eff_tot/sum(eff_tot)*100) 

admin1 <- admin1[-c(1:21,99:101),]
admin1$type_d_administration <- factor(admin1$type_d_administration, levels = c('Autres administrations','Enseignement superieur','EPST','EPIC et assimiles'))
  

admin1 %>% 
  ggplot(aes(x =region, y = percentage))+
  theme(axis.text.y = element_text(face = bold.labels))+
  geom_col(aes(fill = type_d_administration), width = 0.7)+
  scale_fill_discrete(name = "Type d'administration",breaks = rev(levels(admin1$type_d_administration)))+
  # scale_fill_hue()+
  labs(y="Pourcentage de l'effectif",x="Région")+
  scale_x_discrete(labels = c("Alsace",
                              "Aquitaine",
                              "Auvergne",
                              "Basse-Normandie",
                              "Bourgogne",
                              "Bretagne",
                              "Centre",
                              "Champagne-Ardenne",
                              "Corse",
                              "France-Comte",
                              "Haute-Normandie",
                              "Ile-de-France",
                              expression(bold("Langudoc-Roussillon")),
                              "Limousin",
                              "Lorraine",
                              expression(bold("Midi-Pyrenees")),
                              "Nord-Pas-de-Calais",
                              "Pays de la Loire",
                              "Picardie",
                              "Poitou-Charente",
                              "Provence-Alpes-Cote d'Azur",
                              "Rhone-Alpes")) +
  coord_flip()+
  theme_minimal()

view(admin1)  
  
  %>% 
  summarise(personnel=sum(valeur))%>% 
  group_by(annee) %>% 
  mutate(percentage=personnel/sum(personnel)*100) %>% 
  pivot_wider(names_from = sexe, values_from = c(personnel,percentage))





###################" par region
dep_tot<-
  rdFR %>%
  filter(annee==2013) %>%
  filter(code_indicateur=="dird")%>%
  filter(code_region!=7) %>% 
  group_by(region,type_d_administration) %>% 
  summarise(depense_admin=sum(valeur))

eff_tot<-
  rdFR %>%
  filter(annee==2001) %>%
  filter(code_indicateur=="pers")%>%
  filter(code_region!=7) %>% 
  group_by(region,type_d_administration) %>% 
  summarise(eff_admin=sum(valeur))


dep_eff_tot<-merge(x=dep_tot,y=eff_tot,by.x = c("region","type_d_administration"),by.y = c("region","type_d_administration"))

filter_dep_eff_tot_2013<-
  dep_eff_tot%>%
  mutate(dep_eff_admin=depense_admin/eff_admin*10^6) %>% 
  arrange(desc(dep_eff_admin))
view(filter_dep_eff_tot_2013)

filter_dep_eff_tot_2013$type_d_administration<-factor(filter_dep_eff_tot_2013$type_d_administration,levels=filter_dep_eff_tot_2013$type_d_administration[order(filter_dep_eff_tot_2013$dep_eff_admin,decreasing = FALSE)])



require(scales)
filter_dep_eff_tot_2013 %>% 
  ggplot()+
  geom_bar(aes(x=type_d_administration,y = dep_eff_admin, fill=type_d_administration),stat = "identity")+
  scale_x_discrete(name="")+
  scale_y_continuous(name="Dépenses par ETP (euros)",labels = scales::comma)+
  theme(axis.text.x = element_text(face="bold", 
                                   size=14, angle=45))+
  theme_minimal()+
  coord_flip()+
  theme(legend.position = "none")


library(ggplot2)
class(admin1$region)
mtcars$cars <- as.factor(rownames(mtcars))
bold.cars <- c("Merc 280", "Fiat 128")
levels(admin1$region)
bold.labels <- ifelse(levels(mtcars$cars) %in% bold.cars, yes = "bold", no = "plain")

ggplot(mtcars, aes(x = cars, y = drat)) +
  theme(axis.text.y = element_text(face = bold.labels)) +
  geom_col() +
  coord_flip()


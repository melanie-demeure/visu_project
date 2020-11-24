#LIBRARY
library(tidyverse)
library(ggtext)
library(sf)


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


# parite ----------------------------------------------------

#Filtrer en fonction de l'annee, du type d'organisme, et du code indicateur
# par1 <- rdFR %>%
#   filter(code_indicateur=="pers") %>% 
#   filter(annee==2010) %>% 
#   group_by(code_region,region,annee,sexe) %>% 
#   summarise(personnel=sum(valeur)) %>% 
#   pivot_wider(names_from = sexe, values_from = personnel)

par1 <- rdFR %>%
  filter(code_indicateur=="pers") %>% 
  filter(sexe!="Non ventile") %>% 
  filter(annee==2001) %>% 
  group_by(code_region,region,annee,sexe) %>% 
  summarise(personnel=sum(valeur)) %>% 
  group_by(region) %>% 
  mutate(percentage=personnel/sum(personnel)*100) %>% 
  filter(sexe=="Femmes")


#On merge les donn?es et le fond de carte
par11<-merge(x=reg,y=par1,by.x="code_insee",by.y="code_region",all.x=T)
names(par11)

par2001 <- par11 %>%
  st_transform( crs = 32631 )%>% 
  ggplot()+
  geom_sf(aes(fill = as.numeric(percentage))) +
  scale_fill_gradient(low = "#F1D9F7", high = "#FA28FA", breaks=c(30,35,40,45,49)) +
  labs(fill = 'Percentage',title=2001)+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+
  theme(plot.title = element_text(hjust = 0.5,size=10))
par2001

par2 <- rdFR %>%
  filter(code_indicateur=="pers") %>% 
  filter(sexe!="Non ventile") %>% 
  filter(annee==2013) %>% 
  group_by(code_region,region,annee,sexe) %>% 
  summarise(personnel=sum(valeur)) %>% 
  group_by(region) %>% 
  mutate(percentage=personnel/sum(personnel)*100) %>% 
  filter(sexe=="Femmes")


#On merge les donn?es et le fond de carte
par12<-merge(x=reg,y=par2,by.x="code_insee",by.y="code_region",all.x=T)

par2013 <- par12 %>%
  st_transform( crs = 32631 )%>% 
  ggplot()+
  geom_sf(aes(fill = as.numeric(percentage))) +
  scale_fill_gradient(low = "#F1D9F7", high = "#FA28FA", breaks=c(30,35,40,45,49))+
  labs(fill = 'Percentage',title=2013)+
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())+
  theme(plot.title = element_text(hjust = 0.5,size=10))
par2013

vision01_13 <- ggpubr::ggarrange(
  par2001,
  par2013,
  nrow = 1,
  ncol = 2,
  common.legend = TRUE,
  legend = "bottom"
)

annotate_figure(vision01_13,
                top = text_grob("Proportion of women in public research", color = "black", face = "bold", size = 14),
                bottom = text_grob("Data source: data.gouv",
                                   hjust = 1, x = 1, face = "italic", size = 10)
)

par3 <- rdFR %>%
  filter(code_indicateur=="pers") %>% 
  filter(sexe!="Non ventile") %>% 
  group_by(annee,sexe) %>% 
  summarise(personnel=sum(valeur)) %>% 
  group_by(annee) %>% 
  mutate(percentage=personnel/sum(personnel)*100) 

par3 %>% 
  ggplot(aes(x = annee, y = personnel,group=sexe,color=sexe))+
  geom_line()

par3 %>% 
  ggplot() + 
  geom_bar(aes(x = annee, y = percentage,fill=sexe), stat = "identity")


par4 <- rdFR %>%
  filter(code_indicateur=="pers") %>% 
  group_by(annee,sexe) %>% 
  summarise(personnel=sum(valeur)) %>% 
  group_by(annee) %>% 
  mutate(percentage=personnel/sum(personnel)*100) 

par4 %>% 
  ggplot(aes(x = annee, y = personnel,group=sexe,color=sexe))+
  geom_line()

par4 %>% 
  ggplot() + 
  geom_bar(aes(x = annee, y = percentage,fill=sexe), stat = "identity")

g <- ggplot(par4, aes(sexe))

# type d'administration ---------------------------------------------------


#Filtrer en fonction de l'annee, du type d'organisme, et du code indicateur
df <- rdFR %>%
  filter(indicateur=="Effectifs de R&D")%>%
  filter(code_indicateur=="pers")%>%
  #arrange(type_d_administration) %>%
  group_by(type_d_administration,annee) %>%
  summarise(total_type_admin=sum(valeur))
df

# Graphique de l'evolution de l'effectif des types d'administration au niveau national de 2001 à 2013
df %>% 
  ggplot(aes(x = annee, y = total_type_admin,group=type_d_administration,color=type_d_administration))+
  geom_line()

graph1

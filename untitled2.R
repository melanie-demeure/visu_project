Dépenses {data-navmenu="Des disparités régionales"}
=======================================================================
  
  Column 
-----------------------------------------------------------------------
  
  ### 2001
  
  ```{r}
filter_dep_2001<-
  rdFR %>%
  filter(annee==2001) %>%
  filter (indicateur=="Depense interieure de R&D")%>%
  group_by(code_region,region)%>%
  summarise(Depense_Total=round(sum(valeur),2))

#On merge les donnees et le fond de carte
merge_2001<-merge(x=reg,y=filter_dep_2001,by.x="code_insee",by.y="code_region",all.x=T)

#Cartographie des des dépenses intérieures en R&D tout type d'administration confondu en 2001
merge_2001%>%
  st_transform(crs=32631)%>%
  ggplot() +
  geom_sf(aes(fill = as.numeric(Depense_Total))) +
  scale_fill_gradient(low="#ffffff",high="#048B9A",trans="log",breaks=c(50,150,500,1500,3000)) +
  labs(fill = "Dépenses (millions d'euros)") +
  labs(title = "Dépenses totales par régions en R&D",subtitle = "Année : 2001",caption="Source: data.gouv.fr")



```

### 2013

```{r}
#Filtrer en fonction de l'annee 2013 et des dÃƒÂ©penses
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
  scale_fill_gradient(low="#ffffff",high="#048B9A",trans="log",breaks=c(50,150,500,1500,3000)) +
  labs(fill = "Dépenses (millions d'euros)") +
  labs(title = "Dépenses totales par régions en R&D",subtitle = "Année : 2013",caption="Source: data.gouv.fr")


```

Column {data-width=500}
-----------------------------------------------------------------------
  
  ### barplot 2001
  
  ```{r}

filter_barplot_dep_2001 <- filter_dep_2001[-c(1,24),]

color4<-c("grey50","grey50","grey50","grey50","grey50","grey50","grey50","blue","grey50","grey50","grey50","grey50","grey50","blue","blue","grey50","grey50","grey50","grey50","grey50","grey50","grey50")

filter_barplot_dep_2001$region<-factor(filter_barplot_dep_2001$region,levels=filter_barplot_dep_2001$region[order(filter_barplot_dep_2001$Depense_Total)])

ggplot(data=filter_barplot_dep_2001,aes(x=region,y=Depense_Total,fill=region))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none")+
  geom_bar(stat="identity",alpha=0.8)+
  geom_text(aes(label=round(Depense_Total),hjust=-0.2,vjust=0.40))+
  scale_fill_manual(values=color4)+
  scale_y_continuous(breaks=seq(0,6500,1000),limits=c(0,6500))+
  xlab("Régions")+
  ylab("Dépenses (millions d'euros)")+
  labs(title = "Dépenses totales par régions en R&D",subtitle = "Année : 2001",caption="Source: data.gouv.fr")+
  coord_flip()

```


### barplot 2013

```{r}
filter_barplot_dep_2013 <- filter_dep_2013[-c(1:6,29),]

color4<-c("grey50","grey50","grey50","grey50","grey50","grey50","grey50","blue","grey50","grey50","grey50","grey50","grey50","blue","blue","grey50","grey50","grey50","grey50","grey50","grey50","grey50")

filter_barplot_dep_2013$region<-factor(filter_barplot_dep_2013$region,levels=filter_barplot_dep_2013$region[order(filter_barplot_dep_2013$Depense_Total)])

ggplot(data=filter_barplot_dep_2013,aes(x=region,y=Depense_Total,fill=region))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none")+
  geom_bar(stat="identity",alpha=0.8)+
  geom_text(aes(label=round(Depense_Total),hjust=-0.2,vjust=0.40))+
  scale_fill_manual(values=color4)+
  scale_y_continuous(breaks=seq(0,6500,1000),limits=c(0,6500))+
  xlab("Régions")+
  ylab("Dépenses (millions d'euros)")+
  labs(title = "Dépenses totales par régions en R&D",subtitle = "Année : 2013",caption="Source: data.gouv.fr")+
  coord_flip()
```


Effectifs {data-navmenu="Des disparités régionales"}
=======================================================================
  Column {data-width=500}
-----------------------------------------------------------------------
  
  ### 2001
  
  ```{r}
#Filtrer en fonction de l'annee 2001 et des dÃƒÂ©penses
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
  scale_fill_gradient(low = "#ffffff", high = "#66CC33",trans="log",breaks=c(400,1100,3000,8000,22000))  +
  labs(fill = "Effectifs (ETP)") +
  labs(title = "Effectifs totaux par régions en R&D",subtitle = "Année : 2001",caption="Source: data.gouv.fr")

```

### 2013

```{r}
#Filtrer en fonction de l'annee 2013 et des dÃƒÂ©penses
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
  scale_fill_gradient(low = "#ffffff", high = "#66CC33",trans="log",breaks=c(400,1100,3000,8000,22000))   +
  labs(fill = "Effectifs (ETP)") +
  labs(title = "Effectifs totaux par régions en R&D",subtitle = "Année : 2013",caption="Source: data.gouv.fr")


```

Column {data-width=500}
-----------------------------------------------------------------------
  
  ### barplot 2001
  
  ```{r}
filter_barplot_pers_2001 <- filter_pers_2001[-c(1,24),]

color<-c("grey50","grey50","grey50","#66CC33","grey50","grey50","#66CC33","grey50","grey50","grey50","grey50","grey50","#66CC33","grey50","grey50","grey50","grey50","grey50","grey50","grey50","grey50","grey50")

filter_barplot_pers_2001$region<-factor(filter_barplot_pers_2001$region,levels=filter_barplot_pers_2001$region[order(filter_barplot_pers_2001$Effectif_Total)])
ggplot(data=filter_barplot_pers_2001,aes(x=region,y=Effectif_Total,fill=region))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="none")+
  geom_bar(stat="identity",alpha=0.8)+
  geom_text(aes(label=round(Effectif_Total),hjust=-0.2,vjust=0.40))+
  scale_fill_manual(values=color)+
  scale_y_continuous(breaks=seq(0,60000,10000),limits=c(0,60000))+
  labs(title = "Effectif total par région en R&D",subtitle = "Année : 2001",caption="Source: data.gouv.fr")+
  xlab("Régions")+
  ylab("Effectifs (ETP)")+
  coord_flip()

```


### barplot 2013

```{r}
filter_barplot_pers_2013 <- filter_pers_2013[-c(1:6,29),]

color2<-c("grey50","grey50","grey50","grey50","grey50","#66CC33","grey50","grey50","#66CC33","grey50","grey50","grey50","grey50","grey50","#66CC33","grey50","grey50","grey50","grey50","grey50","grey50","grey50","grey50")

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
  ylab("Effectifs (ETP)")+
  labs(title = "Effectif total par région en R&D",subtitle = "Année : 2013",caption="Source: data.gouv.fr")+
  coord_flip()



```
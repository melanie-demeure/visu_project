library(sf)
library(tidyr)
library(dplyr)
library(ggplot2)
library(cowplot)

#Filtrer en fonction de l'annee, du type d'organisme, et du code indicateur
par1 <- rdFR %>%
  filter(code_indicateur=="pers") %>% 
  filter(annee==2010) %>% 
  group_by(code_region,region,annee,sexe) %>% 
  summarise(personnel=sum(valeur)) %>% 
  pivot_wider(names_from = sexe, values_from = personnel)

par1 <- rdFR %>%
  filter(code_indicateur=="pers") %>% 
  filter(annee==2010) %>% 
  group_by(code_region,region,annee,sexe) %>% 
  summarise(personnel=sum(valeur))

#On merge les donn?es et le fond de carte
par11<-merge(x=sf_cent,y=par1,by.x="code_insee",by.y="code_region",all.x=T)
names(par11)

# avoir les centroid des regions
sf_cent <- st_coordinates(st_centroid(reg))
centres <- cbind(code_insee=reg$code_insee,
                 sf_cent <- st_coordinates(st_centroid(reg)))
centres <- as.data.frame(centres)

par11<-merge(x=centres,y=par1,by.x="code_insee",by.y="code_region",all.x=T)
par11$X <- as.numeric(par11$X)
par11$Y <- as.numeric(par11$Y)

ggplot() +
  geom_bar(data = testpar,
           aes(x = "", y = personnel, fill = sexe),
           stat = "identity", width = 1)+
  coord_polar("y")+
  theme_void()+
  theme(legend.position = 0) +
  ggtitle(unique(testpar$region))

make_pie <- function(dt, title = NA, legend.position = 0){
  if(is.na(title)){
    title <- unique(dt$region)
  }
  ggplot() +
    geom_bar(data = dt,
             aes(x = "", y = personnel, fill = sexe),
             stat = "identity", width = 1) +
    coord_polar("y") +
    theme_void() +
    theme(legend.position = legend.position) +
    ggtitle(title)
}

region <- as.character(unique(par11$region))
class(region)
region[1]

make_pie(dplyr::filter(par11, region == 'Auvergne'))

terr1 <- make_pie(dplyr::filter(par11, region == 'Auvergne'))
terr2 <- make_pie(dplyr::filter(par11, region == "Ile-de-France"))
terr3 <- make_pie(dplyr::filter(par11, region == "Champagne-Ardenne"))
terr4 <- make_pie(dplyr::filter(par11, region == "Picardie"))
terr5 <- make_pie(dplyr::filter(par11, region == "Haute-Normandie"))
terr6 <- make_pie(dplyr::filter(par11, region == "Centre"))
terr7 <- make_pie(dplyr::filter(par11, region == "Bourgogne"))
terr8 <- make_pie(dplyr::filter(par11, region == "Lorraine"))
terr9 <- make_pie(dplyr::filter(par11, region == "Franche-Comte"))
terr10 <- make_pie(dplyr::filter(par11, region == "Bretagne"))
terr11 <- make_pie(dplyr::filter(par11, region == "Aquitaine"))
terr12 <- make_pie(dplyr::filter(par11, region == "Limousin"))
terr13 <- make_pie(dplyr::filter(par11, region == "Basse-Normandie"))
terr14 <- make_pie(dplyr::filter(par11, region == "Nord-Pas-de-Calais"))
terr15 <- make_pie(dplyr::filter(par11, region == "Alsace" ))
terr16 <- make_pie(dplyr::filter(par11, region == "Pays de la Loire"))
terr17 <- make_pie(dplyr::filter(par11, region == "Poitou-Charentes"))
terr18 <- make_pie(dplyr::filter(par11, region == "Midi-Pyrenees"))
terr19 <- make_pie(dplyr::filter(par11, region == "Rhone-Alpes"))
terr20 <- make_pie(dplyr::filter(par11, region == "Languedoc-Roussillon"))
terr21 <- make_pie(dplyr::filter(par11, region == "Corse"))
terr22 <- make_pie(dplyr::filter(par11, region == "Provence-Alpes-Cote d'Azur"))

(gg_par_france <- ggplot(data = reg) +
    geom_sf() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0 )) +
    theme(legend.position = 0,
          plot.margin = unit(c(0,0,0,0), "cm"))
)

leg <- get_legend(make_pie(par11, "", legend.position = "left"))

draw_plot_loc <- function(plot, dt){
  draw_plot(plot, x = dt$X[1], y = dt$Y[1],
            height = 0.2)
}

test <- dplyr::filter(par11, region == "Auvergne")
draw_plot(plot, x = test$X[1], y = test$Y[1],
          height = 0.2)

(all <-
    ggdraw(gg_par_france) +
    draw_plot_loc(terr1, dplyr::filter(par11, region == "Auvergne"))

)

cowplot::plot_grid(all, leg, rel_widths = c(1, 0.1))

# avoir les centroid des regions
reg$code_insee
sf_cent1 <- st_centroid(reg)
sf_cent <- st_coordinates(st_centroid(reg))
centres <- cbind(code_insee=reg$code_insee,
                 sf_cent <- st_coordinates(st_centroid(reg)))
centres <- as.data.frame(centres)
plot(centres$X,centres$Y)

par11<-merge(x=centres,y=par1,by.x="code_insee",by.y="code_region",all.x=T)
par11$X <- as.numeric(par11$X)
par11$Y <- as.numeric(par11$Y)

ggplot() + 
  geom_scatterpie(aes(x=X, y=Y), data=par11, cols=c("Femmes", "Hommes")) + 
  coord_fixed()

world <- map_data('france')

p <- ggplot(world, aes(long, lat)) +
  geom_map(map=world,aes(map_id=region), fill=NA, color="black")
p + geom_scatterpie(aes(x=X, y=Y), data=par11, cols=c("Femmes", "Hommes"))

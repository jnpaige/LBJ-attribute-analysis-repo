library(here)
library(readxl)
library(ggplot2)
library(vegan)
library(stringr)
setwd(here::here())
d<-read_xlsx("LBJ attribute data.xlsx", sheet=1)
source("Clean Data.R")
source("Munsell color quantifier.R")
source("Convex hull function.R")
source("Assign_to_component.R")
d<-clean.data(d)


for(i in 1:length(d$Munsell)){
  color<-d$Munsell[i]
  hue<-str_replace(color, "(.)[/](.)", "")
  chroma<-as.numeric(str_match(color, "(.)[/](.)"))[2]
  value<-as.numeric(str_match(color, "(.)[/](.)"))[3]
  output<-munsell.converter(hue, value,chroma)
  d$colorx[i]<-as.numeric(output[1])
  d$colory[i]<-as.numeric(output[2])
  d$colorz[i]<-as.numeric(output[3])
}

d<-assign.component(d)

d$site.component<-paste(d$SITE,d$component)
#Cutting edge differences between sites
# Following Lin e al. 2013. in american antiquity. 
# One proxy we could use is flake area/mass to get at cutting edge
# per unit of volume, since the relationship between platform variables
# and cutting edge for soft hammer percussion is unclear. 

#Let's see if the general dimensions Lin et al. focus on come through in PCA.


test<-d[which(d$TechLength*d$MaxTechWidth*d$MidThickness>0),]

pc <- princomp(~ TechLength + MaxTechWidth + MidThickness+Mass,
               data = test, na.action = na.exclude, cor = TRUE)

summary(pc)
#Looks like component three rlates to bigger area flakes with less mass.

pc$loadings
pc.scores<-as.data.frame(pc$scores)
pc.scores$context<-test$context
pc.scores$component<-test$component
pc.scores$site.component<-test$site.component

#No big differences in component three between sites in general, but there are slight differences in components.

ggplot(data=pc.scores, aes(x=Comp.3,color=site.component, fill=site.component))+
  geom_density()+
  facet_grid(site.component ~.)

ggplot(data=test, aes(x=areabymass,color=site.component, fill=site.component))+
  geom_density() +
  coord_cartesian(xlim = c(0, 750)) +
  facet_grid(site.component ~.)


#317 might have slightly more variability in comp.3

ggplot(data=pc.scores, aes(x=Comp.1,y=Comp.3,color=site.component))+
  geom_point() +
  geom_bag(prop=.9) +
  facet_grid(site.component ~.)



ggplot(data=test, aes(x=areabymass,y=TechLength,color=SITE))+
  geom_point() +
  geom_bag(prop=.9) 



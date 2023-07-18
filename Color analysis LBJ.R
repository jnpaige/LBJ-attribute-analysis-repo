library(here)
library(readxl)
library(ggplot2)
library(vegan)
library(stringr)
library(plotly)
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

#d2 includes comparative data
d2<-read_xlsx("raw material survey data.xlsx", sheet=4)


names(d2)[1]<-"SITE"

temp<-d[grep("SITE|Munsell", names(d))]
temp2<-d2[grep("SITE|Munsell", names(d2))]
d2<-rbind(temp, temp2)

for(i in 1:length(d2$Munsell)){
  color<-d2$Munsell[i]
  hue<-str_replace(color, "(.)[/](.)", "")
  chroma<-as.numeric(str_match(color, "(.)[/](.)"))[2]
  value<-as.numeric(str_match(color, "(.)[/](.)"))[3]
  output<-munsell.converter(hue, value,chroma)
  d2$colorx[i]<-as.numeric(output[1])
  d2$colory[i]<-as.numeric(output[2])
  d2$colorz[i]<-as.numeric(output[3])
}

d<-d[which(d$colorz>0),]
d2<-d2[which(d2$colorz>0),]

jitterfy<-function(x){
  return(rnorm(1,x, .1))
}


for(i in 1:length(d$colorx)){d$jitterx[i]<-jitterfy(d$colorx[i])}
for(i in 1:length(d$colory)){d$jittery[i]<-jitterfy(d$colory[i])}
for(i in 1:length(d$colorz)){d$jitterz[i]<-jitterfy(d$colorz[i])}

for(i in 1:length(d2$colorx)){d2$jitterx[i]<-jitterfy(d2$colorx[i])}
for(i in 1:length(d2$colory)){d2$jittery[i]<-jitterfy(d2$colory[i])}
for(i in 1:length(d2$colorz)){d2$jitterz[i]<-jitterfy(d2$colorz[i])}



p <- plot_ly(data=d2) %>%
  add_trace(x=jitterx, y=jittery, z=jitterz,
            color=SITE,
            type="scatter3d", mode="markers",
            colors = "Set1",
            hoverinfo = "text",
            jitter = 0.7,
            marker = list(opacity = .5, size=4))

p


test<-d[which(d$colorz>0),]
pc <- princomp(~ colorx + colory + colorz,
                  data = test, na.action = na.exclude, cor = TRUE)
scores<-as.data.frame(pc$scores)

d$Comp.1[which(d$colorz>0)]<-scores$Comp.1
d$Comp.2[which(d$colorz>0)]<-scores$Comp.2
d$Comp.3[which(d$colorz>0)]<-scores$Comp.3
pc$loadings
ggplot(data=d, aes(x=Comp.1, y=Comp.2, color=site.component, fill=site.component)) +
  geom_jitter(width=.1, height=.1,
              alpha=.5, shape=21,size=2,
              color="black") +
  geom_bag(prop=.9) + facet_grid(SITE ~.)



p <- plot_ly(data=d) %>%
  add_trace(x=d$Comp.1, y=d$Comp.2,
            type="scatter", mode="markers",
            hover="text", color=d$EvidencePostDepBurning,
            hovertext=paste(d$Munsell,d$EvidencePostDepBurning),
            marker = list(opacity = .5, size=4))


#Let's try just the Chroma /sheet and value (z). The pca
#doesn't seem to capture colo simil all that well. 


ggplot(data=d2, aes(x=colorx, y=colorz, fill=SITE)) +
  geom_jitter(width=.1, height=.1,
              alpha=.8, shape=21,size=2,
              color="black") + 
  geom_bag(prop=.9)

ggplot(data=d, aes(x=colorx, y=colorz, fill=site.component)) +
  geom_jitter(width=.1, height=.1,
              alpha=.8, shape=21,size=2,
              color="black") + 
  geom_bag(prop=.9) +  facet_grid(SITE ~.)

ggplot(data=d, aes(x=colorx, y=colorz, fill=PATINATION)) +
  geom_jitter(width=.1, height=.1,
              alpha=.8, shape=21,size=2,
              color="black") + 
  geom_bag(prop=.9) +  facet_grid(SITE ~.)

ggplot(data=d, aes(x=colorx, y=colorz, fill=EvidencePostDepBurning)) +
  geom_jitter(width=.1, height=.1,
              alpha=.8, shape=21,size=2,
              color="black") + 
  geom_bag(prop=.9) +  facet_grid(SITE ~.)



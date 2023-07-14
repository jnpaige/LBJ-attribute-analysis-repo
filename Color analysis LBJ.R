library(here)
library(readxl)
library(stringr)
library(dplyr)
library(plyr)
library(plotly)
library(vegan)

#Test the function. 
#Load function.

source("Munsell color quantifier.R")


#Read in data
d<-read_xlsx("LBJ attribute data.xlsx")
d2<-read_xlsx("BX232 artifact colors.xlsx")

# Add bx232 data here
#d<-d[grep("SITE|Munsell|Mass", names(d))]
#d2<-d2[grep("SITE|Munsell|Mass", names(d2))]
#d<-rbind(d, d2)

# Test the function
color<-paste("7.5yr7/2")
hue<-str_replace(color, "(.)[/](.)", "")
chroma<-as.numeric(str_match(color, "(.)[/](.)"))[2]
value<-as.numeric(str_match(color, "(.)[/](.)"))[3]
munsell.converter(hue,value, chroma)


## Apply function to every artifact in the dataset. 
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

jitterfy<-function(x){
  return(rnorm(1,x, .1))
}


for(i in 1:length(d$colorx)){d$jitterx[i]<-jitterfy(d$colorx[i])}
for(i in 1:length(d$colory)){d$jittery[i]<-jitterfy(d$colory[i])}
for(i in 1:length(d$colorz)){d$jitterz[i]<-jitterfy(d$colorz[i])}



p <- plot_ly(data=d) %>%
  # the scatter plot of the data points 
  add_trace(x=d$jitterx, y=d$jittery, z=d$jitterz,
            color=d$LithicArtifactClass,
            type="scatter3d", mode="markers",
            colors = "Set1",
            hoverinfo = "text",
            hovertext=paste(d$Munsell, d$LithicArtifactClass),
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
ggplot(data=d) +
  geom_jitter(aes(x=Comp.1, y=Comp.2, fill=d$EvidencePostDepBurning), 
              width=.1, height=.1,
              alpha=.5, shape=21,size=2,
              color="black")



p <- plot_ly(data=d) %>%
  add_trace(x=d$Comp.1, y=d$Comp.2,
            type="scatter", mode="markers",
            hover="text", color=d$EvidencePostDepBurning,
            hovertext=paste(d$Munsell,d$EvidencePostDepBurning),
            marker = list(opacity = .5, size=4))


#Let's try just the Chroma /sheet and value (z). The pca
#doesn't seem to capture colo simil all that well. 


find_hull <- function(x) x[chull(x$colorx, x$colorz), ]
hulls <- ddply(d, "EvidencePostDepBurning", find_hull)

ggplot(data=d) +
  geom_jitter(aes(x=colorx, y=colorz, fill=EvidencePostDepBurning), 
              width=.1, height=.1,
              alpha=.8, shape=21,size=2,
              color="black") + 
  geom_polygon(data = hulls, aes(x=colorx, y=colorz, fill=EvidencePostDepBurning), alpha = 0.5)



library(here)
library(readxl)
library(ggplot2)
library(vegan)
library(stringr)
setwd(here::here())
d<-read_xlsx("LBJ attribute data.xlsx", sheet=1)
source("Clean Data.R")
source("Munsell color quantifier.R")
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


unique(d$Provenience)
GL317<-d[which(d$SITE=="41GL317"),]

title<-paste("GL317", "Weights recorded in levels", collapse=" ")
pdf(paste(title,".pdf",collapse=""), width=8, height =7)
ggplot(data=GL317, aes(x=Mass,y=lvl)) +
  stat_summary(fun = sum, na.rm = TRUE, color = 'black', geom ='line', size=1.2) +
  stat_summary(fun = sum, na.rm = TRUE, color = 'black', geom ='point', size=2) +
  scale_y_discrete(limits = rev(levels(data$lvl)))+ facet_wrap(~context) 
dev.off()



notGL317<-data[which(data$SITE!="41GL317"),]
title<-paste("NonGL317", "Weights recorded in levels", collapse=" ")
pdf(paste(title,".pdf",collapse=""), width=8, height =7)
ggplot(data=notGL317, aes(x=Mass,y=lvl)) +
  stat_summary(fun = sum, na.rm = TRUE, color = 'black', geom ='line', size=1.2) +
  stat_summary(fun = sum, na.rm = TRUE, color = 'black', geom ='point', size=2) +
  scale_y_discrete(limits = rev(levels(data$lvl)))+ facet_wrap(~context) 

dev.off()





GL317east<-GL317[which(d$Provenience %in% c(1,2,3,4,5)),]
length(GL31)





library(here)
library(readxl)
library(ggplot2)
library(vegan)
setwd(here::here())
d<-read_xlsx("LBJ attribute data.xlsx", sheet=1)

# Setting up appropriate factor level orders, making sure things are numeric, etc.

head(d)
pattern<-"EPA|MaxLength|MaxWidth|MaxThickness|TechLength|MaxTechWidth|MidThickness|PlatformWidth|PlatformThickness"
d[,grep(pattern,names(d))]

d$EPA<-as.numeric(d$EPA)
depths<-as.character(unique(sort(d$DepthStartcmbd, decreasing=FALSE)))

d$DepthStartcmbd<-as.character(d$DepthStartcmbd)
d$DepthStartcmbd<-factor(d$DepthStartcmbd, 
                     levels = c(depths))

d$CortexArea<-factor(d$CortexArea, 
                         levels = c("0%","1-24%","25-49%","50-74%","75-99%","100%"))

d$context<-paste(d$SITE,d$Provenience,sep=" ")

grp.list<-list(c(seq(0,9,by=1)),
               c(seq(10,19,by=1)),
               c(seq(20,29,by=1)),
               c(seq(30,39,by=1)),
               c(seq(40,49,by=1)),
               c(seq(50,59,by=1)),
               c(seq(60,69,by=1)),
               c(seq(70,79,by=1)),
               c(seq(80,89,by=1)),
               c(seq(90,99,by=1)),
               c(seq(100,109,by=1)),
               c(seq(110,119,by=1)),
               c(seq(120,129,by=1)),
               c(seq(130,139,by=1)),
               c(seq(140,149,by=1)),
               c(seq(150,159,by=1)))

grp.labs<-c("10-20",
            "10-20",
            "20-30",
            "30-40",
            "40-50",
            "50-60",
            "60-70",
            "70-80",
            "80-90",
            "90-100",
            "100-110",
            "110-120",
            "120-130",
            "130-140",
            "140-150",
            "150+")


d$lvl<-NA
data<-d
for(i in 1:length(grp.list)){
  data$lvl[which(data$DepthStartcmbd %in% grp.list[[i]])]<-grp.labs[i]
}
data$lvl<-as.factor(data$lvl)
levels(data$lvl)<-grp.labs


diversity(table(d$Munsell), "shannon")
diversity(table(d$Munsell), "simpson")
diversity(table(d$Munsell), "invsimpson")

GL317<-data[which(data$SITE=="41GL317"),]
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


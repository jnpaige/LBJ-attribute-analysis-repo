library(here)
library(readxl)
library(ggplot2)
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


ggplot(data=d) +
  geom_bar(aes(x=DepthStartcmbd, fill=EvidencePostDepBurning))

ggplot(data=d) +
  geom_bar(aes(x=DepthStartcmbd, fill=CortexArea))

ggplot(data=d) +
  geom_bar(aes(x=DepthStartcmbd, fill=PATINATION))

ggplot(data=d)+
  geom_point(aes(x=DepthStartcmbd, y=TechLength, color=CortexArea))


## 
ggplot(data=d)+
  geom_point(aes(x=TechLength, y=MaxTechWidth))

ggplot(data=d)+
  geom_point(aes(x=EPA, y=TechLength))

ggplot(data=d)+
  geom_point(aes(x=PlatformWidth*PlatformThickness, y=TechLength))


ggplot(data=d)+
  geom_boxplot(aes(x=BULB, y=MidThickness))

ggplot(data=d)+
  geom_boxplot(aes(x=LIP, y=(TechLength*MaxTechWidth)/Mass))
max(d$Mass)

## Troubleshooting function

get.problem<-function(d,x){
  d
  
}

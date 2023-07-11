library(here)
library(readxl)
library(stringr)
library(plotly)
library(vegan)
#R color convertr. 
#Based on Ruck and Brown 2015. in JAS Reports
#5R is the origin for hue pages. 9 degree intervals for each hue


v1<-c("5r",
      "7.5r",
      "10r",
      "2.5yr",
      "5yr",
      "7.5yr",
      "10yr",
      "2.5y",
      "5y",
      "7.5y",
      "10y",
      "2.5gy",
      "5gy",
      "7.5gy",
      "10gy",
      "2.5g",
      "5g",
      "7.5g",
      "10g",
      "2.5bg",
      "5bg",
      "7.5bg",
      "10bg",
      "2.5b",
      "5b",
      "7.5b",
      "10b",
      "2.5pb",
      "5pb",
      "7.5pb",
      "10pb",
      "2.5p",
      "5p",
      "7.5p",
      "10p",
      "2.5rp",
      "5rp",
      "7.5rp",
      "10rp",
      "2.5r")

# Inrcrements of 8 degrees from 0 degrees at 5R. 
# converted to radians.

v2<-c(0.000000000,
0.157079633,
0.314159265,
0.471238898,
0.628318531,
0.785398163,
0.942477796,
1.099557429,
1.256637061,
1.413716694,
1.570796327,
1.727875959,
1.884955592,
2.042035225,
2.199114858,
2.356194490,
2.513274123,
2.670353756,
2.827433388,
2.984513021,
3.141592654,
3.298672286,
3.455751919,
3.612831552,
3.769911184,
3.926990817,
4.084070450,
4.241150082,
4.398229715,
4.555309348,
4.712388980,
4.869468613,
5.026548246,
5.183627878,
5.340707511,
5.497787144,
5.654866776,
5.811946409,
5.969026042,
6.126105675)

hue<-"10YR"
v2[which(v1==hue)]

munsell.converter<-function(hue, chroma, value){
  hue.angle<-v2[which(v1==hue)]
  coordinate<-c(sin(hue.angle)*chroma, cos(hue.angle)*chroma,value)
return(coordinate)
  }

#Test the function. 
munsell.converter("10YR",3,4)

#Read in data
d<-read_xlsx("LBJ attribute data.xlsx")

## break up string into hue, chroma and value

color<-paste("7.5yr7/2")

hue<-str_replace(color, "(.)[/](.)", "")
chroma<-as.numeric(str_match(color, "(.)[/](.)"))[2]
value<-as.numeric(str_match(color, "(.)[/](.)"))[3]

munsell.converter(hue,value, chroma)

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


p <- plot_ly(data=d) %>%
  # the scatter plot of the data points 
  add_trace(x=d$colorx, y=d$colory, z=d$colorz,
            color=d$EvidencePostDepBurning,
            type="scatter3d", mode="markers",
            colors = "Set1",
            hoverinfo = "text",
            hovertext=paste(d$Munsell, d$EvidencePostDepBurning),
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

ggplot(data=d) +
  geom_jitter(aes(x=colorx, y=colorz, fill=d$EvidencePostDepBurning), 
              width=.1, height=.1,
              alpha=.8, shape=21,size=2,
              color="black")

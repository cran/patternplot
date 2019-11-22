library(patternplot)
library(png)
library(ggplot2)
group1<-c('New_England', 'Great_Lakes','Plains',  'Rocky_Mountain', 
          'Far_West','Southwest', 'Southeast',  'Mideast')
pct1<-c( 12, 11, 17, 15, 8, 11,  16,  10)
label1<-paste(group1, " \n ", pct1, "%", sep="")

pattern.type1<-c("hdashes", "blank", "grid", "blank", "hlines", 
                 "blank", "waves", "blank")
pattern.type.inner<-"blank"
pattern.color1<-rep("white", 8)
background.color1<-c("darkgreen", "darkcyan", "chocolate", "cadetblue1", 
                     "darkorchid", "yellowgreen", "hotpink", "lightslateblue")
density1<-rep(12, length(group1))
pattern.line.size1=c(12, 1, 10, 1, 20, 1, 12, 1)

g<-patternring1(group1, pct1, label1, label.size1=4,label.color1='black', 
label.distance1=1.45, pattern.type1, pattern.color1, pattern.line.size1,
background.color1, frame.color='black',frame.size=1.2, density1, pixel=18,
pattern.type.inner="blank",pattern.color.inner="white", pattern.line.size.inner=1,
background.color.inner="white", pixel.inner=10, density.inner=1, r1=3, r2=4)
g<-g+annotate(geom="text", x=0, y=0,label="2019 Number of Cases \n N=1000",color="black",
size=4)+scale_x_continuous(limits=c(-7, 7))+scale_y_continuous(limits=c(-7, 7))
g


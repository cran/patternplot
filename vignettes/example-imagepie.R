library(patternplot)

#Fill slices with png images.
library(png)
Lines <-  readPNG(system.file("img", "lines.png", package="patternplot"))
Chessboard <-  readPNG(system.file("img", "chessboard.png", package="patternplot"))
Dots <-  readPNG(system.file("img", "dots.png", package="patternplot"))

pattern.type<-list(Chessboard, Lines,Dots)
group<-c('chessboard','Lines',  'Dots')
pct<-c(25, 40,  35)
label<-c("Chessboard 25\% ", "Lines 40\%",  "Dots 35\%")

p<-imagepie(group=group,pct=pct,label=label,pattern.type=pattern.type,label.distance=1.26, 
              frame.color='black')
p

#Fill slices with jpeg images.
library(jpeg)
Tomatoes <-  readJPEG(system.file("img", "tomatoes.jpg", package="patternplot"))
Peas <- readJPEG(system.file("img", "peas.jpg", package="patternplot"))
Peppers <-  readJPEG(system.file("img", "peppers.jpg", package="patternplot"))

pattern.type<-list(Tomatoes,Peas,Peppers)
group<-c('Tomatoes', 'Peas', 'Peppers')
pct<-c(40, 25, 35)
label<-c("Tomatoes \n 40\%", "Peas \n 25\% ", "Peppers \n 35\%")

p<-imagepie(group=group,pct=pct,label=label,pattern.type=pattern.type,label.distance=1.25, 
              frame.color='burlywood4', frame.size=0.8, label.size=6, label.color='forestgreen')
p


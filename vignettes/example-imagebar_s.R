library(patternplot)
library(jpeg)
library(ggplot2)

childcare<-readJPEG(system.file("img", "childcare.jpg", package="patternplot"))
food<-readJPEG(system.file("img", "food.jpg", package="patternplot"))
housing <-readJPEG(system.file("img", "housing.jpg", package="patternplot"))

data <- read.csv(system.file("extdata", "monthlyexp.csv", package="patternplot"))
x<-data$Location
y<-data$Amount
group<-data$Type
pattern.type<-list(childcare, food, housing)

imagebar_s(data,x,y,group,xlab='',ylab='Monthly Expenses, Dollar',pattern.type=pattern.type,
label.size=3.5,frame.size=1,frame.color='black',legend.type='h',legend.h=6,legend.y.pos=0.49,
legend.pixel=20, legend.w=0.2,legend.x.pos=1.1,
legend.label=c("Childcare", "Food", "Housing"))+scale_y_continuous(limits = c(0, 6800))




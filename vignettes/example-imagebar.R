library(patternplot)
library(jpeg)
library(ggplot2)

childcare<-readJPEG(system.file("img", "childcare.jpg", package="patternplot"))
food<-readJPEG(system.file("img", "food.jpg", package="patternplot"))
housing <-readJPEG(system.file("img", "housing.jpg", package="patternplot"))

#Example 1
data <- read.csv(system.file("extdata", "monthlyexp.csv", package="patternplot"))
data<-data[which(data$Location=='City 1'),]
x<-factor(data$Type, c('Housing', 'Food',  'Childcare'))
y<-data$Amount
pattern.type<-list(housing, food, childcare)
imagebar(data,x, y,group=NULL,pattern.type=pattern.type,vjust=-1, hjust=0.5,
frame.color='black',ylab='Monthly Expenses, Dollars')+ggtitle('(A) Bar Chart with Images')

#Example 2
data <- read.csv(system.file("extdata", "monthlyexp.csv", package="patternplot"))
group<-factor(data$Type, c('Housing', 'Food',  'Childcare'))
y<-data$Amount
x<-factor(data$Location, c('City 1', ' City 1'))
pattern.type<-list(housing, food, childcare)
g<-imagebar(data,x,y,group,pattern.type=pattern.type,vjust=-1,hjust=0.5,frame.color='black',
ylab='Monthly Expenses, Dollars')+ggtitle('(B) Image Bar Chart with Two Grouping Variables')
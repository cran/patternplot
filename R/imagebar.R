#' Plot a bar chart with bars filled with png and jpeg images.
#'
#' The \code{imagebar} function is a tool for creating versatile bar charts by filling the bars with external png and jpeg images. 
#' @importFrom Rcpp evalCpp
#' @useDynLib patternplot 
#' @param data the data to be used. 
#' @param x the variable used on x axis.
#' @param y the variable used on y axis.
#' @param group the variable used as the second grouping variable on x axis.
#' @param xlab a character string to give x axis label.
#' @param ylab a character string to give y axis label.
#' @param label.size the font size of labels shown above the bars.   
#' @param vjust the distance of labels from the top border of each bar. 
#' @param pattern.type a list of objects returned by \code{readPNG} and \code{readJPEG} used to fill the bars. 
#' @param frame.color the color of the borders of bars.
#' @param frame.size a numeric value, the line size for the borders of bars.
#' @param legend.type if legend.type='h', the layout of legends is horizontal;
#'  if legend.type='v', the layout of legends is vertical. 
#' @param legend.h a numeric value to change the height of legend boxes.
#' @param legend.y.pos a numeric value to change the position of legends on y axis.
#' @param legend.pixel a numeric value to change the pixel of legends. 
#' @return  A ggplot object.
#'
#' @details \code{imagebar} function offers flexible ways in doing bar charts.
#'   
#' @author Chunqiao Luo (chunqiaoluo@gmail.com)
#'
#' @seealso Function \code{patternbar}  
#'
#' @example vignettes/example-imagebar.R

imagebar<-function(data,x, y, group=NULL, xlab='', ylab='', label.size=3.5, vjust=-1, 
                   pattern.type, frame.color='black',frame.size=1,
                   legend.type='h', legend.h=6, legend.y.pos=0.5, legend.pixel=5){

  if(is.null(group)){
    
    bplot <- ggplot(data, aes(x, y)) + geom_bar(stat="identity")+geom_text(aes(label=y)) 
    
    #Box Data
    gdata<-ggplot_build(bplot)$data[[1]]
    gdata<-gdata[order(gdata$group),]
    boxmatrix<-list()
    picdata<-list()
    picdf<-list()
    xmax<-max(gdata[, c('xmax')])
    xmin<-min(gdata[, c('xmin')])
    ymax<-max(gdata[, c('ymax')])
    ymin<-min(gdata[, c('ymin')])
    for (i in 1:dim(gdata)[1]){
      boxmatrix[[i]]<-matrix(c(gdata[i,"xmin"], 0,
                               gdata[i,"xmax"], 0,
                               gdata[i,"xmax"], gdata[i,"ymax"],
                               gdata[i,"xmin"], gdata[i,"ymax"],
                               gdata[i,"xmin"], 0),
                             nrow=5, 
                             ncol=2, byrow=T)
      
      pattern<-pattern.type[[i]]
      picdata[[i]]<-imagetodf2(pattern,  boxmatrix[[i]],left =xmin, right = xmax ,bottom = ymin,top =ymax)
      picdata[[i]] <- filter(picdata[[i]], pos==1)
    }
    
    
    #Text Data
    ldata<-ggplot_build(bplot)$data[[2]]
    
    g<- ggplot()+ mapply(function(i) geom_raster(data = picdata[[i]], aes(x = X, y = Y, fill = rgb(r,g, b,a))),1:dim(gdata)[1])+scale_fill_identity()+geom_rect(aes(xmin=gdata[,"xmin"],xmax=gdata[,"xmax"],ymin=0,ymax=gdata[,"ymax"]), color=frame.color,size=frame.size, fill=NA)
    g+ theme_bw()+xlab(xlab)+ylab(ylab)+ scale_x_continuous(breaks=seq(1:length(levels(x))), labels=levels(x))+geom_text(data=ldata, aes(x, y, label=label), vjust=vjust, size=label.size)
    
  }else{
    
    bplot <- ggplot(data, aes(x, y, fill=group)) + geom_bar(stat="identity", position=position_dodge()) +geom_text(aes(label=y),position=position_dodge(width=0.9)) 
    gdata<-ggplot_build(bplot)$data[[1]]
    gdata<-gdata[order(gdata$group),]
    boxmatrix<-list()
    picdata<-list()
    picdf<-list()
    xmax<-max(gdata[, c('xmax')])
    xmin<-min(gdata[, c('xmin')])
    ymax<-max(gdata[, c('ymax')])
    ymin<-min(gdata[, c('ymin')])
    pattern.type2<-rep(pattern.type, time=length(unique(x)))
    for (i in 1:dim(gdata)[1]){
      boxmatrix[[i]]<-matrix(c(gdata[i,"xmin"], 0,
                               gdata[i,"xmax"], 0,
                               gdata[i,"xmax"], gdata[i,"ymax"],
                               gdata[i,"xmin"], gdata[i,"ymax"],
                               gdata[i,"xmin"], 0),
                             nrow=5, 
                             ncol=2, byrow=T)
      pattern<-pattern.type2[[i]]
      picdata[[i]]<-imagetodf2(pattern,  boxmatrix[[i]],left =xmin, right = xmax ,bottom = ymin,top =ymax)
      picdata[[i]] <- filter(picdata[[i]], pos==1)
    }
    
    legendbox<-list()
    legenddata<-list()
    
    if(legend.type=='v'){
      legend.y<-seq(from =ymax+(0.1+0.05*legend.h)*ymax, to = ymax+0.1*ymax, length.out=length(pattern.type)+1)
      legend.x<-seq(from = xmin, to = xmin+ 0.5*(gdata[1,"xmax"]-xmin), length.out=2)
      legend.frame.xmin<-vector(mode="numeric", length=length(pattern.type))
      legend.frame.xmax<-vector(mode="numeric", length=length(pattern.type))
      legend.frame.ymin<-vector(mode="numeric", length=length(pattern.type))
      legend.frame.ymax<-vector(mode="numeric", length=length(pattern.type))
      for (i in 1:length(pattern.type)){
        legendbox[[i]]<-matrix(c(legend.x[1], legend.y[i],
                                 legend.x[2], legend.y[i],
                                 legend.x[2],legend.y[i+1],
                                 legend.x[1],legend.y[i+1],
                                 legend.x[1],legend.y[i]),
                               nrow=5, 
                               ncol=2, byrow=T)
        pattern<-pattern.type[[i]]
        pattern = pattern[seq(1, nrow(pattern), legend.pixel), seq(1, ncol(pattern), legend.pixel), ]
        legenddata[[i]]<-imagetodf2(pattern,  legendbox[[i]],left =legendbox[[i]][1, 1], right = legendbox[[i]][2, 1] ,bottom = legendbox[[i]][1, 2],top =legendbox[[i]][3, 2])
        legend.frame.xmin[i]<-legend.x[1]
        legend.frame.xmax[i]<-legend.x[2]
        legend.frame.ymax[i]<-legend.y[i]
        legend.frame.ymin[i]<-legend.y[i+1]
      }
      legend.label.y<-legend.y.pos*(legend.frame.ymin+legend.frame.ymax)
      legend.label.x<-legend.frame.xmax+0.2*(gdata[1,"xmax"]-xmin)
      
    }
    
    if(legend.type=='h'){
      legend.y<-c(ymax+0.1*ymax, ymax+(0.1+0.01*legend.h)*ymax)
      unit.x<-((xmax-xmin)-(gdata[1,"xmax"]-xmin)*length(pattern.type))/length(pattern.type)
      legend.frame.xmin<-vector(mode="numeric", length=length(pattern.type))
      legend.frame.xmax<-vector(mode="numeric", length=length(pattern.type))
      legend.frame.ymin<-vector(mode="numeric", length=length(pattern.type))
      legend.frame.ymax<-vector(mode="numeric", length=length(pattern.type))
      for (i in 1:length(pattern.type)){
        legendbox[[i]]<-matrix(c(xmin+(i-1)*(gdata[1,"xmax"]-xmin+unit.x), legend.y[1],
                                 gdata[1,"xmax"]+(i-1)*(gdata[1,"xmax"]-xmin+unit.x), legend.y[1],
                                 gdata[1,"xmax"]+(i-1)*(gdata[1,"xmax"]-xmin+unit.x),legend.y[2],
                                 xmin+(i-1)*(gdata[1,"xmax"]-xmin+unit.x),legend.y[2],
                                 xmin+(i-1)*(gdata[1,"xmax"]-xmin+unit.x),legend.y[1]),
                               nrow=5, 
                               ncol=2, byrow=T)
        pattern<-pattern.type[[i]]
        pattern = pattern[seq(1, nrow(pattern), legend.pixel), seq(1, ncol(pattern), legend.pixel), ]
        legenddata[[i]]<-imagetodf2(pattern,  legendbox[[i]],left =legendbox[[i]][1, 1], right = legendbox[[i]][2, 1] ,bottom = legendbox[[i]][1, 2],top =legendbox[[i]][3, 2])
        legend.frame.xmin[i]<-legendbox[[i]][1,1]
        legend.frame.xmax[i]<-legendbox[[i]][2,1]
        legend.frame.ymax[i]<-legendbox[[i]][3,2]
        legend.frame.ymin[i]<-legendbox[[i]][1,2]
      }
      legend.label.y<-legend.y.pos*(legend.frame.ymin+legend.frame.ymax)
      legend.label.x<-legend.frame.xmax+0.1*(gdata[1,"xmax"]-xmin) 
    }
    
    #Text Data
    ldata<-ggplot_build(bplot)$data[[2]]
    frame.color2<-rep(frame.color,time=length(unique(x)))
    
    X<-Y<-r<-g<-b<-a<-pos<-x<-y<-label<-NULL
    g<- ggplot()+ mapply(function(i) geom_raster(data = picdata[[i]], aes(x = X, y = Y, fill = rgb(r,g, b,a))),1:dim(gdata)[1])+scale_fill_identity()+geom_rect(aes(xmin=gdata[,"xmin"],xmax=gdata[,"xmax"],ymin=0,ymax=gdata[,"ymax"]), color=frame.color2,size=frame.size, fill=NA)
    g<-g+ theme_bw()+xlab(xlab)+ylab(ylab)+ scale_x_continuous(breaks=seq(1:length(levels(x))), labels=levels(x))+geom_text(data=ldata, aes(x, y, label=label), vjust=vjust, size=label.size)
    g+ mapply(function(i) geom_raster(data = legenddata[[i]], aes(x = X, y = Y, fill = rgb(r,g, b,a))),1:length(pattern.type))+geom_rect(aes(xmin=legend.frame.xmin,xmax=legend.frame.xmax,ymin=legend.frame.ymin,ymax=legend.frame.ymax), color=frame.color,size=frame.size, fill=NA)+geom_text(aes(x=legend.label.x, y=legend.label.y, label=levels(group), hjust=0, vjust=0), size=label.size)
  }
  }

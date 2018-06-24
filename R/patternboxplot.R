#' Plot a boxplot using patterns and colors to fill the boxes.
#'
#' The \code{patternboxplot} function is a tool for creating versatile boxplots 
#' by filling the boxplots with colors and patterns. 
#' @importFrom Rcpp evalCpp
#' @useDynLib patternplot 
#' @param data the data to be used. 
#' @param x the variable used on x axis.
#' @param y the variable used on y axis.
#' @param group the variable used as the second grouping variable on x axis.
#' @param xlab a character string to give x axis label.
#' @param ylab a character string to give y axis label.
#' @param label.size the font size of labels shown above the boxplots.   
#' @param linetype the linetype for the borders of boxplots.
#' @param outlier.shape the shape of outlier dots. 
#' @param outlier.color the color of outlier dots.  
#' @param outlier.size the size of outlier dots.
#' @param pattern.type a vector of patterns to be filled in the boxes 
#' There are 15 pattern types: 'blank', 'bricks', 'circles1','circles2', 'vdashes', 'hdashes',
#' 'crosshatch','dots', 'grid','hlines','nelines', 'nwlines', 'shells', 'vlines', 'waves'. 
#' @param pattern.color a vector of colors for the lines/dots of patterns.
#' @param pattern.line.size a numeric value, the line size for the lines/dots of patterns.
#' @param background.color a vector of colors to be filled in the boxes.
#' @param frame.color the color for the borders of boxes.
#' @param frame.size a numeric value, the line size for the borders of boxes.
#' @param pixel a numeric value, the pixel resolution of boxplot. 
#' @param density a numeric vector, the density for the lines/dots of patterns. 
#' @param legend.type if legend.type='h', the layout of legends is horizontal; if legend.type='v', the layout of legends is vertical. 
#' @param legend.h a numeric value to change the height of legend boxes. 
#' @param legend.x.pos a numeric value to change the position of legend text on x axis.
#' @param legend.y.pos a numeric value to change the position of legend text on y axis.
#' @param legend.pixel a numeric value to change the pixel of legends. 
#' @param legend.ratio1 a numeric value to fine-tune the position of legend boxes on y axis.
#' @return  A ggplot object.
#'
#' @details \code{patternboxplot} function offers flexible ways in doing boxplots.
#'   
#' @author Chunqiao Luo (chunqiaoluo@gmail.com)
#'
#' @seealso Function \code{imageboxplot}  
#'
#' @example vignettes/example-imageboxplot.R

patternboxplot<-function(data,x, y, group=NULL, xlab='', ylab='',outlier.shape=21, outlier.color='black', outlier.size=1,
                   linetype=rep('solid', ifelse(is.null(group), length(unique(x)), length(unique(group)))),
                   pattern.type,
                   pattern.color=rep('black', ifelse(is.null(group), length(unique(x)), length(unique(group)))),
                   pattern.line.size=1, 
                   background.color=rep('white', ifelse(is.null(group), length(unique(x)), length(unique(group)))), 
                   frame.color=rep('black', ifelse(is.null(group), length(unique(x)), length(unique(group)))),
                   frame.size=1, pixel=1, 
                   density=rep(7, ifelse(is.null(group), length(unique(x)), length(unique(group)))),
                   legend.type='h', legend.h=6, legend.x.pos=0.5, legend.y.pos=0.5, legend.pixel=0.3, label.size=3.5, legend.ratio1=0.1){
  location<-gsub('\\','/',tempdir(), fixed=T)
  if(is.null(group)){
    
    bplot <- ggplot(data, aes(x, y)) + geom_boxplot() 
    gdata<-ggplot_build(bplot)$data[[1]]
    gdata<-gdata[order(gdata$group),]
    gdata$xmiddel<-(gdata$xmin+gdata$xmax)/2
    gdata$xrange<-(gdata$xmax-gdata$xmin)/6
    boxmatrix<-list()
    picdata<-list()
    picdf<-list()
    ymax<-max(gdata[, c('upper')])
    ymin<-min(gdata[, c('lower')])
    for (i in 1:dim(gdata)[1]){
      boxmatrix[[i]]<-matrix(c(gdata[i,"xmin"], gdata[i,"lower"],
                               gdata[i,"xmax"], gdata[i,"lower"],
                               gdata[i,"xmax"], gdata[i,"upper"],
                               gdata[i,"xmin"], gdata[i,"upper"],
                               gdata[i,"xmin"], gdata[i,"lower"]),
                             nrow=5, 
                             ncol=2, byrow=T)
      suppressWarnings(pattern(type=pattern.type[i], density=density[i], color=pattern.color[i], pattern.line.size=pattern.line.size, background.color=background.color[i], pixel=pixel))
      picdata[[i]]<-imagetodf2(readPNG(paste(location,'/',pattern.type[i],".png", sep='')),  boxmatrix[[i]],left =gdata[i,"xmin"], right =  gdata[i,"xmax"] ,bottom = ymin,top =ymax)
      picdata[[i]] <- filter(picdata[[i]], pos==1)
    }
    
    #Outliers
    outliers <-gdata[, 'outliers']
    count<-sapply(outliers, length)
    odata=data.frame(y=unlist(outliers), x=rep(gdata[, 'x'], count))
    
    g<- ggplot()+ mapply(function(i) geom_raster(data = picdata[[i]], aes(x = X, y = Y, fill = rgb(r,g, b,a))),1:dim(gdata)[1])+scale_fill_identity()+geom_rect(aes(xmin=gdata[,"xmin"],xmax=gdata[,"xmax"],ymin=gdata[,"lower"],ymax=gdata[,"upper"]), color=frame.color,size=frame.size, fill=NA)
    g<-g+geom_segment(aes(x=gdata[,"xmin"],y=gdata[,"middle"], xend=gdata[,"xmax"], yend=gdata[,"middle"]), color=frame.color,size=frame.size)+geom_segment(aes(x=gdata[,"xmiddel"],y=gdata[,"ymin"], xend=gdata[,"xmiddel"], yend=gdata[,"lower"]), color=frame.color, linetype=linetype,size=frame.size)+geom_segment(aes(x=gdata[,"xmiddel"],y=gdata[,"ymax"], xend=gdata[,"xmiddel"], yend=gdata[,"upper"]), color=frame.color, linetype=linetype,size=frame.size)
    g<-g+geom_segment(aes(x=gdata[,"xmiddel"]-gdata[, "xrange"],y=gdata[,"ymax"], xend=gdata[,"xmiddel"]+gdata[, "xrange"], yend=gdata[,"ymax"]), color=frame.color,size=frame.size)+geom_segment(aes(x=gdata[,"xmiddel"]-gdata[, "xrange"],y=gdata[,"ymin"], xend=gdata[,"xmiddel"]+gdata[, "xrange"], yend=gdata[,"ymin"]), color=frame.color,size=frame.size)
    g+ scale_x_continuous(breaks=gdata[,'group'], labels=levels(x))+ theme_bw()+xlab(xlab)+ylab(ylab)+geom_point(data=odata, aes(x, y), shape=outlier.shape, color=outlier.color, size=outlier.size)
    
  }else{
    
    bplot <- ggplot(data, aes(x, y, fill=group)) + geom_boxplot()
    gdata<-ggplot_build(bplot)$data[[1]]
    gdata<-gdata[order(gdata$group),]
    gdata$xmiddel<-(gdata$xmin+gdata$xmax)/2
    gdata$xrange<-(gdata$xmax-gdata$xmin)/6
    boxmatrix<-list()
    picdata<-list()
    picdf<-list()
    xmax<-max(gdata[, c('xmax')])
    xmin<-min(gdata[, c('xmin')])
    ymax<-max(gdata[, c('upper')])
    ymin<-min(gdata[, c('lower')])
    pattern.type2<-rep(pattern.type, time=length(unique(x)))
    background.color2<-rep(background.color,time=length(unique(x)))
    pattern.color2<-rep(pattern.color,time=length(unique(x)))
    density2<-rep(2*density,time=length(unique(x)))
    
    for (i in 1:dim(gdata)[1]){
      boxmatrix[[i]]<-matrix(c(gdata[i,"xmin"], gdata[i,"lower"],
                               gdata[i,"xmax"], gdata[i,"lower"],
                               gdata[i,"xmax"], gdata[i,"upper"],
                               gdata[i,"xmin"], gdata[i,"upper"],
                               gdata[i,"xmin"], gdata[i,"lower"]),
                             nrow=5, 
                             ncol=2, byrow=T)
      suppressWarnings(pattern(type=pattern.type2[i], density=density2[i], color=pattern.color2[i], pattern.line.size=pattern.line.size, background.color=background.color2[i], pixel=pixel))
      picdata[[i]]<-imagetodf2(readPNG(paste(location,'/',pattern.type2[i],".png", sep='')),  boxmatrix[[i]],left =gdata[i,"xmin"], right =  gdata[i,"xmax"] ,bottom = ymin,top =ymax)
      picdata[[i]] <- filter(picdata[[i]], pos==1)
      }
    
    legendbox<-list()
    legenddata<-list()
    ymax2<-max(gdata[, c('ymax_final')])
    if(legend.type=='v'){
      legend.y<-seq(from =ymax2+(legend.ratio1+0.05*legend.h)*ymax2, to = ymax2+legend.ratio1*ymax2, length.out=length(pattern.type)+1)
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
        suppressWarnings(pattern(type=pattern.type[i], density=density[i], color=pattern.color[i], pattern.line.size=0.5*pattern.line.size, background.color=background.color[i], pixel=legend.pixel))
        legenddata[[i]]<-imagetodf2(readPNG(paste(location,'/',pattern.type[i],".png", sep='')),  legendbox[[i]],left =legendbox[[i]][1, 1], right = legendbox[[i]][2, 1] ,bottom = legendbox[[i]][1, 2],top =legendbox[[i]][3, 2])
        legend.frame.xmin[i]<-legend.x[1]
        legend.frame.xmax[i]<-legend.x[2]
        legend.frame.ymax[i]<-legend.y[i]
        legend.frame.ymin[i]<-legend.y[i+1]
      }
      legend.label.y<-legend.y.pos*(legend.frame.ymin+legend.frame.ymax)
      legend.label.x<-legend.frame.xmax+legend.x.pos*(gdata[1,"xmax"]-xmin)
      
    }
    
    if(legend.type=='h'){
      legend.y<-c(ymax2+legend.ratio1*ymax2, ymax2+(legend.ratio1+0.01*legend.h)*ymax2)
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
        suppressWarnings(pattern(type=pattern.type[i], density=density[i], color=pattern.color[i], pattern.line.size=0.5*pattern.line.size, background.color=background.color[i], pixel=legend.pixel))
        legenddata[[i]]<-imagetodf2(readPNG(paste(location,'/',pattern.type[i],".png", sep='')),  legendbox[[i]],left =legendbox[[i]][1, 1], right = legendbox[[i]][2, 1] ,bottom = legendbox[[i]][1, 2],top =legendbox[[i]][3, 2])
        legend.frame.xmin[i]<-legendbox[[i]][1,1]
        legend.frame.xmax[i]<-legendbox[[i]][2,1]
        legend.frame.ymax[i]<-legendbox[[i]][3,2]
        legend.frame.ymin[i]<-legendbox[[i]][1,2]
      }
      legend.label.y<-legend.y.pos*(legend.frame.ymin+legend.frame.ymax)
      legend.label.x<-legend.frame.xmax+legend.x.pos*(gdata[1,"xmax"]-xmin) 
    }
    
    #Outliers
    outliers <-gdata[, 'outliers']
    count<-sapply(outliers, length)
    odata=data.frame(y=unlist(outliers), x=rep(gdata[, 'x'], count))
    
    frame.color2<-rep(frame.color,time=length(unique(x)))
    linetype2<-rep(linetype,time=length(unique(x)))
    
    X<-Y<-r<-g<-b<-a<-pos<-NULL
    g<- ggplot()+ mapply(function(i) geom_raster(data = picdata[[i]], aes(x = X, y = Y, fill = rgb(r,g, b,a))),1:dim(gdata)[1])+scale_fill_identity()+geom_rect(aes(xmin=gdata[,"xmin"],xmax=gdata[,"xmax"],ymin=gdata[,"lower"],ymax=gdata[,"upper"]), color=frame.color2,size=frame.size, fill=NA)
    g<-g+geom_segment(aes(x=gdata[,"xmin"],y=gdata[,"middle"], xend=gdata[,"xmax"], yend=gdata[,"middle"]), color=frame.color2,size=frame.size)+geom_segment(aes(x=gdata[,"xmiddel"],y=gdata[,"ymin"], xend=gdata[,"xmiddel"], yend=gdata[,"lower"]), color=frame.color2, linetype=linetype2,size=frame.size)+geom_segment(aes(x=gdata[,"xmiddel"],y=gdata[,"ymax"], xend=gdata[,"xmiddel"], yend=gdata[,"upper"]), color=frame.color2, linetype=linetype2,size=frame.size)
    g<-g+geom_segment(aes(x=gdata[,"xmiddel"]-gdata[, "xrange"],y=gdata[,"ymax"], xend=gdata[,"xmiddel"]+gdata[, "xrange"], yend=gdata[,"ymax"]), color=frame.color2,size=frame.size)+geom_segment(aes(x=gdata[,"xmiddel"]-gdata[, "xrange"],y=gdata[,"ymin"], xend=gdata[,"xmiddel"]+gdata[, "xrange"], yend=gdata[,"ymin"]), color=frame.color2,size=frame.size)
    g<-g+ theme_bw()+xlab(xlab)+ylab(ylab)+ scale_x_continuous(breaks=seq(1:length(levels(x))), labels=levels(x))+geom_point(data=odata, aes(x, y), shape=outlier.shape, color=outlier.color, size=outlier.size)
    g+mapply(function(i) geom_raster(data = legenddata[[i]], aes(x = X, y = Y, fill = rgb(r,g, b,a))),1:length(pattern.type))+geom_rect(aes(xmin=legend.frame.xmin,xmax=legend.frame.xmax,ymin=legend.frame.ymin,ymax=legend.frame.ymax), color=frame.color,size=frame.size, fill=NA)+geom_text(aes(x=legend.label.x, y=legend.label.y, label=levels(group), hjust=0, vjust=0), size=label.size)
    
  }
    
  }


#' Plot a boxplot with boxes filled with png and jpeg images.
#'
#' The \code{imageboxplot} function is a tool for creating versatile boxplots 
#' by filling the boxplots with external png and jpeg images. 
#' @importFrom Rcpp evalCpp
#' @useDynLib patternplot 
#' @param data the data to be used. 
#' @param x the variable used on x axis.
#' @param y the variable used on y axis.
#' @param group the variable used as the second grouping variable on x axis.
#' @param xlab a character string to give x axis label.
#' @param ylab a character string to give y axis label.
#' @param label.size the font size of legend labels.   
#' @param pattern.type a list of objects returned by \code{readPNG} and \code{readJPEG} used to fill boxplots. 
#' @param frame.color the color for the borders of boxplots.
#' @param linetype the linetype for the borders of boxplots.
#' @param frame.size a numeric value, the line size for the borders of boxplots.
#' @param outlier.shape the shape of outlier dots. 
#' @param outlier.color the color of outlier dots. 
#' @param outlier.size the size of outlier dots. 
#' @param legend.type if legend.type='h', the layout of legends is horizontal; if legend.type='v', the layout of legends is vertical. 
#' @param legend.h a numeric value to fine-tune the width of legend boxes on y axis. 
#' @param legend.x.pos a numeric value to change the position of legend text on x axis.
#' @param legend.y.pos a numeric value to change the position of legend text on y axis.
#' @param legend.w a numeric value to change the width of legends. 
#' @param legend.pixel a numeric value to change the pixel of legend boxes. 
#' @param legend.label a vector to name legend labels. 
#' @return  A ggplot object.
#'
#' @details \code{imageboxplot} function offers flexible ways of doing boxplots.
#'   
#' @author Chunqiao Luo (chunqiaoluo@gmail.com)
#'
#' @seealso Function \code{patternboxplot}  
#'
#' @example vignettes/example-imageboxplot.R

imageboxplot<-function(data,x, y, group=NULL , xlab='', ylab='',label.size=3.5,  pattern.type, frame.color='black', linetype='solid',
                       frame.size=1, outlier.shape=21, outlier.color='black', outlier.size=1,legend.type='h', legend.h=6, legend.x.pos=1.1, legend.y.pos=0.49, legend.w=0.2, legend.pixel=0.3, legend.label){

  if(is.null(group)){
    
    bplot <- ggplot(data, aes(x, y)) + geom_boxplot() 
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
    for (i in 1:dim(gdata)[1]){
      boxmatrix[[i]]<-matrix(c(gdata[i,"xmin"], gdata[i,"lower"],
                               gdata[i,"xmax"], gdata[i,"lower"],
                               gdata[i,"xmax"], gdata[i,"upper"],
                               gdata[i,"xmin"], gdata[i,"upper"],
                               gdata[i,"xmin"], gdata[i,"lower"]),
                             nrow=5, 
                             ncol=2, byrow=T)
      pattern<-pattern.type[[i]]
      picdata[[i]]<-imagetodf2(pattern, boxmatrix[[i]],left =gdata[i,"xmin"], right = gdata[i,"xmax"] ,bottom = gdata[i,"lower"],top =gdata[i,"upper"]  )
      picdata[[i]] <- filter(picdata[[i]], pos==1)
    }
    
    pattern.type<-NULL
    
    #Outliers
    outliers <-gdata[, 'outliers']
    count<-sapply(outliers, length)
    odata=data.frame(y=unlist(outliers), x=rep(gdata[, 'x'], count), stringsAsFactors = T)
    
    g<- ggplot()+ mapply(function(i) geom_tile(data = picdata[[i]], aes(x = X, y = Y, fill = rgb(r,g, b,a))),1:dim(gdata)[1])+scale_fill_identity()+geom_rect(aes(xmin=gdata[,"xmin"],xmax=gdata[,"xmax"],ymin=gdata[,"lower"],ymax=gdata[,"upper"]), color=frame.color,size=frame.size, fill=NA)
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
    pattern.type2<-rep(pattern.type, time=length(unique(x)))
    xmax<-max(gdata[, c('xmax')])
    xmin<-min(gdata[, c('xmin')])
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
      pattern<-pattern.type2[[i]]
      picdata[[i]]<-imagetodf2(pattern, boxmatrix[[i]],left =gdata[i,"xmin"], right = gdata[i,"xmax"] ,bottom = gdata[i,"lower"],top =gdata[i,"upper"]  )
      picdata[[i]] <- filter(picdata[[i]], pos==1)
    }
    legendbox<-list()
    legenddata<-list()
    ymax2<-max(gdata[, c('ymax_final')])
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
      legend.label.x<-legend.x.pos*legend.w
    }
    
    if(legend.type=='h'){
      legend.y<-c(ymax+0.1*ymax, ymax+(0.1+0.01*legend.h)*ymax)
      legend.x.s<-seq(from =xmin, to =0.67* xmax, length.out=length(pattern.type))
      legend.frame.xmin<-vector(mode="numeric", length=length(pattern.type))
      legend.frame.xmax<-vector(mode="numeric", length=length(pattern.type))
      legend.frame.ymin<-vector(mode="numeric", length=length(pattern.type))
      legend.frame.ymax<-vector(mode="numeric", length=length(pattern.type))
      for (i in 1:length(pattern.type)){
        legendbox[[i]]<-matrix(c(legend.x.s[i], legend.y[1],
                                 legend.x.s[i]+legend.w, legend.y[1],
                                 legend.x.s[i]+legend.w,legend.y[2],
                                 legend.x.s[i],legend.y[2],
                                 legend.x.s[i],legend.y[1]),
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
      legend.label.y<-(legend.frame.ymin+legend.frame.ymax)*legend.y.pos
      legend.label.x<-legend.x.s+legend.x.pos*legend.w
    }
    

    #Outliers
    outliers <-gdata[, 'outliers']
    count<-sapply(outliers, length)
    odata=data.frame(y=unlist(outliers), x=rep(gdata[, 'x'], count), stringsAsFactors = T)
    
    frame.color2<-rep(frame.color,time=length(unique(x)))
    linetype2<-rep(linetype,time=length(unique(x)))
    
    ldata<-data.frame(l.x=legend.label.x, l.y=legend.label.y, l.label=legend.label, stringsAsFactors = T)
    X<-Y<-r<-g<-b<-a<-pos<-l.x<-l.y<-l.label<-NULL
    
    g<- ggplot()+ mapply(function(i) geom_tile(data = picdata[[i]], aes(x = X, y = Y, fill = rgb(r,g, b,a))),1:dim(gdata)[1])+scale_fill_identity()+geom_rect(aes(xmin=gdata[,"xmin"],xmax=gdata[,"xmax"],ymin=gdata[,"lower"],ymax=gdata[,"upper"]), color=frame.color2,size=frame.size, fill=NA)
    g<-g+geom_segment(aes(x=gdata[,"xmin"],y=gdata[,"middle"], xend=gdata[,"xmax"], yend=gdata[,"middle"]), color=frame.color2,size=frame.size)+geom_segment(aes(x=gdata[,"xmiddel"],y=gdata[,"ymin"], xend=gdata[,"xmiddel"], yend=gdata[,"lower"]), color=frame.color2, linetype=linetype2,size=frame.size)+geom_segment(aes(x=gdata[,"xmiddel"],y=gdata[,"ymax"], xend=gdata[,"xmiddel"], yend=gdata[,"upper"]), color=frame.color2, linetype=linetype2,size=frame.size)
    g<-g+geom_segment(aes(x=gdata[,"xmiddel"]-gdata[, "xrange"],y=gdata[,"ymax"], xend=gdata[,"xmiddel"]+gdata[, "xrange"], yend=gdata[,"ymax"]), color=frame.color2,size=frame.size)+geom_segment(aes(x=gdata[,"xmiddel"]-gdata[, "xrange"],y=gdata[,"ymin"], xend=gdata[,"xmiddel"]+gdata[, "xrange"], yend=gdata[,"ymin"]), color=frame.color2,size=frame.size)
    g<-g+ theme_bw()+xlab(xlab)+ylab(ylab)+ scale_x_continuous(breaks=seq(1:length(levels(x))), labels=levels(x))+geom_point(data=odata, aes(x, y), shape=outlier.shape, color=outlier.color, size=outlier.size)
    g<-g+mapply(function(i) geom_tile(data = legenddata[[i]], aes(x = X, y = Y, fill = rgb(r,g, b,a))),1:length(pattern.type))+geom_rect(aes(xmin=legend.frame.xmin,xmax=legend.frame.xmax,ymin=legend.frame.ymin,ymax=legend.frame.ymax), color=frame.color,size=frame.size, fill=NA)
    g+geom_text(data=ldata, aes(x=l.x, y=l.y, label=l.label), hjust=0, vjust=0, size=label.size)
    
  }
    
  }




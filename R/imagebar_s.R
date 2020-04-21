#' Plot a stacked bar chart with bars filled with png and jpeg images.
#'
#' The \code{imagebar_s} function is a tool for creating versatile stacked bar charts by filling the bars with external png and jpeg images. 
#' @importFrom Rcpp evalCpp
#' @useDynLib patternplot 
#' @param data the data to be used. 
#' @param x the variable used on x axis.
#' @param y the variable used on y axis.
#' @param group the variable used as the second grouping variable on x axis.
#' @param xlab a character string to give x axis label.
#' @param ylab a character string to give y axis label.
#' @param pattern.type a list of objects returned by \code{readPNG} and \code{readJPEG} used to fill the stacked bars. 
#' @param label.size the font size of legend labels shown above the stacked bars.   
#' @param frame.color the color of the borders of the stacked bars. 
#' @param frame.size a numeric value, the line size for the borders of the stacked bars. 
#' @param legend.type if legend.type='h', the layout of legends is horizontal;
#'  if legend.type='v', the layout of legends is vertical. 
#' @param legend.h a numeric value to change the height of legend boxes.
#' @param legend.x.pos a numeric value to change the position of legends on x axis.
#' @param legend.y.pos a numeric value to change the position of legends on y axis.
#' @param legend.w a numeric value to change the width of legends. 
#' @param legend.pixel a numeric value to change the pixel of legend boxes. 
#' @param legend.label a vector to name legend labels. 
#' @param bar.width a numeric value to change the width of the bars.  
#' @return  A ggplot object.
#'
#' @details \code{imagebar_s} function offers flexible ways of doing stacked bar charts.
#'   
#' @author Chunqiao Luo (chunqiaoluo@gmail.com)
#'
#' @seealso Function \code{patternbar_s}  
#'
#' @example vignettes/example-imagebar_s.R


imagebar_s<-function(data,x, y, group, xlab='', ylab='',  pattern.type, label.size=3.5,frame.color='black',frame.size=1,
                     legend.type='h', legend.h=6, legend.x.pos=1.1, legend.y.pos=0.49, legend.w=0.2, legend.pixel=20, legend.label, bar.width=0.9){
    location<-gsub('\\','/',tempdir(), fixed=T)
    
    bplot <- ggplot(data, aes(x, y, fill=group)) + geom_bar(stat="identity", position="stack", width = bar.width)
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
      boxmatrix[[i]]<-matrix(c(gdata[i,"xmin"], gdata[i,"ymin"],
                               gdata[i,"xmax"], gdata[i,"ymin"],
                               gdata[i,"xmax"], gdata[i,"ymax"],
                               gdata[i,"xmin"], gdata[i,"ymax"],
                               gdata[i,"xmin"], gdata[i,"ymin"]),
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
    
    ldata<-data.frame(l.x=legend.label.x, l.y=legend.label.y, l.label=legend.label, stringsAsFactors = T)
    X<-Y<-r<-g<-b<-a<-pos<-label<-l.x<-l.y<-l.label<-NULL
    g<- ggplot()+ mapply(function(i) geom_tile(data = picdata[[i]], aes(x = X, y = Y, fill = rgb(r,g, b,a))),1:dim(gdata)[1])+scale_fill_identity()+geom_rect(aes(xmin=gdata[,"xmin"],xmax=gdata[,"xmax"],ymin=0,ymax=gdata[,"ymax"]), color=frame.color,size=frame.size, fill=NA)
    g<-g+ theme_bw()+xlab(xlab)+ylab(ylab)+ scale_x_continuous(breaks=seq(1:length(levels(x))), labels=levels(x))
    g<-g+ mapply(function(i) geom_tile(data = legenddata[[i]], aes(x = X, y = Y, fill = rgb(r,g, b,a))),1:length(pattern.type))+geom_text(data=ldata, aes(x=l.x, y=l.y, label=l.label), hjust=0, vjust=0, size=label.size)+geom_rect(aes(xmin=legend.frame.xmin,xmax=legend.frame.xmax,ymin=legend.frame.ymin,ymax=legend.frame.ymax), color=frame.color,size=frame.size, fill=NA)
    g
    }
  


pattern<-function(type='bricks', density=8, pattern.line.size=0.5,color='black', background.color='white', pixel=0.7){
  #background color
  background_theme <- theme(panel.grid=element_blank(), panel.background = element_rect(fill = background.color), panel.border = element_rect(linetype = "solid", color=background.color,size=0.0001,fill = NA))
  
  if((density %% 2) != 0) {
    density<-density+1
  } 
  
  
  if(type=='blank'){
    var1<-rep(seq(1, 5, by=1), each=5)
    var2<-rep(seq(1, 5, by=1),5)
    df<-data.frame(var1, var2)
    p<-ggplot(df, aes(var1, var2)) + geom_point(aes( x = var1, y = var2), size=pattern.line.size, color=background.color)+background_theme
    
  }
  
  
  if(type=='dots'){
    var1<-rep(seq(1, density*2, by=1), density*2)
    var1<-sort(var1)
    var2<-rep(seq(1, density*2, by=1), density*2)
    df<-data.frame(var1, var2)
    p<-ggplot(df, aes(var1, var2)) + geom_point(aes( x = var1, y = var2), shape=20,size=pattern.line.size, color=color)+coord_equal()+background_theme+scale_x_continuous(limits=range(var1), expand = c(0, 0))+scale_y_continuous(limits=range(var2), expand = c(0, 0))
    
  }
  

  fullcircle<- function(center=c(0,0), npoints=400){
    hc <- seq(0, 2*pi, length.out=npoints)
    df <- data.frame(
      x = center[1] + cos(hc)/2,
      y = center[2] + sin(hc)/2
    )
    df
  }
  
  if(type=='circles1'){
    var1<-rep(seq(1, density*1.5, by=1), density*1.5)
    var1<-sort(var1)
    var2<-rep(seq(1, density*1.5, by=1), density*1.5)
    df<-data.frame(var1, var2)
    
    hcircledata<-list()
    for (i in 1:nrow(df)){
      hcircledata[[i]] <- fullcircle(c(df[i,1], df[i,2]))
      
    }
    p<-ggplot(df, aes(var1, var2)) + mapply(function(i) geom_path(data=hcircledata[[i]], aes_string(x='x',y='y'), color=color, size=pattern.line.size), i=1:nrow(df))+coord_equal()+background_theme+scale_x_continuous(limits=range(var1), expand = c(0, 0))+scale_y_continuous(limits=range(var2), expand = c(0, 0))
    
  }
  
  if (type=='circles2'){
    var1<-rep(seq(1, density, by=1), density/2)
    var1<-sort(var1)
    var2<-rep(c(seq(1, density, by=2), seq(2, density, by=2)), density/2)
    df<-data.frame(var1, var2)
    
    hcircledata<-list()
    for (i in 1:nrow(df)){
      hcircledata[[i]] <- fullcircle(c(df[i,1], df[i,2]))
      
    }
    
    
    p<-ggplot(df, aes(var1, var2)) + mapply(function(i) geom_path(data=hcircledata[[i]], aes_string(x='x',y='y'), color=color, size=pattern.line.size), i=1:nrow(df))+coord_equal()+background_theme+scale_x_continuous(limits=range(var1), expand = c(0, 0))+scale_y_continuous(limits=range(var2), expand = c(0, 0))
    p
  }
  
  if (type=='shells'){
    density2<-2*density
    var1<-rep(seq(1, density2, by=1), density2/2)
    var1<-sort(var1)
    var2<-rep(c(seq(1, density2, by=2), seq(2, density2, by=2)), density2/2)
    df<-data.frame(var1, var2)
    
    halfcircle<- function(center=c(0,0), npoints=100){
      hc <- seq(0, pi, length.out=npoints)
      df <- data.frame(
        x = center[1] + cos(hc),
        y = center[2] + sin(hc)
      )
      df
    }
    
    hcircledata<-list()
    for (i in 1:nrow(df)){
      hcircledata[[i]] <- halfcircle(c(df[i,1], df[i,2]),npoints = 1000)
      
    }
    
    p<-ggplot(df, aes(var1, var2)) + mapply(function(i) geom_path(data=hcircledata[[i]], aes_string(x='x',y='y'), color=color, size=pattern.line.size), i=1:nrow(df))+coord_equal()+background_theme+scale_x_continuous(limits=range(var1)+0.01, expand = c(0, 0))+scale_y_continuous(limits=range(var2)+0.01, expand = c(0, 0))
    p
  }
  
  var1<-rep(seq(0, density, by=1), density)
  var1<-sort(var1)
  var2<-rep(seq(0, density, by=1), density)
  df<-data.frame(var1, var2)
  if(type=='hlines'){
    p<-ggplot(df, aes(var1, var2)) + geom_hline(aes(yintercept=var2), size=pattern.line.size, color=color)+coord_equal()+background_theme+scale_x_continuous(limits=range(var1), expand = c(0, 0))+scale_y_continuous(limits=range(var2), expand = c(0, 0))
    
  }
  
  if(type=='vlines'){
    p<-ggplot(df, aes(var1, var2)) + geom_vline(aes(xintercept=var2), size=pattern.line.size,color=color)+coord_equal()+background_theme+scale_x_continuous(limits=range(var1), expand = c(0, 0))+scale_y_continuous(limits=range(var2), expand = c(0, 0))
    
  }
  
  if (type=='grid'){
    p<-ggplot(df, aes(var1, var2)) + geom_hline(aes(yintercept=var2), size=pattern.line.size, color=color) + geom_vline(aes(xintercept=var2), size=pattern.line.size, color=color) +coord_equal()+background_theme+scale_x_continuous(limits=range(var1), expand = c(0, 0))+scale_y_continuous(limits=range(var2), expand = c(0, 0))
    
  }
  
  if (type=='bricks'){
    var1<-rep(seq(1, density, by=1), density/2)
    var1<-sort(var1)
    var2<-rep(c(seq(1, density, by=2), seq(2, density, by=2)), density/2)
    df<-data.frame(var1, var2)
    p<-ggplot(df, aes(var1, var2)) + geom_hline(aes(yintercept=var2), size=pattern.line.size, color=color) + geom_segment(aes(x =var1, y = var2-1, xend =var1, yend = var2), size=pattern.line.size, color=color)+coord_equal()+background_theme+scale_x_continuous(limits=range(var1), expand = c(0, 0))+scale_y_continuous(limits=range(var2), expand = c(0, 0))
    p
  }
  
  if (type=='vdashes'){
    var1<-rep(seq(1, density, by=1), density/2)
    var1<-sort(var1)
    var2<-rep(c(seq(1, density, by=2), seq(2, density, by=2)), density/2)
    df<-data.frame(var1, var2)
    p<-ggplot(df, aes(var1, var2))+ geom_segment(aes(x =var1, y = var2-1, xend =var1, yend = var2), size=pattern.line.size, color=color)+coord_equal()+background_theme+scale_x_continuous(limits=range(var1), expand = c(0, 0))+scale_y_continuous(limits=range(var2), expand = c(0, 0))
    p
  }
  
  if (type=='hdashes'){
    var1<-rep(seq(1, density, by=1), density/2)
    var1<-sort(var1)
    var2<-rep(c(seq(1, density, by=2), seq(2, density, by=2)), density/2)
    df<-data.frame(var1, var2)
    p<-ggplot(df, aes(var1, var2))+ geom_segment(aes(y =var1, x = var2-1, yend =var1, xend = var2), size=pattern.line.size, color=color)+coord_equal()+background_theme+scale_x_continuous(limits=range(var1), expand = c(0, 0))+scale_y_continuous(limits=range(var2), expand = c(0, 0))
    p
  }
  
  if(type=='waves'){
    var1<-rep(seq(0, 5*density, length=density*100), density)
    var2<-sin(var1)
    var3<-rep(seq(0, (density-1)*2 , by=2),length=100*density*density)
    var3<-sort(var3)
    var2<-var2+var3
    df<-data.frame(var1, var2, var3)
    
    p<-ggplot(df, aes(var1, var2, group=var3)) + geom_path(size=pattern.line.size, color=color)+background_theme+scale_x_continuous(limits=range(var1), expand = c(0, 0))+scale_y_continuous(limits=range(var2), expand = c(0, 0))
    
  }
  
  var1<-c(rep(0, density+1), rep(density*2, density+1), rep(seq(2, density*2-2, by=2),2))
  var2<-c(rep(seq(0, density*2, by=2),2), rep(0, length=length(seq(2, density*2-2, by=2))),rep(density*2, length=length(seq(2, density*2-2, by=2)))) 
  
  var3<-c(1:(density*2+1),1, (density*2+1):(density+3), density:2)
  var4<-c(1:(density+1),(density+1), (density+2):(density*2+1), 2:density, (density+2):(density*2))
  df<-data.frame(var1, var2, var3,var4)
  
  if(type=='nelines'){
    p<-ggplot(df, aes(var1, var2, group=var3)) + geom_path(size=pattern.line.size, color=color)+coord_equal()+background_theme+scale_x_continuous(limits=range(var1), expand = c(0, 0))+scale_y_continuous(limits=range(var2), expand = c(0, 0))
    
  }
  
  if(type=='nwlines'){
    p<-ggplot(df, aes(var1, var2, group=var4)) + geom_path(size=pattern.line.size, color=color)+coord_equal()+background_theme+scale_x_continuous(limits=range(var1), expand = c(0, 0))+scale_y_continuous(limits=range(var2), expand = c(0, 0))
    
  }
  
  if(type=='crosshatch'){
    p<-ggplot(df, aes(var1, var2)) + geom_path(aes(var1, var2, group=var3), size=pattern.line.size, color=color)+ geom_path(aes(var1, var2, group=var4), size=pattern.line.size, color=color)+coord_equal()+background_theme+scale_x_continuous(limits=range(var1), expand = c(0, 0))+scale_y_continuous(limits=range(var2), expand = c(0, 0))
    
  }
  gt<-ggplotGrob(p)
  gt<-gtable_filter(gt, "panel")
  
  outputlocation<-gsub('\\\\\\\\','/',tempdir())
  ggsave(paste(outputlocation,'/',type,'.png', sep=''), gt,width = pixel, height =pixel)
  
}


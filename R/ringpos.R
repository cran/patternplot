
ringpos<- function(r1=0.5, r2=0.3, start=0, end=2, length=800){
  slice.position <- seq(start*pi, end*pi, length.out=length) 
  df1 <- data.frame(
    x = r1*cos(slice.position),
    y = r1*sin(slice.position), stringsAsFactors = T
  )
  slice.position <- seq(end*pi, start*pi, length.out=length) 
  df2 <- data.frame(
    x = r2*cos(slice.position),
    y = r2*sin(slice.position), stringsAsFactors = T
  )
  df <- do.call("rbind", list(df1, df2, df1[1,]))
  df
}

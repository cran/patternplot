
innercircle <- function(r3=0.2, length=800){
  slice.position <- seq(0*pi, 2*pi, length.out=length) 
  df <- data.frame(
    x = r3*cos(slice.position),
    y = r3*sin(slice.position)
  )
  df
}

# convert an hsv colour to rgb  
hsv2rgb = function(x){  
  
  # input:  a 3 x 1 matrix (same as output of rgb2hsv() function)  
  # output: vector of length 3 with values in [0,1]    
  
  # recover h, s, v values  
  h <- x[1,1]  
  s <- x[2,1]  
  v <- x[3,1]    
  
  # follow the algorithm from Wikipedia  
  C <- s*v   
  
  # in R, h takes values in [0,1] rather than [0, 360], so dividing by  
  # 60 degrees is the same as multiplying by six  
  hdash <- h*6  
  X <- C * (1 - abs(hdash %% 2 -1))
  
  if (0 <= hdash & hdash <=1) RGB1 <- c(C, X, 0)  
  if (1 <= hdash & hdash <=2) RGB1 <- c(X, C, 0)  
  if (2 <= hdash & hdash <=3) RGB1 <- c(0, C, X)  
  if (3 <= hdash & hdash <=4) RGB1 <- c(0, X, C)  
  if (4 <= hdash & hdash <=5) RGB1 <- c(X, 0, C)  
  if (5 <= hdash & hdash <=6) RGB1 <- c(C, 0, X)    
  
  # the output is a vector of length 3. This is the most convenient  
  # format for using as the col argument in an R plotting function  
  RGB1 + (v-C)
}

#make a colour a pastelle
pastellize <- function(x, p){
  
  # x is a colour
  # p is a number in [0,1]
  # p = 1 will give no pastellization
  
  # convert hex or letter names to rgb
  if (is.character(x)) x <- col2rgb(x)/255
  
  # convert vector to rgb
  if (is.numeric(x)) x <- matrix(x, nr=3)
  
  col <- rgb2hsv(x, maxColorValue=1)
  col[2,1] <- col[2,1]*p
  col <- hsv2rgb(col)
  
  # return in convenient format for plots
  rgb(col[1], col[2], col[3])
}

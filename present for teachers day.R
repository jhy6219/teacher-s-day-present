library(animation)
library(ggplot2)
library(tidyverse)
library(ggimage)

# 줄기는 geom_curve를 사용해서 그릴 예정이며, 뿌리 지점 (X,Y)값은 고정하고 xend,yend값이 곡선으로 움직인다..

#줄기 
a=0.5
w=2*pi
h=0.5
t=seq(from=0,to=3,by=0.1)
x=a*sin(w*t)
y=h*cos(pi*x/(2*a))
dt=0.1
mycurve<-cos(w*t)*(-0.1)


#꽃 (등고선 이용)
n=1000
mean=c(0,0)
.x <- mvtnorm::rmvnorm(n, mean)
df1 <- tibble(x = .x[, 1], y = .x[, 2])

geom_rose <- function(n,xcore,ycore, ...) {
  n=1000
  mean=c(0,0)
  df=tibble(x = .x[, 1]+xcore, y = .x[, 2]+ycore)
  list(
    stat_density_2d(
      aes(x = x, y = y, fill = stat(level)), data = df, 
      geom = "polygon", show.legend = FALSE, color = "grey80"),
    scale_fill_gradient2(...)
  )
}

#잎사귀 함수
geom_leaf <- function(x, xend, f, xoffset = 0, yoffset = 0, 
                      xflip = 1, yflip = 1, ...) {
  
  .x <- seq(x, xend, length.out = 100)
  .y <- f(.x)
  
  df <- tibble(x = c(.x, .y), y = c(.y, .x))
  df$x <- xflip * df$x + xoffset
  df$y <- yflip * df$y + yoffset
  
  geom_polygon(aes(x = x, y = y), data = df, ...)
}

#잎사귀 위치 (줄기의 곡률 계산)
leaf_root <- function(xend,yend,p,i){
  d <- sqrt(xend^2+yend^2)
  beta <- atan2(yend,xend)
  k = abs(mycurve[i])
  
  sx = (2*p-1)
  virtual.x = d/2*sx
  virtual.y = -sign(mycurve[i])*d/2*(sqrt(1/k^2-(sx)^2)-sqrt(1/k^2-1))
  
  cb = cos(beta)
  sb = sin(beta)
  
  leaf.x = cb*virtual.x -sb*virtual.y + d/2*cb
  leaf.y = sb*virtual.x +cb*virtual.y + d/2*sb
  
  return(c(leaf.x, leaf.y))
}

f <- function(x) x^2 / 2 # 잎사귀의 곡선 함수

# 살짝 진동하는 글씨를 위해 랜덤하게 글씨의 위치 설정
text.x<-17+rnorm(length(x),0,0.03)
text.y<-15+rnorm(length(x),0,0.03)
#최종 gif 만들
saveGIF({
  for (i in (1:length(x))){
    p1<-ggplot()+
      #꽃1
      geom_rect(mapping=aes(xmin=-5, xmax=30, ymin=0, ymax=20), fill="skyblue",alpha=0.2)+#하늘
      geom_image(mapping=aes(text.x[i],text.y[i]),image="text.png", size=0.47) +
      geom_curve(aes(x = 0, y = 0, xend = x[i], yend = y[i]+9), 
                 ncp = 1000, curvature = mycurve[i], size = 3, 
                 color = "olivedrab3")+
      coord_cartesian(xlim = c(-3, 23),ylim=c(-1,18))+
      geom_rose(1000, xcore=x[i],ycore=y[i]+7,
                low = "red", mid = "purple", high = "pink",
                midpoint = 0.075)+
      geom_leaf(0, 2, f, leaf_root(x[i], y[i]+9,0.5,i)[1],
                leaf_root(x[i], y[i]+8,0.3,i)[2], 0.5, 
                fill = "olivedrab3", color = "palegreen") +
      geom_leaf(0, 2, f, leaf_root(x[i], y[i]+9,0.5,i)[1],
                leaf_root(x[i], y[i]+9,0.3,i)[2], -0.5, 
                fill = "olivedrab3", color = "palegreen")+ 
      geom_image(mapping=aes(x[i],y[i]+7),image="student1.png", size=.1,color="white")+
      #꽃2
      geom_curve(aes(x = 0+5, y = 0, xend = x[i]+5, yend = y[i]+9), 
                 ncp = 1000, curvature = mycurve[i], size = 3, 
                 color = "olivedrab3")+
      geom_rose(1000, xcore=x[i]+5,ycore=y[i]+7,
                low = "red", mid = "purple", high = "pink",
                midpoint = 0.075)+
      geom_leaf(0, 2, f, leaf_root(x[i], y[i]+9,0.5,i)[1]+5,
                leaf_root(x[i], y[i]+8,0.3,i)[2], 0.5, 
                fill = "olivedrab3", color = "palegreen") +
      geom_leaf(0, 2, f, leaf_root(x[i], y[i]+9,0.5,i)[1]+5,
                leaf_root(x[i], y[i]+9,0.3,i)[2], -0.5, 
                fill = "olivedrab3", color = "palegreen")+ 
      geom_image(mapping=aes(x[i]+5,y[i]+7),image="student2.png", size=.1,color="white")+
      #꽃3
      geom_curve(aes(x = 0+10, y = 0, xend = x[i]+10, yend = y[i]+9), 
                 ncp = 1000, curvature = mycurve[i], size = 3, 
                 color = "olivedrab3")+
      geom_rose(1000, xcore=x[i]+10,ycore=y[i]+7,
                low = "red", mid = "purple", high = "pink",
                midpoint = 0.075)+
      geom_leaf(0, 2, f, leaf_root(x[i], y[i]+9,0.5,i)[1]+10,
                leaf_root(x[i], y[i]+8,0.3,i)[2], 0.5, 
                fill = "olivedrab3", color = "palegreen") +
      geom_leaf(0, 2, f, leaf_root(x[i], y[i]+9,0.5,i)[1]+10,
                leaf_root(x[i], y[i]+9,0.3,i)[2], -0.5, 
                fill = "olivedrab3", color = "palegreen")+ 
      geom_image(mapping=aes(x[i]+10,y[i]+7),image="student3.png", size=.1,color="white")+
      #꽃4
      geom_curve(aes(x = 0+15, y = 0, xend = x[i]+15, yend = y[i]+9), 
                 ncp = 1000, curvature = mycurve[i], size = 3, 
                 color = "olivedrab3")+
      geom_rose(1000, xcore=x[i]+15,ycore=y[i]+7,
                low = "red", mid = "purple", high = "pink",
                midpoint = 0.075)+
      geom_leaf(0, 2, f, leaf_root(x[i], y[i]+9,0.5,i)[1]+15,
                leaf_root(x[i], y[i]+8,0.3,i)[2], 0.5, 
                fill = "olivedrab3", color = "palegreen") +
      geom_leaf(0, 2, f, leaf_root(x[i], y[i]+9,0.5,i)[1]+15,
                leaf_root(x[i], y[i]+9,0.3,i)[2], -0.5, 
                fill = "olivedrab3", color = "palegreen")+ 
      geom_image(mapping=aes(x[i]+15,y[i]+7),image="student4.png", size=.1,color="white")+
      #꽃5
      geom_curve(aes(x = 0+20, y = 0, xend = x[i]+20, yend = y[i]+9), 
                 ncp = 1000, curvature = mycurve[i], size = 3, 
                 color = "olivedrab3")+
      geom_rose(1000, xcore=x[i]+20,ycore=y[i]+7,
                low = "red", mid = "purple", high = "pink",
                midpoint = 0.075)+
      geom_leaf(0, 2, f, leaf_root(x[i], y[i]+9,0.5,i)[1]+20,
                leaf_root(x[i], y[i]+8,0.3,i)[2], 0.5, 
                fill = "olivedrab3", color = "palegreen") +
      geom_leaf(0, 2, f, leaf_root(x[i], y[i]+9,0.5,i)[1]+20,
                leaf_root(x[i], y[i]+9,0.3,i)[2], -0.5, 
                fill = "olivedrab3", color = "palegreen")+ 
      geom_image(mapping=aes(x[i]+20,y[i]+7),image="student5.png", size=.1,color="white")+
      
      geom_image(mapping=aes(0,15),image="prof.png", size=0.35,color="yellow")+
      geom_rect(mapping=aes(xmin=-5, xmax=30, ymin=-2, ymax=0), fill="brown")+
      theme_void()
    print(p1)
  }
},interval = dt, ani.width = 1300, ani.height = 950,movie.name = "flower_git.gif")


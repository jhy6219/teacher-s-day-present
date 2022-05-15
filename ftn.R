library(animation)
library(ggplot2)
library(tidyverse)
library(ggimage)

## 꽃 
geom_flower <- function(xcore, #꽃의 중심 위치 
                        ycore,
                        n=1000, #꽃 만들때 난수 생성 수  
                        image_path,#이미지 경로 
                        image_size,
                        ...){
  #등고선으로 꽃 만들고, 꽃 모양이 변하면 안되니까 시드 고정 
  set.seed(1234) 
  mean=c(0,0)
  
  #난수 2쌍 생성
  .x <- mvtnorm::rmvnorm(n, mean) 
  df1 <- tibble(x = .x[, 1], y = .x[, 2])
  
  df=tibble(x = .x[, 1]+xcore, y = .x[, 2]+ycore)
  list(
    stat_density_2d(
      aes(x = x, y = y, fill = stat(level)), data = df, 
      geom = "polygon", show.legend = FALSE, color = "grey80"),
    geom_image(mapping=aes(xcore,ycore),image=image_path, size=image_size),
    scale_fill_gradient2(...)
  )
}

## 줄기 
geom_stem <- function(x=0, # 뿌리 위치 
                      y=0, 
                      xend, # 줄기 끝 위치  
                      yend,
                      curvature, # 곡률 
                      color="olivedrab3"
                    ){
  list(
    geom_curve(aes(x = x, y = y, xend = xend, yend = yend), 
               ncp = 1000, curvature = curvature, size = 6, 
               color = color)
      )
}


## 잎사귀
geom_leaf <- function(x=0, # 잎과 잎사귀와의 간격 
                      xend=2, # xend-x : 잎사귀 길이 
                      xoffset = 0, # 잎사귀 위치  
                      yoffset = 0, 
                      xflip = 1, # 잎사귀 방향 
                      yflip = 1,      
                      fill = "olivedrab3", 
                      color = "palegreen4"
                    ){
  
  # 잎사귀의 곡선 함수
  f <- function(x) x^2 / 2 
  
  .x <- seq(x, xend, length.out = 100)
  .y <- f(.x)
  
  df <- tibble(x = c(.x, .y), y = c(.y, .x))
  df$x <- xflip * df$x + xoffset
  df$y <- yflip * df$y + yoffset
  
  geom_polygon(data = df, 
               aes(x = x, y = y),
               fill = fill, 
               color = color)
}

# 잎사귀 위치 (줄기의 곡률 기반)
leaf_loc <- function(xend, # 꽃 위치 
                     yend,
                     p, # 위치(0~1 ->줄기 중심 기준 -100%~100%)
                     curvature # 줄기 곡률 
                     ){
  d <- sqrt(xend^2+yend^2)
  beta <- atan2(yend,xend)
  k = abs(curvature)
  
  sx = (2*p-1)
  virtual.x = d/2*sx
  virtual.y = -sign(curvature)*d/2*(sqrt(1/k^2-(sx)^2)-sqrt(1/k^2-1))

  cb = cos(beta)
  sb = sin(beta)
  
  leaf.x = cb*virtual.x -sb*virtual.y + d/2*cb
  leaf.y = sb*virtual.x +cb*virtual.y + d/2*sb
  
  return(c(leaf.x, leaf.y))
}

dancing_flower<-function(flower_xcore, # flower 만의 좌표 
                         flower_ycode,
                         flower_ox, # 전체 이미지 좌표로 볼때
                         flower_oy=9,
                         flower_image_path,
                         flower_image_size,
                         flower_low='red',
                         flower_mid='purple',
                         flower_high='pink',
                         stem_curvature
                         ){
  list(
    geom_stem(x=flower_ox,
              xend=flower_xcore+flower_ox,
              yend=flower_ycode+flower_oy,
              curvature=stem_curvature,
              ),
    geom_flower(xcore=flower_xcore+flower_ox, 
                ycore=flower_ycode+flower_oy,
                image_path=flower_image_path,
                image_size=flower_image_size,
                low = flower_low, 
                mid = flower_mid, 
                high = flower_high,
                midpoint = 0.075),
    geom_leaf(xoffset = leaf_loc(flower_xcore, flower_ycode+flower_oy,0.3,stem_curvature)[1]+flower_ox,  
              yoffset = leaf_loc(flower_xcore, flower_ycode++flower_oy-1,0.3,stem_curvature)[2], 
              xflip = 0.5),  
    geom_leaf(xoffset = leaf_loc(flower_xcore, flower_ycode+flower_oy,0.3,stem_curvature)[1]+flower_ox,  
              yoffset = leaf_loc(flower_xcore, flower_ycode+flower_oy-1,0.3,stem_curvature)[2], 
              xflip = -0.5) 
  )
}

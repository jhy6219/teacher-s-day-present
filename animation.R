setwd("~/Desktop/haeyoon_private/teachers_day_present")
source('ftn.R')
a=0.5
w=2*pi
h=0.5
t=seq(from=0,to=3,by=0.1)
x=a*sin(w*t)
y=h*cos(pi*x/(2*a))
dt=0.1
mycurve<-cos(w*t)*(-0.1)

# 글씨, 햇빛 움직임 설정 
.x <- mvtnorm::rmvnorm(length(x), mean=c(0,0)) 
sun_size=seq(0.27,0.31,length=5)
rnorm<- tibble(text_x = .x[, 1], text_y = .x[, 2], sun=c(rep(c(sun_size,rev(sun_size)),3),sun_size[1]))

# 애니메이션 저장 
saveGIF({
  for (i in (1:length(x))){
    p1<-ggplot()+
      geom_rect(mapping=aes(xmin=-5, xmax=30, ymin=0, ymax=22), fill="lightskyblue2",alpha=0.2)+#하늘
      dancing_flower(flower_xcore=x[i],
                     flower_ycode=y[i],
                     flower_ox=0,
                     flower_image_path=paste0(path,"/image/student1.png"),
                     flower_image_size=0.1,
                     flower_low = "palevioletred1", 
                     flower_mid = "firebrick2", 
                     flower_high = "lightpink1",
                     stem_curvature=mycurve[i])+
      dancing_flower(flower_xcore=x[i],
                     flower_ycode=y[i],
                     flower_ox=5,
                     flower_image_path=paste0(path,"/image/student2.png"),
                     flower_image_size=0.1,
                     flower_low = "palevioletred1", 
                     flower_mid = "firebrick2", 
                     flower_high = "lightpink1",
                     stem_curvature=mycurve[i])+
      dancing_flower(flower_xcore=x[i],
                     flower_ycode=y[i],
                     flower_ox=5*2,
                     flower_image_path=paste0(path,"/image/student3.png"),
                     flower_image_size=0.1,
                     flower_low = "palevioletred1", 
                     flower_mid = "firebrick2", 
                     flower_high = "lightpink1",
                     stem_curvature=mycurve[i])+
      dancing_flower(flower_xcore=x[i],
                     flower_ycode=y[i],
                     flower_ox=5*3,
                     flower_image_path=paste0(path,"/image/student4.png"),
                     flower_image_size=0.1,
                     flower_low = "palevioletred1", 
                     flower_mid = "firebrick2", 
                     flower_high = "lightpink1",
                     stem_curvature=mycurve[i])+
      dancing_flower(flower_xcore=x[i],
                     flower_ycode=y[i],
                     flower_ox=5*4,
                     flower_image_path=paste0(path,"/image/student5.png"),
                     flower_image_size=0.1,
                     flower_low = "palevioletred1", 
                     flower_mid = "firebrick2", 
                     flower_high = "lightpink1",
                     stem_curvature=mycurve[i])+
      geom_rect(mapping=aes(xmin=-5, xmax=30, ymin=-5, ymax=0), fill="sienna4")+
      coord_cartesian(xlim=c(-3,23),ylim=c(-2,20))+
      geom_image(mapping=aes(16+rnorm$text_x[i]/50,16+rnorm$text_y[i]/50),image=paste0(path,'/image/text.png'),size=0.6)+
      geom_image(mapping=aes(1,16),image=paste0(path,'/image/sun2.png'),size=0.2)+ # 해 
      geom_image(mapping=aes(1.145,15.83),image=paste0(path,'/image/sun1.png'),size=rnorm$sun[i]) + # 빛 
      geom_image(mapping=aes(1,16),image=paste0(path,'/image/prof.png'),size=0.13)
    print(p1)
  }
},interval = dt, ani.width = 1300, ani.height = 950,movie.name = paste0(path,"/plot/present.gif"))

c(seq(0.3,0.4,length=15),seq(0.4,0.3,length=15))


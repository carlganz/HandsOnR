library("ggplot2")
x<-seq(-1,1,by=.2)

y<-x^3

qplot(x,y)

x<-c(1,2,2,2,3,3)
qplot(x,binwidth=1)
x2<-c(1,1,1,1,1,2,2,2,2,3,3,4)
qplot(x2,binwidth=2)

x3<-c(0,1,1,2,2,2,3,3,4)
## 5,0 to 4, 1 2 3 2 1
qplot(x3,binwidth=1)

rolls<-replicate(10000,roll())
qplot(rolls,binwidth=1)

roll<-function() {
  die<-1:6
  dice<-sample(die,size=2,replace=T,prob=c(1/8,1/8,1/8,1/8,1/8,3/8))
  return(sum(dice))
}

rolls<-replicate(10000,roll())
qplot(rolls,binwidth=1)
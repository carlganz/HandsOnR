(((7+2)*3)-6)/3
##7
roll<-function() {
  die<-1:6
  dice<-sample(die,size=2,replace=T)
  return(sum(dice))
}

roll2<-function(bones=6) {
  die<-eval(parse(text=paste("1:",bones)))
  dice<-sample(die,size=2,replace=T)
  return(sum(dice))
}
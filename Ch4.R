deal<-function(cards){
  return(cards[1,])
}
shuffle<-function(cards) {
  random<-sample(1:52,size=52)
  cards[random,]
}

altdeal<-function(cards) {
  return(cards[sample(1:52,size=52),][1,])
}
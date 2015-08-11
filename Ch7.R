get_symbols<-function() {
  wheel<-c("DD","&","BBB","BB","B","C","0")
  sample(wheel,size=3,replace=T,
         prob=c(.03,.03,.06,.1,.25,.01,.52))
}

play<-function() {
  symbols<-get_symbols()
  print(symbols)
  print(score(symbols)) #you need to add an extra print statement to get the results like in the text
  score(symbols)
}

score<-function(symbols) {
  #identify case
  same<-symbols[1]==symbols[2] & symbols[1]==symbols[3]
  bars<-all(symbols %in% c("BBB","BB","B"))
  #get prize
  if (same) {
    payouts<-c("DD"=100,"7"=80,"BBB"=40,"BB"=25,"B"=10,"C"=10,"0"=0)
    prize<-unname(payouts[symbols[1]])
  } else if (bars) {
    prize<-5
  } else {
    cherries<-sum(symbols=="C")
    prize<-c(0,2,5)[cherries+1]
  }
  #adjust for diamonds
  diamonds<-sum(symbols=="DD")
  return(prize<-prize*2^diamonds)
}


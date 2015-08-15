wheel<-c("DD","7","BBB","BB","B","C","0")

prob=c("DD"=.03,"7"=.03,"BBB"=.06,"BB"=.1,"B"=.25,"C"=.01,"0"=.52)

combos<-expand.grid(wheel,wheel,wheel,stringsAsFactors = F)

combos$prob1<-prob[combos$Var1]
combos$prob2<-prob[combos$Var2]
combos$prob3<-prob[combos$Var3]

combos$prob<-combos$prob1*combos$prob2*combos$prob3

sum(combos$prob)

for (i in 1:nrow(combos)) {
  combos$prize[i]<-score(c(combos$Var1[i],combos$Var2[i],combos$Var3[i]))
}

combos$prize<-NA

score<-function(symbols) {
  diamonds<-sum(symbols=="DD")
  cherries<-sum(symbols=="C")
  
  #identify case
  #since diamondsare wild, only nondiamonds matterfor three of a kind and all bars
  slots<-symbols[symbols != "DD"]
  same<-length(unique(slots))==1
  bars<-all(slots %in% c("B","BB","BBB"))
  
  #assign prize
  if (diamonds==3) {
    prize<-100
  } else if (same) {
    payouts<-c("7"=80,"BBB"=40,"BB"=25,"B"=10,"C"=10,"0"=0)
    prize<-unname(payouts[slots[1]])
  } else if (bars) {
    prize<-5
  } else if (cherries>0) {
    #diamonds coust as cherries as long as there is one real cherry
    prize<-c(0,2,5)[cherries+diamonds+1]
  } else {
    prize<-0
  }
  #double for each diamond
  return(prize*2^diamonds)
}

for (i in 1:nrow(combos)) {
  combos$prize[i]<-score(c(combos$Var1[i],combos$Var2[i],combos$Var3[i]))
}

sum(combos$prob*combos$prize)
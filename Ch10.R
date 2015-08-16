long<-rep(c(-1,1),5000000)
system.time(abs(long))

change_symbols<-function(vec) {
  card<-c("DD"="joker","C"="ace","7"="king","B"="queen","BB"="jack","BBB"="ten","0"="nine")
  vec<-unname(card[vec])
}

get_many_symbols<-function(n) {
  wheel<-c("DD","7","BBB","BB","B","C","0")
           vec<-sample(wheel,size=3*n,replace=T,prob = c(.03,.03,.06,.1,.25,.01,.52))
           return(matrix(vec,ncol=3))
}

play_many<-function(n) {
  symb_mat<-get_many_symbols(n=n)
  return(data.frame(w1=symb_mat[,1],w2=symb_mat[,2],w3=symb_mat[,3],prize=score_many(symb_mat)))
}

score_many<-function(symbols) {
  #step 1: assign base prize based on cherries and diamonds
  ## count cherries and diamonds
  cherries<-rowSums(symbols=="C")
  diamonds<-rowSums(symbols=="DD")
  
  ##Wild diamonds count as cherries
  prize<-c(0,2,5)[cherries+diamonds+1]
  
  ##but not if there are zero real cherries
  #coerces to logical so if cherries equals zero it is false and not false is true so it sets it to zero if no cherries
  prize[!cherries]<-0
  
  #step 2: change prize for combinations that contain three of a kind
  same<-symbols[,1]==symbols[,2] & symbols[,2]==symbols[,3]
  payoffs<-c("DD"=100,"7"=80,"BBB"=40,"BB"=25,"B"=10,"C"=10,"0"=0)
  prize[same]<-payoffs[symbols[same,1]]
  
  #step 3: change prize for combinations that contin all bars
  bars<-symbols=="B" | symbols=="BB" | symbols=="BBB"
  all_bars<-bars[,1] & bars[,2] & bars[,3] & !same
  prize[all_bars]<-5
  
  #step 4: handle wilds
  
  ##combos with two diamonds
  two_wilds<-diamonds==2
  
  #identify the nonwild symbol
  one<-two_wilds & symbols[,1] != symbols[,2] & symbols [,2]==symbols[,3]
  two<-two_wilds & symbols[,1] != symbols[,2] & symbols[,1]==symbols[,3]
  three<-two_wilds & symbols[,1] == symbols[,2] & symbols[,2]!=symbols[,3]
  
  #combos with one wild
  one_wild<-diamonds==1
  
  #Treat as all bars (if appropriate)
  wild_bars<-one_wild & (rowSums(bars)==2)
  prize[wild_bars]<-5
  
  #treat as three of a kind (if appropriate)
  one<-one_wild & symbols[,1]==symbols[,2]
  two<-one_wild & symbols[,2]==symbols[,3]
  three<-one_wild & symbols[,3]==symbols[,1]
  prize[one]<-payoffs[symbols[one,1]]
  prize[two]<-payoffs[symbols[two,2]]
  prize[three]<-payoffs[symbols[three,3]]
  
  #step 5: Double prize for every diamond in combo
  return(unname(prize*2^diamonds))
}
# Yes, R will first search the active environment for deck, and then the parent environment, which does contain deck

deal<-function() {
  card<-deck[1,]
  assign("deck",deck[-1,],envir = globalenv())
  return(card)
}

shuffle<-function() {
  random<-sample(1:52,size=52)
  assign("deck",DECK[random,],envir = globalenv())
}

setup<-function(deck) {
  DECK<-deck
  
  DEAL<-function() {
    card<-deck[1,]
    assign("deck",deck[-1,],envir=parent.env(environment()))
    return(card)
  }
  
  SHUFFLE<-function() {
    random<-sample(1:52,size=52)
    assign("deck",DECK[random,],envir = parent.env(environment()))
  }
  return(list(deal=DEAL,shuffle=SHUFFLE))
}
deck<-read.csv("cards.csv")
cards<-setup(deck)
# make sure not to put paranthesis at end
deal<-cards$deal
shuffle<-cards$shuffle

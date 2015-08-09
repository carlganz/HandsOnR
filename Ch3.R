c("ace","king","queen","jack","ten")
##character
names(die)<-c("one","two","three","four","five","six")

hand1<-cbind(c("ace","king","queen","jack","ten"),"spades")
hand1<-c("ace","king","queen","jack","ten","spades","spades","spades","spades","spades")
matrix(hand1,nrow=5)
matrix(hand1,ncol=2)
dim(hand1)<-c(5,2)

card<-c("ace","hearts",1)

card<-list("ace","hearts",1)

df<-data.frame(face=c("ace","two","six"),suit=c("clubs","clubs","clubs"),value=c(1,2,3))

write.csv(deck,file="cards.csv",row.names=F)

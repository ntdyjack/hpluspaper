#revison C, starting with pai and pbi at (1,1) or (0,0) maybe means we only ever have to move one direction if we're careful
#maybe just go greedily from 0,0?
set.seed(1234)
b <- rnorm(mean=-0.3,n=10000)
a <- rnorm(mean=0.3,n=10000)
ht <- sum(sapply(a, function(x) sum(x>b)))/(length(a)*length(b))

p <- 51
ps <- seq(0,1,length.out=p)
qa <- quantile(a, probs = ps)
qb <- quantile(b, probs = ps)
l <- length(ps)

syvl <- sapply(sort(a), function(x) sum(x>b)/length(b))
sxvl <- seq(0,1,length.out=length(syvl))

clz <- comp <- t(sapply(qa, function(x) x>qb))
clz[which(comp)] <- '#0072B2'
clz[which(!comp)] <- '#D41159'
comp <- sapply(ps, function(x) x*ps)
brds<- matrix('black',nrow=nrow(comp),ncol=ncol(comp))
brds2 <- matrix(NA,nrow=nrow(comp),ncol=ncol(comp))
htcut <- 0.01
#s <- 0
ai <- bi <- 1
inty <- rep(0,l)
while(ai <= l & bi <= l){
  brds2[ai,bi] <- '#56B4E9'
  #rect(xleft=(ai-1)/l,xright=ai/l,ybottom=(bi-1)/l,ytop=bi/l, col=NA,border='#56B4E9')
  si <- qa[ai] > qb[bi]
  if(si){ #increase qB
    bi <- bi + 1
    #print(paste0('bi=',bi))
  }else{ #increase qA
    inty[ai] <- (bi-1)
    ai <- ai + 1
    #s <- s + (bi-1)
#    print(paste0('ai=',ai))
  }
  #print(s)
  #print(paste(ai,bi,s))
}
brds2[abs(comp-ht)<=0.01 & !is.na(brds2) ] <- '#009E73'
brds2[abs(comp-ht)<=0.01 &  is.na(brds2)] <- '#F0E442'

pdf('C:/Users/Nathan/Documents/hplus_scratchwork/gridtest4.pdf',width=8,height=8)
par(mar=c(3.1,3.1,0.1,0.1))

plot(x=0,y=0,xlim=c(-0.01,1.1),ylim=c(-0.01,1.1),ylab='',xlab='',type='n',xaxt='n',yaxt='n',xaxs = "i",yaxs = "i",bty='n')
axis(side=1,at=seq(0,1,by=0.25),las=1,mgp=c(0, .3, 0),line=0.0,cex.axis=0.8)
axis(side=2,at=seq(0,1,by=0.25),las=1,mgp=c(0, .5, 0),line=0.0,cex.axis=0.8)
mtext(side=1,text='q(A)',line=1.0,las=1,cex=1.5)
mtext(side=2,text='q(B)',line=2.0,las=3,cex=1.5)

for(i in 1:l){
  for(j in 1:l){
    rect(xleft=(i-1)/l,xright=i/l,ybottom=(j-1)/l,ytop=j/l, col=clz[i,j],border=brds[i,j],lwd=0.5)
  }
}

for(i in 1:l){
  for(j in 1:l){
    rect(xleft=(i-1)/l,xright=i/l,ybottom=(j-1)/l,ytop=j/l, col=NA,border=brds2[i,j],
      lwd=ifelse(brds2[i,j]=='#009E73',4,2))
  }
}
lines(x=sxvl,y=syvl,type='l',lwd=1.5,col='white')
dev.off()
#if(bi==l+1){ 
#  s <- s + (bi-1)
#}
#he1 <- sum(inty)/(l^2)
#he2 <- sum(sapply(qa, function(x) sum(x>qb))) / (l^2)
#he1
#he2

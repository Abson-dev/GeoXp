`makedistancew` <-
function(x,y,distance)
{
# initialisation
W=matrix(0,length(x),length(x));

Tp1=matrix(rep(t(x),length(x)),ncol=dim(t(x))[2],byrow=TRUE);
Tp2=matrix(rep(t(y),length(y)),ncol=dim(t(y))[2],byrow=TRUE);
H=sqrt((c(rep(x,length(x)))-Tp1)^2+(c(rep(y,length(y)))-Tp2)^2);
W[which(H <= distance)]=1;
W=W-diag(length(x));

return(W)
  }


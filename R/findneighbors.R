"findneighbors" <-
function (xc,yc,m)
{
# initialisation

n <- length(xc);
nnlist <- matrix(0, nrow=n, ncol=m);
dist <- matrix(0, nrow=n, ncol=n);
uns <- rep(1,n);

# calcul de la matrice contenant les indices des plus proches voisins de chaque observation

for (i in 1:n)
{
    dist[i,] <- sqrt((xc - xc[i]*uns)*(xc - xc[i]*uns) + (yc - yc[i]*uns)*(yc - yc[i]*uns));
    d1 <- dist[i,];
    names(d1) <- 1:n;
    d2 <- sort(d1,index.return=TRUE);
    x<-d2$x
    ix=d2$ix
    ind=which(x[2:(length(x)-1)]==x[3:length(x)])
    p<-length(ind)
    if(p!=0)
     {for (j in 1:p)
       ix[which(x==x[ind[j]+1])]=ix[sample(which(x==x[ind[j]+1]))]
     }
    nnlist[i,] <- as.character(ix)[2:(m+1)];
  }

return(nnlist);
  }


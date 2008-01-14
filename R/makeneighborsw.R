`makeneighborsw` <-
function(xc,yc,m,cum=TRUE)
{

# initialisation

list <- findneighbors (xc,yc,m);
w <- matrix(0,nrow=length(xc),ncol=length(xc))

if(cum==TRUE)
{for (j in 1:m)
 {
  for (i in 1:length(xc))
   {
    ind <- list[i,j];
    w[i,as.integer(ind)] <- 1;
   }
 }
}
else
 {
  for (i in 1:length(xc))
   {
    ind <- list[i,m];
    w[i,as.integer(ind)] <- 1;
   }
 }

return(w);
  }


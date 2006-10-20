"normw" <-
function(w)
{
# initialisation
res <- dim(w)
if(res[1]!=res[2])
{tkmessageBox(message="matrix must be squared",icon="Error",
type="ok")}

nterm <- rep(0,ncol(w))

# normalisation

for(i in 1:ncol(w))
{ v <- cumsum(w[i,])
 nterm[i] <- v[ncol(w)]
  }

for(i in 1:ncol(w))
{  if(nterm[i]!=0)
  {
    W <- w/nterm
  }
}
return(W)
} 
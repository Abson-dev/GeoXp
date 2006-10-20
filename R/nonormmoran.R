"nonormmoran" <-
function(y,x,W)
{
# initialisation

x<-as.matrix(x)
y<-as.vector(y)

if (length(dim(x)) == 0)
{
    n <- length(x);
    k <- 1;
  }
else
{
    res <- dim(x);
    n <- res[1];
    k <- res[2];
  }

# calcul des éléments nécessaires au calcul du I de Moran


x1 <- qr(x);
#b <- qr.coef(x1,y)
e <- qr.resid(x1,y);


epe <- e %*% e
mi <- (e %*% W %*% e)/epe
#s<-t(rep(1,n))%*%W%*%rep(1,n)

M <- diag(n) - (x %*% solve(t(x) %*% x) %*% t(x))

mat <-  M %*% W 

tmv<-sum(diag(mat))

meani <- tmv/(n-k);
mat1 <- (M %*% W) %*% (M %*% t(W))
mat2 <- (M %*% W) %*% (M %*% W)


tmw1<-sum(diag(mat1))
tmw2<-sum(diag(mat2))


vari <- tmw1 + tmw2 + tmv*tmv;
vari <- vari/((n-k)*(n-k+2));
vari <- vari - meani*meani;
mis <- (mi-meani)/sqrt(vari);
prob <- dnorm(mis);

nobs <- n;
nvar <- k;
morani <- round(mi,4);
istat <- mis;
imean <- round(meani,4);
ivar <- round(vari,4);
prob <- round(prob,4);


return(list(nobs=nobs,nvar=nvar,morani=morani,imean=imean,istat=istat,ivar=ivar,prob=prob))

  }


`graphique` <-
function(var1, var2, var3, obs,num, graph="",couleurs="", symbol=0,col=0, labvar="", nbcol=10, alpha1, W, Xpoly, Ypoly, F, G, opt1=1,opt2=1, quantiles=0.5, labmod="", direct,inertie,label=0,kernel, obsq, locmoran=0,bin=NULL)
{

####################################################
# définition la fenêtre sur laquelle le graphique va être tracé
####################################################

dev.set(num)

par(mar=c(4.5,4.5,0.5,0.5))

####################################################
# Dessin d'un histogramme
####################################################

if (graph == "Histogram")
{
    # initialisation
    mnv <- min(var1)
    mxv <- max(var1)
    h <- (mxv - mnv)/(nbcol)
    cpt <- NULL
    absc <- mnv
    
    # calcul des effectifs 
    for (i in 1:nbcol)
    {   
        cnt <- 0;
        x1 <- mnv + h*i


        for (j in 1:length(var1))
        {   

           if(x1==(mnv + h))
            {if ((var1[j] >= absc[i]) && (var1[j] <= x1))
             {   
                cnt <- cnt+1
             }
            }
           else
              {if ((var1[j] > absc[i]) && (var1[j] <= x1))
               {   
                cnt <- cnt+1
               }
              }
        }
        cpt <- c(cpt, cnt)
        absc <- c(absc, x1)
    }   

    cpt <- c(cpt,0);

    # dessin des barres de l'histogramme
    plot(absc,cpt,"n",xlim=c(mnv-h, mxv+h), ylim=c(0, max(cpt)), xlab=labvar, ylab="Number")
    
     for (i in 1:nbcol)
    {   
         if (symbol == 1)
         {
              rect(absc[i],0,absc[i+1],cpt[i], lwd=1)
         }
         else
         {
              rect(absc[i],0,absc[i+1],cpt[i],col="blue")
         }
     }
    
    # dessin des unités sélectionnées
    if (length(var1[obs])!=0)
    {
        vrob <- var1[obs]
        cpt1 <- NULL
    
        # calcul des effectifs 
        for (i in 1:nbcol)
        {   
            cnt <- 0
           if(i==1)
            {
             for (j in 1:length(vrob))
             {
                if ((vrob[j] >= absc[i]) && (vrob[j] <= absc[i+1]))
                {   
                    cnt <- cnt+1
                }
             }
            }
           else
            {
             for (j in 1:length(vrob))
             {
                if ((vrob[j] > absc[i]) && (vrob[j] <= absc[i+1]))
                {   
                    cnt <- cnt+1
                }
             }
            }
            cpt1 <- c(cpt1, cnt)
        }

        cpt1 <- c(cpt1,0)

        # dessin des barres de l'histogramme pour les points sélectionnés
         for (i in 1:nbcol)
         {   
             if (symbol == 1)
             {
                 rect(absc[i],0,absc[i+1],cpt1[i],density=4, angle=45, lwd=1, col="red")
             }
             else
             {
                 rect(absc[i],0,absc[i+1],cpt1[i],col="red")
             }
         }
    }
  }


####################################################
# Dessin d'un diagramme en barres (idem pour cluster)
####################################################

if ((graph == "Barplot")||(graph == "Cluster"))
{
    # dessin du diagramme en barres

    r <- table(var1)
    nomsr <- names(r)

if(labmod[1]=="")
{
    labmod <- nomsr
}


if(length(couleurs)==1)
{
if(length(labmod)<5)
 {couleurs <- c('gold','orchid1','coral','chartreuse3')
 }
 else
 {
 couleurs <- terrain.colors(length(nomsr)+1)
 }
 }


   if (col==0)
    {
     if (symbol == 1)
     {
        g <- barplot(r, xlim=c(0,length(r)), names.arg=labmod, width=0.8, col="white", lwd=1, xlab=labvar , ylab="Number")
     }
     else 
     {
        g <- barplot(r, xlim=c(0,length(r)), names.arg=labmod, width=0.8, col="blue", xlab=labvar, ylab="Number")
     }
    }
    else
     {
        g <- barplot(r, xlim=c(0,length(r)), col=couleurs, names.arg=labmod, width=0.8, xlab=labvar, ylab="Number")
     }



    # dessin des unités sélectionnées
    if (length(var1[obs])!=0)
    {
        # initialisation
        t <- table(var1[obs])
        nomst <- names(t)

        for (i in 1:length(t))
        {
            quit <- FALSE
            j <- 1
            while ((j<=length(r)) && (!quit))
            {
                if (nomst[i] == nomsr[j])
                {
                    # dessin des barres pour les unités sélectionnées
                    if (symbol == 1)    
                    {
                        rect(g[j]-0.4,0,g[j]+0.4,t[[i]], col="red", density=4, angle=45, lwd=1)
                    }
                    else
                    {
                        rect(g[j]-0.4,0,g[j]+0.4,t[[i]], col="red")
                    }
                    quit <- TRUE
                }
                j <- j+1
            }
        }
    }
  }


####################################################
# Dessin d'un scatterplot
####################################################

if (graph == "Scatterplot")
{
   couleur <- heat.colors(length(quantiles))
    # dessin du scatterplot

par()
layout( matrix( c(1,3,0,2), 2, 2, byrow=TRUE ),
        c(1,4), c(5,1),
      )


par(mar=c(2,1,2,2))
boxplot(var2, axes=FALSE)

title(ylab=labvar[2], line=0)

par(mar=c(1,2,1,2))
boxplot(var1, horizontal=TRUE, axes=FALSE)


title(xlab=labvar[1], line=0)

par(mar=c(2,2,2,2))

    plot(var1,var2,"n",xlab='',ylab='')
    points(var1[!obs],var2[!obs],col="blue",pch=16);
  
      if(opt1==2)
      {xg <- seq(min(var1), max(var1), length = 100)
       reg<-lm(var2~var1)
       lines(xg,reg$coefficients[1]+reg$coefficients[2]*xg)}
       
       

    if (opt1 == 3)
    {
        for (i in 1:length(quantiles))
        {
            fit <- qsreg(as.numeric(var1),as.numeric(var2),lam=alpha1, alpha=quantiles[i])
            xg<- seq(min(var1),max(var1),length=100)
            lines( xg, predict( fit, xg),col=couleur[i])
        }
    }

    # dessin des unités sélectionnées
    if(length(var1[obs]) != 0)
    {
        if (symbol == 1)    
        {
            points(var1[obs], var2[obs], col="red", pch=10, cex=1.5)
        }
        else
        {
            points(var1[obs], var2[obs], col="red", pch=16)
        }
    }


  }

####################################################
# Dessin d'un boxplot
####################################################

if (graph == "Boxplot")
{
    # dessin du boxplot
    r <- boxplot(var1, boxwex=0.5, xlab=labvar)
    mat <- r$stats
    out <- r$out
    quit1 <- FALSE
    quit2 <- FALSE
    quit3 <- FALSE
    quit4 <- FALSE

    # dessin des unités sélectionnées
    if (length(var1[obs])!=0)
    {
        for (j in 1:length(var1[obs]))
        {
            # si la sélection appartient aux intervalles compris entre les moustaches
            if ((!quit1) && (var1[obs][j] >= mat[1,1]) && (var1[obs][j] < mat[2,1]))
            {
                if (symbol == 1)
                {
                    rect(0.92,mat[1,1],1.08,mat[2,1], col="red", density=4, angle=45, lwd=1)
                }
                else
                {
                    rect(0.92,mat[1,1],1.08,mat[2,1], col="red")
                }
                quit1 <- !quit1    
            }

            if ((!quit2) && (var1[obs][j] >= mat[2,1]) && (var1[obs][j] < mat[3,1]))
            {
                if (symbol == 1)
                {
                    rect(0.875,mat[2,1],1.125,mat[3,1], col="red", density=4, angle=45, lwd=1)
                }
                else
                {
                    rect(0.875,mat[2,1],1.125,mat[3,1], col="red")
                }
                quit2 <- !quit2
            }

            if ((!quit3) && (var1[obs][j] >= mat[3,1]) && (var1[obs][j] < mat[4,1]))
            {
                if (symbol == 1)
                {
                    rect(0.875,mat[3,1],1.125,mat[4,1], col="red", density=4, angle=45, lwd=1)
                }
                else
                {
                    rect(0.875,mat[3,1],1.125,mat[4,1], col="red")
                }
                quit3 <- !quit3
            }
    
            if ((!quit4) && (var1[obs][j] >= mat[4,1]) && (var1[obs][j] <= mat[5,1]))
            {
                if (symbol == 1)
                {
                    rect(0.92,mat[4,1],1.08,mat[5,1], col="red", density=4, angle=45, lwd=1)
                }
                else
                {
                    rect(0.92,mat[4,1],1.08,mat[5,1], col="red")
                }
                quit4 <- !quit4
            }

            # si la sélection appartient aux valeurs absurdes
         if(length(as.vector(out))>0) 
          {    
            if ((var1[obs][j] >= mat[5,1]) && (var1[obs][j] <= max(out)))
            {
                if (symbol == 1)
                {
                    points(1, var1[obs][j], col="red", pch=10, cex=1.5)
                }
                else
                {
                    points(1, var1[obs][j], col="red", pch=16)
                }
            }
    
            if ((var1[obs][j] < mat[1,1]) && (var1[obs][j] >= min(out)))
            {
                if (symbol == 1)
                {
                    points(1, var1[obs][j], col="red", pch=10, cex=1.5)
                }
                else
                {
                    points(1, var1[obs][j], col="red", pch=16)
                }
            }
          }
        }
    }
  }

####################################################
# Dessin d'un polyboxplot
####################################################

if (graph == "Polyboxplot")
{

if(length(names(table(var2)))<5)
 {couleurs <- c('gold','orchid1','coral','chartreuse3')
 }
 else
 {
 couleurs <- terrain.colors(length(names(table(var2)))+1)
 }

 

    # dessin du polyboxplot
    x <- factor(var2)
   if(col==1)
    {r <- boxplot(var1 ~ x, boxwex=0.8, xlab=labvar,col=couleurs,names=labmod)
     r <- boxplot(var1 ~ x, boxwex=0.8, xlab=labvar,col=couleurs,plot=FALSE)}
   else
    {r <- boxplot(var1 ~ x, boxwex=0.8, xlab=labvar,names=labmod)
     r <- boxplot(var1 ~ x, boxwex=0.8, xlab=labvar,plot=FALSE)}

    mat <- r$stats
    out <- r$out
    quit1 <- FALSE
    quit2 <- FALSE
    quit3 <- FALSE
    quit4 <- FALSE

    # dessin des unités sélectionnées
    if (length(var1[obs])!=0)
    {
        quit <- FALSE;
        k <- 1;
        for (k in 1:length(r$n))
        {
            for (j in 1:length(var1[obs]))
            {
                if (as.character(var2[obs][j]) == as.character(r$names[k]))
                {
                    if ((var1[obs][j] >= mat[1,k]) && (var1[obs][j] < mat[2,k]))
                    {
                        # si la sélection appartient aux intervalles compris entre les moustaches
                        if ((symbol == 1))
                        {  
                          rect(k-0.2,mat[1,k],k+0.2,mat[2,k], col="red", density=4, angle=45, lwd=1)
                          
                        }
                        else
                        {
                            rect(k-0.2,mat[1,k],k+0.2,mat[2,k], col="red")
                        }
                    }
    
                    if ((var1[obs][j] >= mat[2,k]) && (var1[obs][j] < mat[3,k]))
                    {
                        if ((symbol == 1))
                        {
                            rect(k-0.4,mat[2,k],k+0.4,mat[3,k], col="red", density=4, angle=45, lwd=1)
                        }
                        else
                        {
                            rect(k-0.4,mat[2,k],k+0.4,mat[3,k], col="red")
                        }
                    }
        
                    if ((var1[obs][j] >= mat[3,k]) && (var1[obs][j] < mat[4,k]))
                    {
                        if ((symbol == 1))    
                        {
                            rect(k-0.4,mat[3,k],k+0.4,mat[4,k], col="red", density=4, angle=45, lwd=1)
                        }
                        else
                        {
                            rect(k-0.4,mat[3,k],k+0.4,mat[4,k], col="red")
                        }
                    }
            
                    if ((var1[obs][j] >= mat[4,k]) && (var1[obs][j] <= mat[5,k]))
                    {
                        if ((symbol == 1))
                        {
                            rect(k-0.2,mat[4,k],k+0.2,mat[5,k], col="red", density=4, angle=45, lwd=1)
                        }
                        else
                        {
                            rect(k-0.2,mat[4,k],k+0.2,mat[5,k], col="red")
                        }
                    }

                     # si la sélection appartient aux valeurs absurdes
               if(length(as.vector(out))>0) 
                  {  
                    if ((var1[obs][j] >= mat[5,k]) && (var1[obs][j] <= max(out))&&(as.character(var2[obs][j]) == as.character(r$names[k])))
                    {
                        if ((symbol == 1))
                        {
                            points(k, var1[obs][j], col="red", pch=10, cex=1.5)
                        }
                        else
                        {
                            points(k, var1[obs][j], col="red", pch=16)
                        }

                    }
                
                    if ((var1[obs][j] < mat[1,k]) && (var1[obs][j] >= min(out)))
                    {
                        if ((symbol==1))
                        {
                            points(k, var1[obs][j], col="red", pch=10, cex=1.5)
                        }
                        else
                        {
                            points(k, var1[obs][j], col="red", pch=16)
                        }
                    }
                   }
                }
            }
            k <- k+1
        }   
    }
  }   


####################################################
# Dessin de deux estimateurs du noyau sur le même graphique
####################################################

if (graph=="Densityplot1")
{
    ypartsup <- 0;
    h <- (alpha1/100)*(max(var1)-min(var1))/2

    # dessin de l'estimateur de tous les points
    dens <- bkde(var1, kernel=kernel, bandwidth=h, gridsize=100)
    ysup <- max(dens$y)+0.1*max(dens$y)

    # dessin des unités sélectionnées
    if(length(var1[obs]) != 0)
    {
        h2 <- (alpha1/100)*(max(var1[obs])-min(var1[obs]))/2

        # dessin de l'estimateur des points sélectionnés
        denspart <- bkde(var1[obs], kernel=kernel, bandwidth=h2, gridsize=100)
        ypartsup <- max(denspart$y)+0.1*max(denspart$y)
    }

    plot(dens, type='l', col="blue", ylim=c(0,max(ysup,ypartsup)), xlab=labvar, ylab="")
    if(length(var1[obs]) != 0)
    {
        points(denspart, type='l', col="red")
    }
  }

####################################################
# Dessin d'une aire sous la courbe d'un estimateur du noyau
####################################################

if (graph == "Densityplot2")
{
    # dessin de l'estimateur du noyau
    h <- (alpha1/100)*(max(var1)-min(var1))/2
    aire=0
    dens <- bkde(var1, kernel=kernel, bandwidth=h, gridsize=100)
        
    plot(dens, type='l', col="blue", ylim=c(0,max(dens$y)+0.1*max(dens$y)), xlab=labvar, ylab="")

    # délimitation de l'aire
    i <- 1;
    while ((Xpoly > dens$x[i]) && (i<99))
    {
        i <- i+1;
    }   
    i1 <- i;

    i <- 1;
    while ((Ypoly > dens$x[i]) && (i<99))
    {
        i <- i+1;
    }   
    i2 <- i;

    # dessin de l'aire sous la courbe et de l'aire sous la courbe
    for (i in i1:i2)
    {
        polygon(c(dens$x[i],dens$x[i],dens$x[i+1],dens$x[i+1]),c(0,dens$y[i],dens$y[i+1],0), col="red", border=0)
   
        aire=aire+(dens$x[i+1]-dens$x[i])*dens$y[i]+(dens$y[i+1]-dens$y[i])*(dens$x[i+1]-dens$x[i])/2
    }
   
  }


####################################################
# Dessin d'un angleplot 
####################################################


   if (graph == "Angleplot") 
      {

   couleur <- heat.colors(length(quantiles))

   if (labvar != "") 
        {
            msg <- paste("Variable : "  , labvar, sep = "")
        }
        else 
        {
            msg <- ""
        }
        diag(var1)<-NA
        diag(var2)<-NA
        vect1 <- as.vector(var1[which(!is.na(var1))])
        vect2 <- as.vector(var2[which(!is.na(var2))])
        vect <- cbind(vect1, vect2)
        res <- sort(vect[, 1], index.return = TRUE)
        x <- res$x
        y <- vect[res$ix, 2]
        z <- seq(1, length(x), by = (length(x)/1000))
        z <- round(z)


  if(length(quantiles)!=0)
{fitmax <- qsreg(vect[z, 1], vect[z, 2],lam=alpha1,alpha = max(quantiles))}
    

xg <- seq(min(vect[z, 1]), max(vect[z, 1]), length = 100)


 if(length(quantiles)!=0)
{ plot(vect1,vect2,"n",xlab="angle",ylab="absolut magnitude",xlim=c(0,max(vect1)))
       points(vect1[which(vect2>predict(fitmax,vect1))], vect2[which(vect2>predict(fitmax,vect1))],col = "blue", pch = 16, cex = 0.5)   
        lines(xg, predict(fitmax, xg), col = couleur[1])

      if(length(quantiles)>1)
          {  for (i in 1:(length(quantiles)-1)) 
            {
               fit <- qsreg(vect[z, 1], vect[z, 2],lam=alpha1,alpha = quantiles[i])
                lines(xg, predict(fit, xg), col = couleur[i+1])
            }
          }
}
else
{
plot(vect1,vect2,"n",xlab="angle",ylab="absolut magnitude",xlim=c(0,max(vect1)))
points(vect1, vect2,col = "blue", pch = 16, cex = 0.5)   
}

        if (length(var1[obs]) != 0) {
            if (symbol == 1) {
                points(var1[obs], var2[obs], col = "red", pch = 10,cex = 1.5)
            }
            else {
                points(var1[obs], var2[obs], col = "red", pch = 16)
            }
        }
    }


####################################################
# Dessin d'un variocloud
####################################################

if (graph == "Variocloud")
{
    # initialisation
    
      couleur <- heat.colors(length(quantiles))
    if (labvar!="")
    {
        msg <- paste("Studied variable : ",labvar,sep="")
    }
    else
    {
        msg <- ""
    }
    
    diag(var1)<-NA
    diag(var2)<-NA
    diag(var3)<-NA

    vect1 <- as.vector(var1[which(!is.na(var1))])
    vect2 <- as.vector(var2[which(!is.na(var2))])
    vect3 <- as.vector(var3[which(!is.na(var2))])

    vect1 <- vect1[which(!(vect1==-10))]
    vect2 <- vect2[which(!(vect2==-10))]
    vect3 <- vect3[which(!(vect3==-10))]
    
    # dessin des estimateurs de la médiane ou des quantiles
    vect <- cbind(vect1,vect2,vect3)
    res <- sort(vect[,1],index.return=TRUE)
    x <- res$x;
    y <- vect[res$ix,2];
    y2 <- vect[res$ix,3];

    z <- seq(1,length(x),by=(length(x)/1000))
    z <- round(z)


 if(length(quantiles)!=0)
{fitmax <- qsreg(vect[z, 1], vect[z, 2],lam=alpha1,alpha = max(quantiles))}
      

etendue<-max(vect[z, 1])-min(vect[z, 1]);
n<-length(x);
d<-max(x[2:length(x)]-x[1:(length(x)-1)]);
xg <- seq(min(vect[z, 1]), max(vect[z, 1]), length = 100)


if(length(bin)==0)
{
xg2 <- seq(min(vect1), max(vect1), length = 1/(max(c(30/n,0.08,d/etendue) )))
}
else
{
xg2 <- sort(bin)
}

   xg3<-matrix(0,ncol=(length(xg2)-1))
   for(i in 1:(length(xg2)-1))
       { xg3[i]<-(xg2[i]+xg2[i+1])/2 }

 if(length(quantiles)!=0)
{ plot(vect1,vect2,"n",xlab="Distance",ylab="semivariance",xlim=c(0,max(vect1)));
       points(vect1[which(vect2>predict(fitmax,vect1))], vect2[which(vect2>predict(fitmax,vect1))],col = "blue", pch = 16, cex = 0.5)   
        lines(xg, predict(fitmax, xg), col = couleur[1])

      if(length(quantiles)>1)
          {  for (i in 1:(length(quantiles)-1)) 
            {
               fit <- qsreg(vect[z, 1], vect[z, 2],lam=alpha1,alpha = quantiles[i])
                lines(xg, predict(fit, xg), col = couleur[i+1])
            }
          }
}
else
{
plot(vect1,vect2,"n",xlab="Distance",ylab="semivariance",xlim=c(0,max(vect1)))
points(vect1, vect2,col = "blue", pch = 16, cex = 0.5)   
}

#################################################################
# variogramme empirique
#################################################################
       if(opt1==2)
{   
     if(length(bin)==0)
       {dis=matrix(0,ncol=(length(xg2)-1))
         j=min(which(x>=0))
          for(i in 1:(length(xg2)-1))
           {Nh=0
             while((x[j]<=xg2[i+1])&&(j<=length(vect2)))
              {dis[i]=dis[i]+y[j]
               Nh=Nh+1
               j=j+1  
              }
            dis[i]<-dis[i]/Nh
           } 
        points(xg3,dis, col = 'red',pch=1)
        lines(xg3,dis, col = 'red',pch=1)
       }
     else
       {
         dis=matrix(0,ncol=(length(xg2)))
         j=1
          for(i in 1:(length(xg2)))
           {Nh=0
             while((x[j]<=xg2[i])&&(j<=length(vect2)))
              {dis[i]=dis[i]+y[j]
               Nh=Nh+1
               j=j+1  
              }
            dis[i]<-dis[i]/Nh
           } 

        dis<<-dis
        points(xg2,dis, col = 'red',pch=1)
        lines(xg2,dis, col = 'red',pch=1)
       }

}
       


#################################################################
# variogramme empirique robuste
#################################################################
 if(opt2==2)
{
        {
     if(length(bin)==0)
       {dis2=matrix(0,ncol=(length(xg2)-1))
         j=1
          for(i in 1:(length(xg2)-1))
           {Nh=0
             while((x[j]<=xg2[i+1])&&(j<=length(vect2)))
              {dis2[i]=dis2[i]+y2[j]
               Nh=Nh+1
               j=j+1  
              }
             dis2[i]=(dis2[i]/Nh)^4/(0.457+0.494/Nh)/2
           } 
        points(xg3,dis2, col = 'red',pch=1)
        lines(xg3,dis2, col = 'red',pch=1,lty=2)
       }
     else
       {
         dis2=matrix(0,ncol=(length(xg2)))
         j=1
          for(i in 1:(length(xg2)))
           {Nh=0
             while((x[j]<=xg2[i])&&(j<=length(vect2)))
              {dis2[i]=dis2[i]+y2[j]
               Nh=Nh+1
               j=j+1  
              }
            dis2[i]=(dis2[i]/Nh)^4/(0.457+0.494/Nh)/2
           } 


        points(xg2,dis2, col = 'red',pch=1)
        lines(xg2,dis2, col = 'red',pch=1,lty=2)
       }

       }
       }

#################################################################

    # dessin des unités sélectionnées
    if(length(var1[obs])!=0)
    {
        if (symbol == 1)
        {
            points(var1[obs], var2[obs], col="red", pch=10, cex=1.5)
        }
        else
        {
            points(var1[obs], var2[obs], col="red", pch=16)
        }
    }

  }

####################################################
# Dessin d'une courbe de Lorentz à partir d'un point
####################################################

    if (graph == "Lorentz") {

  G<-G[cumsum(as.data.frame(table(F))$Freq)]
  F<-F[cumsum(as.data.frame(table(F))$Freq)]

  
var1u=unique(var1)
var1u<-c(0,var1u) 
plot(F, G, "n", col = "red")
segments(0, 0, 1, 1, col = "blue")
lines(F,G,col = "red")


        if (length(var1[obs])!=0) 
        {

imax=which.min(F<=Xpoly)
imax=imax+1

vsort <- sort(var1u)
vsort<-c(0,vsort)



 segments(F[imax-1], 0, F[imax-1], G[imax-1], col = "green")
segments(F[imax-1], G[imax-1], 0, G[imax-1], col = "green")

if(F[2]>G[2])
{
            msg <- paste("f = ",round(F[imax-1], 2))
            text(0.15, 0.98, msg, cex = 0.8)

            msg <- paste("g = ",round(G[imax-1], 2))
            text(0.15, 0.94, msg, cex = 0.8)

            msg <- paste("expectile = ", round(var1u[which(var1u == vsort[imax])],2))
            text(0.15, 0.90, msg, cex = 0.8)
}
else
{
            msg <- paste("f = ",round(F[imax-1], 2))
            text(0.8, 0.12, msg, cex = 0.8)

            msg <- paste("g = ",round(G[imax-1], 2))
            text(0.8, 0.08, msg, cex = 0.8)

            msg <- paste("expectile = ", round(var1u[which(var1u == vsort[imax])],2))
            text(0.8, 0.04, msg, cex = 0.8)

}

        }
    }


####################################################
# Dessin d'une courbe de Lorentz à partir d'une valeur
####################################################

    if (graph == "VLorentz") {

  G<-G[cumsum(as.data.frame(table(F))$Freq)]
  F<-F[cumsum(as.data.frame(table(F))$Freq)]


var1u=unique(var1)
var1u<-c(0,var1u)    
var1u<-sort(var1u)

        plot(F, G, "n", col = "red")
        segments(0, 0, 1, 1, col = "blue")
        lines(F,G,col = "red")


        if (length(var1[obs]) != 0) 
        {    imax=which.min(var1u<=Xpoly)
            segments(F[imax-1], 0, F[imax-1], G[imax-1], col = "green")
            segments(F[imax-1], G[imax-1], 0, G[imax-1], col = "green")


if(F[2]>G[2])
{
            msg <- paste("f = ",round(F[imax-1], 2))
            text(0.15, 0.98, msg, cex = 0.8)

            msg <- paste("g = ",round(G[imax-1], 2))
            text(0.15, 0.94, msg, cex = 0.8)

            msg <- paste("expectile = ", Xpoly)
            text(0.15, 0.90, msg, cex = 0.8)

}
else
{
            msg <- paste("f = ",round(F[imax-1], 2))
            text(0.8, 0.12, msg, cex = 0.8)

            msg <- paste("g = ",round(G[imax-1], 2))
            text(0.8, 0.08, msg, cex = 0.8)

            msg <- paste("expectile = ", Xpoly)
            text(0.8, 0.04, msg, cex = 0.8)

}

        }
    }
####################################################
# Dessin d'un Moran Scatterplot
####################################################

if (graph == "Moran")
{
    # dessin du scatterplot
    if (labvar != "")
    {
        msg1 <- labvar;
        msg2 <- paste("W * ",labvar,sep="")
    }
    else
    {
        msg1 <- "Var"
        msg2 <- "W * var"
    }
    plot(var1,var2,"n",xlab=msg1,ylab=msg2);
    segments(min(var1),0,max(var1),0,col="black")
    segments(0,min(var2),0,max(var2),col="black")
    
    points(var1[!obs],var2[!obs],col="blue",pch=16)

      if(opt1==2)
      {xg <- seq(min(var1), max(var1), length = 100)
       reg<-lm(var2~var1)
       lines(xg,reg$coefficients[1]+reg$coefficients[2]*xg)}

    # dessin des unités sélectionnées
    if(length(var1[obs])!=0)
    {
        if (symbol==1)
        {
            points(var1[obs], var2[obs], col="red", pch=10, cex=1.5)
        }
        else
        {
            points(var1[obs], var2[obs], col="red", pch=16)
        }

        ilocal <- (var1[obs]/sd(var1))*(var2[obs]/sd(var1))
        #écriture des labels de chaque point sélectionné
        if (locmoran == 1)
        {
            msg <- paste(round(ilocal,2)) 
            text(var1[obs]+0.02, var2[obs]+0.02, msg, cex=0.8)
        }
    }
  }

####################################################
# Dessin des quadrants d'un diagramme de Moran
####################################################

if (graph=="Quadrant")
{
    # initialisation



couleurs <- c('gold','orchid1','coral','chartreuse3')

symbols <- c(7,9,12,15)

    # dessin du scatterplot
    if (labvar!="")
    {
        msg1 <- labvar;
        msg2 <- paste("W * ",labvar,sep="");
    }
    else
    {
        msg1 <- "Var"
        msg2 <- "W * var"
    }
    plot(var1,var2,"n",xlab=msg1,ylab=msg2);
    segments(min(var1),0,max(var1),0,col="black")
    segments(0,min(var2),0,max(var2),col="black")

    points(var1[!obs], var2[!obs], col="blue", pch=16)


      if(opt1==2)
      {xg <- seq(min(var1), max(var1), length = 100)
       reg<-lm(var2~var1)
       lines(xg,reg$coefficients[1]+reg$coefficients[2]*xg)}

    # dessin des unités sélectionnées
    if(length(var1[obs])!=0)
    {
        if (symbol==1)
        {
            points(var1[obs],var2[obs], col=couleurs[obsq[obs]], pch=symbols[obsq[obs]], cex=1.5)
        }
        else
        {
            points(var1[obs], var2[obs], col=couleurs[obsq[obs]], pch=16)
        }
    }
  }

####################################################
# Dessin d'un scatterplot des plus proches voisins
####################################################


if (graph == "Neighbourplot")
{
    # dessin du scatterplot
    plot(var1,var1,"n",xlab=labvar[1],ylab=labvar[2])

      if(opt1==1)
      {
       lines(c(0,max(var1)),c(0,max(var1)))
      }


    indt <- which(obs == TRUE, arr.ind = TRUE)
    indf <- which(obs == FALSE, arr.ind = TRUE)

if(length(indf)>0)
{
    for (i in 1:nrow(indf))
    {
        if (W[indf[i,1],indf[i,2]] != 0)
        {
            points(var1[indf[i,1]],var1[indf[i,2]],col="blue",pch=16)
        }
    }
}

    # dessin des unités sélectionnées
    if(length(var1[obs])!=0)
    {
        if (symbol==1)
        {
            for (i in 1:nrow(indt))
            {
                if (W[indt[i,1],indt[i,2]]!=0)
                {
                    points(var1[indt[i,1]],var1[indt[i,2]],col="red", pch=10, cex=1.5)
                }
            }
        }
        else
        {
            for (i in 1:nrow(indt))
            {
                if (W[indt[i,1],indt[i,2]] !=0)
                {
                    points(var1[indt[i,1]],var1[indt[i,2]],col="red", pch=16)
                }
            }
        }
    }
}
####################################################
# Dessin d'un scatterplot des individus de l'ACP
####################################################

if (graph=="Acp1")
{
    # dessin du scatterplot
    msg1 <- paste("component ",as.character(direct[1])," : ",round(inertie[direct[1]],0),"%",sep="")
    msg2 <- paste("component ",as.character(direct[2])," : ",round(inertie[direct[2]],0),"%",sep="")
    plot(var1,var2,"n",xlab=msg1,ylab=msg2);
    points(var1,var2,col="blue",pch=16,cex=0.5); 
    segments(min(var1),0,max(var1),0,col="black");
    segments(0,min(var2),0,max(var2),col="black");
    
    # dessin des unités sélectionnées
    if(length(var1[obs])!=0)
    {
        if (symbol==1)
        {
            points(var1[obs], var2[obs], col="red", pch=10, cex=1.5)
        }
        else
        {
            points(var1[obs], var2[obs], col="red", pch=16)
        }
        #écriture des labels de chaque point sélectionné
        if (label == 1)
        {
            msg <- paste(round(labmod,0),"%",sep="") 
            text(var1[obs]+0.02, var2[obs]+0.02, msg[obs], cex=0.8)
        }
    }
}

####################################################
# Graphique des variables de l'ACP
####################################################

if (graph=="Acp2")
{
    # dessin des vecteurs des variables
    centre <- rep(0,length(var1))
    msg1 <- paste("component ",as.character(direct[1])," : ",round(inertie[direct[1]],0),"%",sep="")
    msg2 <- paste("component ",as.character(direct[2])," : ",round(inertie[direct[2]],0),"%",sep="")

if((max(abs(var1))>1)||(max(abs(var2))>1))
{  
  plot(var1,var2,"n",xlab=msg1,ylab=msg2);   
}
else
{
    plot(-1:1,-1:1,"n",xlab=msg1,ylab=msg2);
    segments(-1,0,1,0,col="black")
    segments(0,-1,0,1,col="black")
}
    segments(centre,centre,var1,var2,col="red");

    #écriture des labels de chaque variable
    msg <- paste(labvar," : ",round(labmod,0),"%",sep="") 
    text(var1+0.02, var2+0.02, msg, cex=0.8)
    x=seq(-1,1,0.02)
    y=sqrt(1-x^2)
    lines(x,y)
    lines(x,-y)

}


  }


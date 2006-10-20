"selectstat" <-
function(var1, var2,obs,Xpoly,Ypoly,method,nbcol, W, F, long, lat)
{

if (method == "Histogram")
{
    #initialisation
    mnv <- min(var1);
    mxv <- max(var1);
    h <- (mxv - mnv)/(nbcol);
    quit <- FALSE;
    i=0;

    # si le point sélectionné est contenu dans une des barres de l'histogramme
    # alors la valeur logique des observations de cette barre est inversée 
    while ((i < nbcol) && (!quit))
    {   
        x1 <- mnv + h*i;
        x2 <- mnv + h*(i+1);


        if ((Xpoly >= x1) && (Xpoly < x2))
        {
            cnt <- 0;
         if(x1==mnv)
          {
            for (j in 1:length(var1))
            {
                if ((var1[j] >= x1) && (var1[j] <= x2))
                {   
                    cnt <- cnt+1;
                }
            }

            if (Ypoly <= cnt)
            {
                for (j in 1:length(var1))
                {
                    if ((var1[j] >= x1) && (var1[j] <= x2))
                    {   
                        obs[j] <- !obs[j];
                    }
                }

                quit <- TRUE;
            }
          }
         else 
         {
            for (j in 1:length(var1))
            {
                if ((var1[j] > x1) && (var1[j] <= x2))
                {   
                    cnt <- cnt+1;
                }
            }

            if (Ypoly <= cnt)
            {
                for (j in 1:length(var1))
                {
                    if ((var1[j] > x1) && (var1[j] <= x2))
                    {   
                        obs[j] <- !obs[j];
                    }
                }

                quit <- TRUE;
            }
          }
        }
        i <- i+1;
    }
    return(obs);
  }

####################################################
# sélection d'une barre d'un diagramme en barres
####################################################

if (method == "Barplot")
{
    r <- table(var1);
    g <- barplot(r, xlim=c(0,length(r)), width=0.8, plot=FALSE);
    noms <- names(r);
    i <- 1;
    quit <- FALSE;

    # si le point sélectionné est contenu dans une des barres du diagramme en barres
    # alors la valeur logique des observations de cette barre est inversée  
    while ((i <= length(g)) && (!quit))
    {   
        x1 <- g[i] - 0.4;
        x2 <- g[i] + 0.4;

        if ((Xpoly >= x1) && (Xpoly < x2))
        {
            if (Ypoly <= as.numeric(r[[i]]))
            {
                for (j in 1:length(var1))
                {
                    if (var1[j] == noms[i])
                    {   
                        obs[j] <- !obs[j];
                    }
                }
                quit <- TRUE;
            }
        }
        i <- i+1;
    }
    return(obs);
  }

####################################################
# sélection d'une partie du boxplot
####################################################

if (method == "Boxplot")
{
    r <- boxplot(var1, plot=FALSE);
    mat <- r$stats;
    out <- r$out;
    quit <- FALSE;
    i <- 1;
 #   seuil=sort(var1)
 #   seui1=seuil[1:(length(seuil)-1)]
  #  seuil2=seuil[2:length(seuil)]
  #  diff=min(abs(seuil2-seuil1))
    

    # si le point sélectionné est contenu dans une des parties du boxplot
    # alors la valeur logique des observations de cette partie est inversée     
    if ((Xpoly >= 0.875) && (Xpoly <= 1.125))
    {
        while ((i < 5) && (!quit))
        {
            if ((Ypoly >= mat[i,1]) && (Ypoly <= mat[i+1,1]))
            {
                for (j in 1:length(var1))
                {
                  if (i == 4)
                   {
                    if ((var1[j] >= mat[i,1]) && (var1[j] <= mat[i+1,1]))
                    {   
                        obs[j] <- !obs[j];
                    }
                   }
                  else
                   {
                    if ((var1[j] >= mat[i,1]) && (var1[j] < mat[i+1,1]))
                    {   
                        obs[j] <- !obs[j];
                    }
                   }
                }
                quit <- !quit;
            }
            i <- i+1;
        }
    }

    # si le point sélectionné est une valeur absurde,
    # alors la valeur logique de cette observation est inversée   
   if(length(as.vector(out))>0)  
    {   out<-unique(out)
     for (j in 1:length(out))
      {diff1=abs(var1-out[j])
           l<-1
           while(sort((diff1)/2)[l]==0)
           {l=l+1}
           diff=sort((diff1)/2)[l]  
        #diff <- sqrt( (1 - as.numeric(Xpoly))^2 + (out[j] - as.numeric(Ypoly))^2);
        if (abs(as.numeric(Ypoly)-out[j])<diff)
        {
            for (k in 1:length(var1))
            {
                if (var1[k] == out[j])
                {
                    obs[k] <- !obs[k];
                }
            }
        }
    }
    }
    return(obs);
  }


####################################################
# sélection d'une partie du polyboxplot
####################################################

if (method == "Polyboxplot")
{
#    x <- factor(var2);
    r <- boxplot(var1 ~ factor(var2), plot=FALSE);
    mat <- r$stats;
    out <- r$out;
    quit <- FALSE;
    k <- 1;

    # si le point sélectionné est contenu dans une des parties du polyboxplot
    # alors la valeur logique des observations de cette partie est inversée     
    while ((k <= length(r$n)) && (!quit))
    {   
        if ((Xpoly >= k-0.4) && (Xpoly <= k+0.4))
        {
            i <- 1;
            while ((i < 5) && (!quit))
            {
                if ((Ypoly >= mat[i,k]) && (Ypoly <= mat[i+1,k]))
                {
                    for (j in 1:length(var1))
                    {
                        if (as.character(var2[j]) == as.character(r$names[k]))
                        {
                            if (i == 4)
                            {
                                if ((var1[j] >= mat[i,k]) && (var1[j] <= mat[i+1,k]))
                                {   
                                    obs[j] <- !obs[j];
                                }
                            }
                            else 
                            {
                                if ((var1[j] >= mat[i,k]) && (var1[j] < mat[i+1,k]))
                                {   
                                    obs[j] <- !obs[j];
                                }
                            }   
                        }
                        quit <- TRUE;
                    }   

                }
                i <- i+1;
            }
        }
        k <- k+1;
    }

    # si le point sélectionné est contenu une valeur absurde du polyboxplot
    # alors la valeur logique de cette observation est inversée  
  if(length(as.vector(out))>0)    
   {out<-unique(out)
    for (j in 1:length(out))
    {
        for (k in 1:length(r$n))
        {  diff1=abs(var1[which((var2== r$names[k]))]-out[j])
           diff=sort((diff1)/2)[2]  
           # if(length(out)==1)
           # {seuil=out}
           # else
           # {seuil=(max(out)-min(out))/500}
           # diff <- sqrt( (k - as.numeric(Xpoly))^2 + (out[j] - as.numeric(Ypoly))^2);

            if ((abs(as.numeric(Ypoly)-out[j])<diff)&&((k - as.numeric(Xpoly))^2<0.25))
            {
                for (i in 1:length(var1))
                {
                    if (var1[i] == out[j])
                    {
                        if (var2[i] == r$names[k])
                        {
                            obs[i] <- !obs[i];
                        }
                    }
                }
            }
        }
    }
   }
   return(obs);
  }

####################################################
# sélection d'une aire sous la courbe de densité
####################################################

if (method == "Densityplot")
{
    if (as.numeric(Xpoly) < as.numeric(Ypoly))
    {
        a <- Xpoly;
        b <- Ypoly;
    }
    else
    {
        a <- Ypoly;
        b <- Xpoly;
    }
        
    # si les observations sont contenues dans l'intervalle
    # alors leurs valeurs logiques sont inversées   
    for (i in 1:length(var1))
    {
        if ((var1[i] >= a) && (var1[i] <= b))
        {

            obs[i] <- !obs[i];
        }
    }

    aire=sum(as.numeric(obs))/length(obs)
    msg <- paste(round(aire*100,2),"% of observations are included between", round(as.numeric(a),1), " and ", round(as.numeric(b),1))

    tkmessageBox(message=msg,icon="info",type="ok") 

    return(obs);
  }

####################################################
# sélection d'un point du neighbourplot
####################################################

if (method == "Neighbourplot")
{
    v1 <- matrix(rep(t(var1),length(var1)),ncol=dim(t(var1))[2],byrow=FALSE);
    v2 <- matrix(rep(t(var1),length(var1)),ncol=dim(t(var1))[2],byrow=TRUE);
    Xuns <- matrix(as.numeric(Xpoly),length(var1),length(var1));
    Yuns <- matrix(as.numeric(Ypoly),length(var1),length(var1));



  diff<-abs(v1 - Xuns) + abs(v2 - Yuns);

  #  # si le point sélectionné est très proche d'un point existant
   # # alors la valeur logique l'observation est inversée   
    if(min(diff[diff==min(diff[which(W != 0)])]/((max(var1) - min(var1))))<0.01)
    {
        obs[diff==min(diff[which(W != 0)])] <- !obs[diff==min(diff[which(W != 0)])]; 
  #    obs[(diff==min(diff[which(W != 0)]))] <- !obs[(diff==min(diff[which(W != 0)]))&&(W==1)]; 
        obs[which(W==0,arr.ind=TRUE)]<-FALSE

    }
    
 return(obs);



  #dirt<-(v1-Xuns)^2+(v2-Yuns)^2

#which(dirt==min(dirt),arr.ind=TRUE)


  #seuil<-min(dist[which(dist!=0)])/2

  #dist<-(v1-Xuns)^2+(v2-Yuns)^2

  # diff<-abs(v1 - as.numeric(Xuns)) * (max(v2) - min(v2)) + abs(v2 - as.numeric(Yuns)) * (max(v1) - min(v1));

  #  diff<-abs(v1 - Xuns) + abs(v2 - Yuns);

    # si le point sélectionné est très proche d'un point existant
    # alors la valeur logique l'observation est inversée   
   # if(min(dist[dist==min(dist[which(W == 1)])])<10)
   # {
    #    obs[min(dist==min(dist[which(W == 1)]))] <- !obs[min(dist==min(dist[which(W == 1)]))]; 
    #}

#return(obs);

  }


####################################################
# sélection d'un intervalle [0,Xpoly] de la courbe de Lorentz
####################################################

if (method == "Lorentz")
{
    # si les observations sont contenues dans l'intervalle
    # alors leurs valeurs logiques sont inversées  

 obs<-vector(mode = "logical", length = length(var1))

#  G<-G[cumsum(as.data.frame(table(F))$Freq)]
  F<-F[cumsum(as.data.frame(table(F))$Freq)]

var1u=unique(var1)
var1u<-c(0,var1u) 
vsort <- sort(var1u);

  #  vsort<-as.numeric(as.character(as.data.frame(table(vsort))$vsort))


  #if((Xpoly<=1)&&(Xpoly>=0))
 
 #  while((as.numeric(F[j])<as.numeric(Xpoly))&&(j<length(var1)))
 #  while((as.numeric(F[j])<as.numeric(Xpoly))&&(j<length(var1)))
 
#  while((as.numeric(F[j])<as.numeric(Xpoly)))  
 #   {j=j+1}
 

  j=which.min(F<=Xpoly)


val=vsort[j]

 
 if(j>2)
 {
  obs[which(var1 <= val)]<- !obs[which(var1 <= val)]
 }
else
 {  
   obs[which(var1 < val)]<- !obs[which(var1 < val)]
 }

    return(obs);
  }

####################################################
# sélection d'un point sur l'angle plot
####################################################

if (method == "AnglePoint")
{
    Xuns <- matrix(as.numeric(Xpoly),length(long), length(long));
    Yuns <- matrix(as.numeric(Ypoly),length(lat), length(lat))

    diff<-abs(var1 - Xuns) * (max(var2) - min(var2)) + abs(var2 - Yuns) * (max(var1) - min(var1));

    # si le point sélectionné est très proche d'un point existant
    # alors la valeur logique l'observation est inversée   
    if(min(diff[diff==min(diff)]/((max(var2)-min(var2)) * (max(var1) - min(var1))))<0.01)
    {
            obs[diff==min(diff)] <- !obs[diff==min(diff)];  
    }
    return(obs);
  }

####################################################
# sélection d'un point sur le variocloud map
####################################################

if (method == "Variopoint")
{
    Xuns <- matrix(as.numeric(Xpoly),length(long), length(long));
    Yuns <- matrix(as.numeric(Ypoly),length(lat), length(lat));

    diff<-abs(var1 - Xuns) * (max(var2) - min(var2)) + abs(var2 - Yuns) * (max(var1) - min(var1));

    # si le point sélectionné est très proche d'un point existant
    # alors la valeur logique des l'observation est inversée   
    if(min(diff[diff==min(diff)]/((max(var2)-min(var2)) * (max(var1) - min(var1))))<0.01)
    {
        obs[which(diff == min(diff))] <- !obs[which(diff == min(diff))];    
    }
    return(obs);
  }

  }


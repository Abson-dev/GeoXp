`carte` <-
function (long, lat, obs,criteria=NULL,buble=FALSE,cbuble=NULL,nointer=FALSE, carte=NULL,nocart=FALSE, label="",symbol=0,col=0, lablong="", lablat="", method="", W=NULL,couleurs="", classe=NULL, obsq,legmap=NULL,legends=list(FALSE,FALSE),labmod="",axis=FALSE)
{

####################################################
# définition des paramètres graphiques
####################################################

dev.set(2);

if(method == "Quadrant")
  {classe=obsq;
   col=1}


if(!axis)
{par(pty="s",mar=c(0.4,0.4,0.4,0.4))}

if(buble)
 {if(axis)
  {plot(long,lat,"n", xlab=lablong, ylab=lablat, tcl=-.25, las=1, cex=cbuble, xlim=c(min(long)-(max(long)-min(long))/10, max(long)+(max(long)-min(long))/10), ylim=c(min(lat)-(max(lat)-min(lat))/10, max(lat)+(max(lat)-min(lat))/10))}
  else
  {plot(long,lat,"n", xlab=lablong, ylab=lablat,xaxt="n",yaxt="n",bty="n", tcl=-.25, las=1, cex=cbuble, xlim=c(min(long)-(max(long)-min(long))/10, max(long)+(max(long)-min(long))/10), ylim=c(min(lat)-(max(lat)-min(lat))/10, max(lat)+(max(lat)-min(lat))/10))}
 }
else
{if(axis)
{plot(long,lat,"n", xlab=lablong, ylab=lablat,tcl=-.25, las=1, cex=1.5, xlim=c(min(long)-(max(long)-min(long))/10, max(long)+(max(long)-min(long))/10), ylim=c(min(lat)-(max(lat)-min(lat))/10, max(lat)+(max(lat)-min(lat))/10))}
 else
{plot(long,lat,"n", xlab=lablong, ylab=lablat,xaxt="n",yaxt="n",bty="n", tcl=-.25, las=1, cex=1.5, xlim=c(min(long)-(max(long)-min(long))/10, max(long)+(max(long)-min(long))/10), ylim=c(min(lat)-(max(lat)-min(lat))/10, max(lat)+(max(lat)-min(lat))/10))}
}

if(length(couleurs)==1)
{
if(length(classe)>0)
{
if(length(names(table(classe)))<5)
 {couleurs <- c('gold','orchid1','coral','chartreuse3','chocolate2','orchid1',
        eval(cm.colors(17))[17],eval(topo.colors(17))[17],eval(terrain.colors(17))[17],eval(heat.colors(17))[9],
        eval(terrain.colors(17))[8],eval(topo.colors(17))[8])
 }
 else
 {
 couleurs <- terrain.colors(length(names(table(classe)))+1)
 }
#    couleurs <- c(eval(topo.colors(10))[5],eval(terrain.colors(10))[7],eval(topo.colors(10))[7],eval(cm.colors(10))[7])
 }
}


symbols <- c(7,9,12,15:18,25,1,2);


####################################################
# Pour les bubbles s'il y a des valeurs nulles
####################################################

if(length(cbuble)!=0)
{
if(min(cbuble)==0)
{cbuble[which(cbuble==0)]<-min(cbuble[which(cbuble!=0)])/2}

if(length(legmap)>0)
{
if(as.numeric(legmap[3])==0)
{legmap[3]<-min(cbuble)}
}

}
####################################################
# dessin des contours des unités spatiales
####################################################

if(nocart)
{
if(buble)
 {if(axis)
  {plot(carte[,1],carte[,2],"n", xlab=lablong, ylab=lablat, tcl=-.25, las=1, cex=cbuble, xlim=c(min(carte[,1],na.rm=TRUE)-(max(carte[,1],na.rm=TRUE)-min(carte[,1],na.rm=TRUE))/10, max(carte[,1],na.rm=TRUE)+(max(carte[,1],na.rm=TRUE)-min(carte[,1],na.rm=TRUE))/10), ylim=c(min(carte[,2],na.rm=TRUE)-(max(carte[,2],na.rm=TRUE)-min(carte[,2],na.rm=TRUE))/10, max(carte[,2],na.rm=TRUE)+(max(carte[,2],na.rm=TRUE)-min(carte[,2],na.rm=TRUE))/10))}
  else
  {plot(carte[,1],carte[,2],"n", xlab=lablong, ylab=lablat,xaxt="n",yaxt="n",bty="n", tcl=-.25, las=1, cex=cbuble, xlim=c(min(carte[,1],na.rm=TRUE)-(max(carte[,1],na.rm=TRUE)-min(carte[,1],na.rm=TRUE))/10, max(carte[,1],na.rm=TRUE)+(max(carte[,1],na.rm=TRUE)-min(carte[,1],na.rm=TRUE))/10), ylim=c(min(carte[,2],na.rm=TRUE)-(max(carte[,2],na.rm=TRUE)-min(carte[,2],na.rm=TRUE))/10, max(carte[,2],na.rm=TRUE)+(max(carte[,2],na.rm=TRUE)-min(carte[,2],na.rm=TRUE))/10))}
 }
else
{if(axis)
{plot(carte[,1],carte[,2],"n", xlab=lablong, ylab=lablat,tcl=-.25, las=1, cex=1.5, xlim=c(min(carte[,1],na.rm=TRUE)-(max(carte[,1],na.rm=TRUE)-min(carte[,1],na.rm=TRUE))/10, max(carte[,1],na.rm=TRUE)+(max(carte[,1],na.rm=TRUE)-min(carte[,1],na.rm=TRUE))/10), ylim=c(min(carte[,2],na.rm=TRUE)-(max(carte[,2],na.rm=TRUE)-min(carte[,2],na.rm=TRUE))/10, max(carte[,2],na.rm=TRUE)+(max(carte[,2],na.rm=TRUE)-min(carte[,2],na.rm=TRUE))/10))}
 else
{plot(carte[,1],carte[,2],"n", xlab=lablong, ylab=lablat,xaxt="n",yaxt="n",bty="n", tcl=-.25, las=1, cex=1.5, xlim=c(min(carte[,1],na.rm=TRUE)-(max(carte[,1],na.rm=TRUE)-min(carte[,1],na.rm=TRUE))/10, max(carte[,1],na.rm=TRUE)+(max(carte[,1],na.rm=TRUE)-min(carte[,1],na.rm=TRUE))/10), ylim=c(min(carte[,2],na.rm=TRUE)-(max(carte[,2],na.rm=TRUE)-min(carte[,2],na.rm=TRUE))/10, max(carte[,2],na.rm=TRUE)+(max(carte[,2],na.rm=TRUE)-min(carte[,2],na.rm=TRUE))/10))}
}

    n <- nrow(carte);
    abs1 <- carte[1:(n-1),1];
    ord1 <- carte[1:(n-1),2];
    abs2 <- carte[2:n,1];
    ord2 <- carte[2:n,2];
  #  points(long,lat,"n", xlab=lablong, ylab=lablat,xaxt="n",yaxt="n", bty="n", xlim=c(min(long)-(max(long)-min(long))/10, max(long)+(max(long)-min(long))/10), ylim=c(min(lat)-(max(lat)-min(lat))/10, max(lat)+(max(lat)-min(lat))/10));
    segments(abs1,ord1,abs2,ord2,col="black")


}

####################################################
# dessin des points
####################################################

   if (method == "Cluster")
    {
     if(col==1)  
      { 
       if (symbol==1)
          {if(buble)   
           {points(long[!obs],lat[!obs],col=couleurs[as.factor(classe)[!obs]],pch=symbols[as.factor(classe)[!obs]],cex=cbuble[!obs]) }  
           else 
           {points(long,lat,col=couleurs[as.factor(classe)],pch=symbols[as.factor(classe)]) }        
           }
       else
          {if(buble)   
           {points(long,lat,col=couleurs[as.factor(classe)],pch=16,cex=cbuble) }  
           else 
           {points(long,lat,col=couleurs[as.factor(classe)],pch=16) }        
           }
      }
   
     else


    { if (symbol==1)
       { if(buble)
            {points(long[!obs],lat[!obs],col="blue",pch=symbols[as.factor(classe)[!obs]],cex=cbuble[!obs])}
           else
            {points(long[!obs],lat[!obs],col="blue",pch=symbols[as.factor(classe)[!obs]])}
        }
        else
        { if(buble)
            {points(long[!obs],lat[!obs],col="blue",pch=16,cex=cbuble[!obs])}
           else 
            {points(long[!obs],lat[!obs],col="blue",pch=16, cex=0.5)}
        }
      }
   }

  else
  {
      { if(buble)
       {#points(long[!obs],lat[!obs],col="blue",pch=16,  cex=cbuble[!obs])
         points(long,lat,col="blue",pch=16,  cex=cbuble)}
       else
       {#points(long[!obs],lat[!obs],col="blue",pch=16,  cex=0.5)
      points(long,lat,col="blue",pch=16,  cex=0.5)
       }
      }
  }







####################################################
# dessin des points sélectionnés
####################################################

if(length(long[obs])!=0)
{


    #dans le cas général
    if ((method == "") || (method == "Cluster")||(method == "Quadrant"))
    {    
      

        if (symbol == 1)
        {  if ((method == "Cluster")||(method == "Quadrant"))            
           {if(buble)
            {if (method == "Quadrant")
              { if (col==1)
                 {points(long[obs],lat[obs], col=couleurs[classe[obs]],pch=symbols[as.factor(classe)[obs]],cex=cbuble[obs])}
                  else 
                 {points(long[obs],lat[obs], col="red",pch=symbols[as.factor(classe)[obs]],cex=cbuble[obs])}
              }
               else
              {points(long[obs],lat[obs], col="red",pch=symbols[as.factor(classe)[obs]],cex=cbuble[obs])}
             }
             else
            {if (method == "Quadrant")
              {if (col==1)
                 {points(long[obs],lat[obs], col=couleurs[classe[obs]],pch=symbols[as.factor(classe)[obs]])}
               else
                 {points(long[obs],lat[obs], col="red",pch=symbols[as.factor(classe)[obs]])}
               }
             else
              {points(long[obs],lat[obs], col="red",pch=symbols[as.factor(classe)[obs]])}
            }
           } 
           else  
           {if(buble)
            {points(long[obs],lat[obs], col="red", pch=10, cex=cbuble[obs])}
             else
            {points(long[obs],lat[obs], col="red", pch=10, cex=1.5)}
           } 
        }
        else
        {     
          if(col==1)
           {
            if(buble)
            {if (method == "Quadrant")
              {points(long[obs],lat[obs],col=couleurs[classe[obs]],pch=16,cex=cbuble[obs])}
             else
              {points(long[obs],lat[obs],col="red",pch=1,cex=cbuble[obs])}
              }
              else
            {if (method == "Quadrant")
              {points(long[obs],lat[obs],col=couleurs[classe[obs]],pch=16)}
             else
              {points(long[obs],lat[obs],col="red",pch=1)}  
             }
           }
          else
           {
            if(buble)
            {points(long[obs],lat[obs],col="red",pch=16,cex=cbuble[obs])}
              else
            {points(long[obs],lat[obs],col="red",pch=16)
            }
           }
        }

      #écriture des labels de chaque point sélectionné
      text(long[obs], lat[obs]+(max(lat)-min(lat))/40, label[obs], cex=0.8);
    }

#    if (method == "Quadrant")
#    {
#        if (symbol == 1)
#        {
#            points(long[obs],lat[obs], col=couleurs[obsq[obs]],pch=symbols[obsq[obs]], cex=1.5);
#        }
#        else
#        {
 #           points(long[obs],lat[obs],col=couleurs[obsq[obs]],pch=16);
 #       }
            
  #  }
    #dans le cas où on sélectionne un point sur la carte dans la fonction Neighbourmap
    if (method == "Neighbourplot1")
    {
        for (j in 1:length(long))
        {
            for (i in 1:length(long))
            {
                if (length(obs) == length(long))
                {
                    if ((W[j,i] != 0) && (obs[j]))
                    { if(buble)
                      { points(long[j],lat[j],col="red",pch=16,cex=cbuble[j])
                        #écriture des labels de chaque point sélectionné
                         text(long[j], lat[j]+(max(lat)-min(lat))/50, label[j], cex=0.8);       
                      }
                      else
                      { points(long[j],lat[j],col="red",pch=16)}
                        #écriture des labels de chaque point sélectionné
                         text(long[j], lat[j]+(max(lat)-min(lat))/50, label[j], cex=0.8);                         
                       segments(long[j], lat[j], long[i], lat[i], col="red");
                    }
                }
                else 
                {
                    if ((obs[j,1]) && (W[j,i] != 0))
                    {
                       if(buble)
                      { points(long[j],lat[j],col="red",pch=16,cex=cbuble[j])
                        #écriture des labels de chaque point sélectionné
                         text(long[j], lat[j]+(max(lat)-min(lat))/50, label[j], cex=0.8);  }
                      else
                      { points(long[j],lat[j],col="red",pch=16)}
                        #écriture des labels de chaque point sélectionné
                         text(long[j], lat[j]+(max(lat)-min(lat))/50, label[j], cex=0.8);                         
                       segments(long[j], lat[j], long[i], lat[i], col="red");
                    }
                }
            }
        }
    }

    #dans le cas où on sélectionne un point sur le graphique dans la fonction Neighbourmap
    if (method == "Neighbourplot2")
    {
        for (j in 1: length(long))
        {
            for (i in 1:length(long))
            {
                if (obs[j,i])
                {  if(buble)
                   { 
                    points(long[j],lat[j],col="red",pch=16,cex=cbuble[j]);
                    points(long[i],lat[i],col="red",pch=16,cex=cbuble[i]);
                    segments(long[j], lat[j], long[i], lat[i], col="red");
                        #écriture des labels de chaque point sélectionné
                         text(long[i], lat[i]+(max(lat)-min(lat))/50, label[i], cex=0.8);  
                        #écriture des labels de chaque point sélectionné
                         text(long[j], lat[j]+(max(lat)-min(lat))/50, label[j], cex=0.8);  
                   }
                   else
                   { 
                    points(long[j],lat[j],col="red",pch=16);
                    points(long[i],lat[i],col="red",pch=16);
                    segments(long[j], lat[j], long[i], lat[i], col="red");
                        #écriture des labels de chaque point sélectionné
                         text(long[i], lat[i]+(max(lat)-min(lat))/50, label[i], cex=0.8);  
                        #écriture des labels de chaque point sélectionné
                         text(long[j], lat[j]+(max(lat)-min(lat))/50, label[j], cex=0.8);  
                   }
                }
            }
        }
    }

    if ((method == "Angleplot") || (method == "Variocloud"))
    {
        for (j in 1:length(long))
        {
            for (i in 1:length(long))
            {
                if (obs[j,i])
                {
                  if(buble)
                  {if(symbol==0)
                   {
                    points(long[j],lat[j],col="red",pch=16, cex=cbuble[j]);
                    points(long[i],lat[i],col="red",pch=16,cex=cbuble[i]);
                    segments(long[j], lat[j], long[i], lat[i], col="black")
                   }
                   else
                   {
                    points(long[j],lat[j],col="red",pch=10, cex=cbuble[j]);
                    points(long[i],lat[i],col="red",pch=10,cex=cbuble[i]);
                    segments(long[j], lat[j], long[i], lat[i], col="black")
                   }

                  }                   
                  else
                    {if(symbol==0)
                     {
                      points(long[j],lat[j],col="red",pch=16);
                      points(long[i],lat[i],col="red",pch=16);
                      segments(long[j], lat[j], long[i], lat[i], col="black")
                     }
                     else
                     {
                      points(long[j],lat[j],col="red",pch=10);
                      points(long[i],lat[i],col="red",pch=10);
                      segments(long[j], lat[j], long[i], lat[i], col="black")
                     }
                    }
                 #écriture des labels de chaque point sélectionné
                   text(long[i], lat[i]+(max(lat)-min(lat))/50, label[i], cex=0.8);
                   text(long[j], lat[j]+(max(lat)-min(lat))/50, label[j], cex=0.8);
                }
            }
        }
    }
    
    if (method == "Scatter3d")
    {
        if (symbol == 1)
        {
            points(long[obs],lat[obs], col="green", pch=10, cex=1.5);
        }
        else
        {
            points(long[obs],lat[obs],col="green",pch=16);
        }
    }
  }


####################################################
# dessin des points selctionnés par critère
####################################################



if(nointer)
{if (method == "Cluster")   
 {if(symbol==1)
  {if(buble)
   {points(long[criteria],lat[criteria], pch="X",cex=0.7)}
  else
   {points(long[criteria],lat[criteria], pch="X",cex=0.7)}
  }
  else
  {if(buble)
   {points(long[criteria],lat[criteria], pch="X",cex=0.7)}
  else
   {points(long[criteria],lat[criteria], pch="X",cex=0.7)}
  }
 }
 else
 {
  if(buble)
   {points(long[criteria],lat[criteria], pch="X",cex=0.7)}
  else
   {points(long[criteria],lat[criteria], pch="X",cex=0.7)}
 }

}

####################################################
# Pour la légende
####################################################



 if(legends[[1]])
 {if(symbol==0)
  {
#  legend(max(long)-(max(long)-min(long))/9,min(lat)+(max(lat)-min(lat))/9, c(legmap[1],legmap[2],legmap[3]),pch = 16, pt.cex=c(2,1.125,0.25))
#  text(max(long)-(max(long)-min(long))/15,min(lat)+(max(lat)-min(lat))/7,legmap[4])
 
  legend(legends[[3]]$x,legends[[3]]$y, c(legmap[4],legmap[5],legmap[6]),title=legmap[7],pch = 16, pt.cex=c(as.numeric(legmap[1]),as.numeric(legmap[2]),as.numeric(legmap[3])))
 # text(legends[3]$x,legends[3]$y,legmap[4])


#symbols(legends[[3]]$x,legends[[3]]$y+as.numeric(legmap[3]),circle=as.numeric(legmap[3])/1.1,inches=FALSE,add=TRUE,bg=grey(.5),fg=grey(.2))
#symbols(legends[[3]]$x,legends[[3]]$y+as.numeric(legmap[2]),circle=as.numeric(legmap[2])/1.1,inches=FALSE,add=TRUE,bg=grey(.5),fg=grey(.2))
#symbols(legends[[3]]$x,legends[[3]]$y+as.numeric(legmap[1]),circle=as.numeric(legmap[1])/1.1,inches=FALSE,add=TRUE,bg=grey(.5),fg=grey(.2))

 }
  else
  {
   legend(legends[[3]]$x,legends[[3]]$y, c(legmap[4],legmap[5],legmap[6]),title=legmap[7],pch = 10, pt.cex=c(as.numeric(legmap[1]),as.numeric(legmap[2]),as.numeric(legmap[3])))
 #  text(legends[2][[1]]$x,legends[2][[1]]$y,legmap[4])
  } 
 }

 if(legends[[2]])
 {
  if((col==1)&&(symbol==1))
   {legend(legends[[4]]$x,legends[[4]]$y, labmod,col=couleurs,pch = symbols, pt.cex=1.125)}

  if((col==1)&&(symbol==0))
   {legend(legends[[4]]$x,legends[[4]]$y, labmod,col=couleurs,pch = 16, pt.cex=1.125)}

  if((col==0)&&(symbol==1))
   {legend(legends[[4]]$x,legends[[4]]$y, labmod,pch = symbols, pt.cex=1.125)}
 }


}


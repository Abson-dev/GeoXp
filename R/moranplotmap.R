`moranplotmap` <-
function (long, lat, var, W, flower=0,opt1=1, locmoran=0,listvar=NULL, listnomvar=NULL,carte=NULL,criteria=NULL, label="", symbol=0, labvar="",axis=FALSE,lablong="", lablat="")
{

#initialisation

obs<-vector(mode = "logical", length = length(long))
obsq<-rep(0,length(long));


nointer<-FALSE
nocart<-FALSE
buble<-FALSE
maptest<-FALSE

legends<-list(FALSE,FALSE,"","")
z<-NULL
legmap<-NULL
num<-NULL

graphChoice <- "";
varChoice1 <- "";
varChoice2 <- "";
choix<-""
listgraph <- c("Histogram","Barplot","Scatterplot")

graphics.off();

get(getOption("device"))()
get(getOption("device"))()

fin <- tclVar(FALSE);

quad <- FALSE;

# Transformation data.frame en matrix

if(length(listvar)>0)
{
 if(dim(as.matrix(listvar))[2]==1)
 {
 listvar<-as.matrix(listvar)
 }
}

#Normalize w
W<-normw(W)
# calcul du I de Moran
var <- var - mean(var);
wvar <- W%*%var
stdvar <- var/sd(var);
uns <- rep(1, length(var));
result <- nonormmoran(stdvar,uns,W);
MORAN <- result$morani;
prob <- 1-pnorm(result$istat);
rvar <- qr(var);
beta <- qr.coef(rvar,wvar)

# calcul de la variable obsq (pour les quadrants)
for (i in 1:length(var))
{
    if ((var[i] > 0) && (wvar[i] >= 0))
    {
        obsq[i] <- 1;
    }
    if ((var[i] <= 0) && (wvar[i] > 0))
    {
        obsq[i] <- 2;
    }
    if ((var[i] < 0) && (wvar[i] <= 0))
    {
        obsq[i] <- 3;
    }
    if ((var[i] >= 0) && (wvar[i] < 0))
    {
        obsq[i] <- 4;
    }
  }   

####################################################
# sélection d'un point
####################################################

pointfunc<-function() 
{
    quit <- FALSE;
    quad <<- FALSE;

    while(!quit)
    {
        if(maptest) 
        {
            dev.set(2);
            loc<-locator(1);
            if(is.null(loc)) 
            {
                quit<-TRUE;
                next;
            }           
            obs<<-selectmap(var1=long,var2=lat,obs=obs,Xpoly=loc[1], Ypoly=loc[2], method="point"); 
        }
        else
        {
            dev.set(3);
            loc<-locator(1);
            if(is.null(loc)) 
            {
                quit<-TRUE;
                next;
            }
            obs<<-selectmap(var1=var,var2=wvar,obs=obs,Xpoly=loc[1], Ypoly=loc[2], method="point");
        }
        
        #graphiques
        if (flower == 1)
        {
            carte(long=long, lat=lat, obs=obs,  label=label, carte=carte,nocart=nocart, symbol=symbol, W=W, method="Neighbourplot1",buble=buble,cbuble=z,criteria=criteria,nointer=nointer,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
        }
        else
        {
            carte(long=long, lat=lat, obs=obs,  label=label, carte=carte,nocart=nocart, symbol=symbol,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
        }       

        graphique(var1=var, var2=wvar, obs=obs, num=3, graph="Moran", labvar=labvar, symbol=symbol, b=beta, locmoran=locmoran,opt1=opt1);
        if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
        {
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=4, graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2));
        }
    }
  }

####################################################
# sélection d'un point sur la carte
####################################################

pt1func <- function()
{
    maptest <<- TRUE;
    pointfunc();
  }

####################################################
# sélection d'un point sur le graphique
####################################################

pt2func <- function()
{
    maptest <<- FALSE;
    pointfunc();
  }

####################################################
# sélection d'un polygone
####################################################

polyfunc<-function() 
{
    polyX <- NULL;
    polyY <- NULL;
    quit <- FALSE;
    quad <<- FALSE;

    while(!quit)
    {
        if(maptest)
        {
            dev.set(2);
            loc<-locator(1);
            if(is.null(loc)) 
            {
                quit<-TRUE;
                next;
            }
        }
        else
        {
            dev.set(3);
            loc<-locator(1);
            if(is.null(loc)) 
            {
                quit<-TRUE;
                next;
            }   
        }
        polyX <- c(polyX, loc[1]);
        polyY <- c(polyY, loc[2]);
        lines(polyX,polyY);
    }
    polyX <- c(polyX, polyX[1]);
    polyY <- c(polyY, polyY[1]);
if (length(polyX)>0)
{
    lines(polyX,polyY);

    if (maptest)
    {
        obs <<- selectmap(var1=long, var2=lat, obs=obs, Xpoly=polyX, Ypoly=polyY, method="poly");
    }
    else
    {
        obs <<- selectmap(var1=var, var2=wvar, obs=obs, Xpoly=polyX, Ypoly=polyY, method="poly");
    }

    #graphiques
    if (flower == 1)
    {
        carte(long=long, lat=lat, obs=obs,  label=label, carte=carte,nocart=nocart, symbol=symbol, W=W, method="Neighbourplot1",buble=buble,cbuble=z,criteria=criteria,nointer=nointer,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
    }
    else
    {
        carte(long=long, lat=lat, obs=obs,  label=label, carte=carte,nocart=nocart, symbol=symbol,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
    }       

    graphique(var1=var, var2=wvar, obs=obs, num=3, graph="Moran", labvar=labvar, symbol=symbol, b=beta, locmoran=locmoran,opt1=opt1);
    if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
        {
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=4, graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2));
        }
}
  }

####################################################
# sélection d'un polygone sur la carte
####################################################

poly1func <- function()
{
    if (quad == TRUE)
    {
        SGfunc();
        quad <<- FALSE;
    }
    
    maptest <<- TRUE;
    polyfunc();
  }

####################################################
# sélection d'un polygone sur le graphique
####################################################

poly2func <- function()
{
    if (quad == TRUE)
    {
        SGfunc();
        quad <<- FALSE;
    }
    maptest <<- FALSE;
    polyfunc();
  }

####################################################
# sélection d'un quadrant
####################################################

quadfunc <- function()
{
    if (quad == FALSE)
    {
        SGfunc();
        quad <<- TRUE;
    }

    obs[which(obsq == num)] <<- !obs[which(obsq == num)];



    carte(long=long, lat=lat, obs=obs, obsq=obsq,  carte=carte,nocart=nocart, label=label, symbol=symbol, method="Quadrant",buble=buble,cbuble=z,criteria=criteria,nointer=nointer,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)

    graphique(var1=var, var2=wvar, obs=obs, num=3, graph="Quadrant", labvar=labvar, symbol=symbol, b=beta, locmoran=locmoran, obsq=obsq,opt1=opt1);
    if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
        {
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=4, graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2));
        }
  }

####################################################
# sélection du quadrant 1 sur le graphique
####################################################

quad1func <- function()
{
    num <<- 1
    quadfunc();
  }

####################################################
# sélection du quadrant 2 sur le graphique
####################################################

quad2func <- function()
{
    num <<- 2
    quadfunc();
  }

####################################################
# sélection du quadrant 3 sur le graphique
####################################################

quad3func <- function()
{
    num <<- 3
    quadfunc();
  }

####################################################
# sélection du quadrant 4 sur le graphique
####################################################

quad4func <- function()
{
    num <<- 4
    quadfunc();
  }


####################################################
# choix d'un autre graphique
####################################################

graphfunc <- function()
{
    if ((length(listvar) != 0) && (length(listnomvar) != 0))
    {
        dev.off(4);
        choix <<- selectgraph(listnomvar,listgraph);
        varChoice1 <<- choix$varChoice1;
        varChoice2 <<- choix$varChoice2;
        graphChoice <<- choix$graphChoice;
            
        if ((graphChoice != "") && (varChoice1 != ""))
        {
            x11(width=5, height=5);
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=4, graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2));
            }
    }
    else
    {
        tkmessageBox(message="List of Variables and list of variables names must have been given",icon="warning",type="ok");
    }
  }


####################################################
# contour des unités spatiales
####################################################
cartfunc <- function()
{
  if(!nocart)
   { if (length(carte) != 0)
       {nocart<<-TRUE
        if (quad)
        {
            carte(long=long, lat=lat, obs=obs,  label=label, symbol=symbol, carte=carte,nocart=nocart, obsq=obsq, method="Quadrant",buble=buble,cbuble=z,criteria=criteria,nointer=nointer,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
        }
        else
        {
          if (flower == 1)
          {
           carte(long=long, lat=lat, obs=obs,  label=label, carte=carte,nocart=nocart, symbol=symbol, W=W, method="Neighbourplot1",buble=buble,cbuble=z,criteria=criteria,nointer=nointer,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
          }
         else
          {
            carte(long=long, lat=lat, obs=obs,  label=label, carte=carte,nocart=nocart, symbol=symbol,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
          }        
        }


      }
     else
       {
        tkmessageBox(message="Spatial Contours have not been given",icon="warning",type="ok")
       }
  }

 else


 {
   nocart<<-FALSE;
        if (quad)
        {
            carte(long=long, lat=lat, obs=obs,  label=label, symbol=symbol, carte=carte,nocart=nocart, obsq=obsq, method="Quadrant",buble=buble,cbuble=z,criteria=criteria,nointer=nointer,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
        }
        else
        {
         if (flower == 1)
         {
          carte(long=long, lat=lat, obs=obs,  label=label, carte=carte,nocart=nocart, symbol=symbol, W=W, method="Neighbourplot1",buble=buble,cbuble=z,criteria=criteria,nointer=nointer,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
         }
         else
         {
          carte(long=long, lat=lat, obs=obs,  label=label, carte=carte,nocart=nocart, symbol=symbol,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
         }    
      } }

}


####################################################
# rafraichissement des graphiques
####################################################

SGfunc<-function() 
{
    obs<<-vector(mode = "logical", length = length(long));

    carte(long=long, lat=lat, obs=obs,  label=label, carte=carte,nocart=nocart, symbol=symbol,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
    graphique(var1=var, var2=wvar, obs=obs, num=3, graph="Moran", labvar=labvar, symbol=symbol, b=beta, locmoran=locmoran, obsq=obsq,opt1=opt1);
   if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
        {
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=4, graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2));
        }

}

####################################################
# quitter l'application
####################################################

quitfunc<-function() 
{
    tclvalue(fin)<<-TRUE;
#    graphics.off();
    tkdestroy(tt);
  }


####################################################
# Open a no interactive selection
####################################################

fnointer<-function() 
{
 if(!nointer)
  { if (length(criteria) != 0)
     {nointer<<-TRUE
        if (quad)
        {
            carte(long=long, lat=lat, obs=obs,  label=label, symbol=symbol,  carte=carte,nocart=nocart, obsq=obsq, method="Quadrant",buble=buble,cbuble=z,criteria=criteria,nointer=nointer,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
        }
        else
        {
          if (flower == 1)
          {
           carte(long=long, lat=lat, obs=obs,  label=label, carte=carte,nocart=nocart, symbol=symbol, W=W, method="Neighbourplot1",buble=buble,cbuble=z,criteria=criteria,nointer=nointer,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
          }
         else
          {
            carte(long=long, lat=lat, obs=obs,  label=label, carte=carte,nocart=nocart, symbol=symbol,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
          }          }     
     }
   else
      {
       tkmessageBox(message="Criteria has not been given",icon="warning",type="ok")
      }

  }

 else 

 {
   nointer<<-FALSE;
        if (quad)
        {
            carte(long=long, lat=lat, obs=obs,  label=label, symbol=symbol,  carte=carte,nocart=nocart, obsq=obsq, method="Quadrant",buble=buble,cbuble=z,criteria=criteria,nointer=nointer,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
        }
        else
        {
          if (flower == 1)
          {
           carte(long=long, lat=lat, obs=obs,  label=label, carte=carte,nocart=nocart, symbol=symbol, W=W, method="Neighbourplot1",buble=buble,cbuble=z,criteria=criteria,nointer=nointer,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
          }
         else
          {
            carte(long=long, lat=lat, obs=obs,  label=label, carte=carte,nocart=nocart, symbol=symbol,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
          }          }
 }

}



####################################################
# Choisir une variable
####################################################

    choixvarfunc <- function(title, question, liste) {
        tt2 <- tktoplevel()
        scr <- tkscrollbar(tt2, repeatinterval = 5, command = function(...) tkyview(lstbox, 
            ...))
        lstbox <- tklistbox(tt2, height = 4, selectmode = "single", 
            yscrollcommand = function(...) tkset(scr, ...), background = "white")
        tkgrid(tklabel(tt2, text = question))
        tkfocus(tt2)
        tkwm.title(tt2, title)
        varChoice <- ""
        tkgrid(lstbox, scr)
        tkgrid.configure(scr, rowspan = 4, sticky = "nsw")
        var <- liste
        for (i in 1:length(var)) {
            tkinsert(lstbox, "end", var[i])
        }
        OnOK <- function() {
            varChoice <<- var[as.numeric(tkcurselection(lstbox)) + 
                1]
            tkdestroy(tt2)
        }
        OK.but <- tkbutton(tt2, text = "   OK   ", command = OnOK)
        tkgrid(OK.but)
        tkbind(tt2, "<Destroy>", function() {
            tkdestroy(tt2)
        })
        tkfocus(tt2)
        tkwait.window(tt2)
        return(varChoice)
    }



####################################################
# Bubble
####################################################

fbubble<-function(){

if(!buble)
{
   if ((length(listvar) != 0) && (length(listnomvar) != 0))
    {

     varChoix <- choixvarfunc("Choice of variables","Choose the variable",listnomvar)
     bubble <- listvar[,which(listnomvar == varChoix)]
      if ((length(bubble) != 0)&&(min(bubble)>=0))
       { 
         buble<<-TRUE   
       } 
      else
       {
        tkmessageBox(message="Bubbles have not been given or must be positiv",icon="warning",type="ok")
       }

    }

else
    {
        tkmessageBox(message="To use Bubbles, listvar and listnomvar must have be given in input",icon="warning",type="ok");
    }

}

else
{buble<<-FALSE
 legends<<-list(FALSE,legends[[2]],"",legends[[4]])
 if (quad)
        {
            carte(long=long, lat=lat, obs=obs,  label=label, symbol=symbol,  carte=carte,nocart=nocart, obsq=obsq, method="Quadrant",buble=buble,cbuble=z,criteria=criteria,nointer=nointer,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
        }
        else
        {
          if (flower == 1)
          {
           carte(long=long, lat=lat, obs=obs,  label=label, carte=carte,nocart=nocart, symbol=symbol, W=W, method="Neighbourplot1",buble=buble,cbuble=z,criteria=criteria,nointer=nointer,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
          }
         else
          {
            carte(long=long, lat=lat, obs=obs,  label=label, carte=carte,nocart=nocart, symbol=symbol,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
          }          }}




if(buble)
{

tt2 <- tktoplevel()

OnOK <- function()
{ 
tkdestroy(tt2)	

tt3 <- tktoplevel()


bubl<-function()
{
rbValue="math"
tkdestroy(tt3)	
msg <- paste("Click on the map to indicate the location of the upper left corner of the legend box
")
tkmessageBox(message=msg)
dev.set(2);
loc <- locator(1)

if(rbValue=="math")
{
z<<-sqrt(bubble/max(bubble))*3
legmap<<-c(sqrt(as.numeric(tclvalue(ma))/max(bubble))*3,sqrt(as.numeric(tclvalue(mea))/max(bubble))*3,sqrt(as.numeric(tclvalue(mi))/max(bubble))*3,as.numeric(tclvalue(ma)),as.numeric(tclvalue(mea)),as.numeric(tclvalue(mi)),varChoix)
}
else
{
z<<-3*(bubble/max(bubble))^0.57
legmap<<-c(3*(as.numeric(tclvalue(ma))/max(bubble))^0.57,3*(as.numeric(tclvalue(mea))/max(bubble))^0.57,3*(as.numeric(tclvalue(mi))/max(bubble))^0.57,as.numeric(tclvalue(ma)),as.numeric(tclvalue(mea)),as.numeric(tclvalue(mi)),varChoix)
}


legends<<-list(TRUE,legends[[2]],loc,legends[[4]])
 if (quad)
        {
            carte(long=long, lat=lat, obs=obs,  label=label, symbol=symbol,  carte=carte,nocart=nocart, obsq=obsq, method="Quadrant",buble=buble,cbuble=z,criteria=criteria,nointer=nointer,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
        }
        else
        {
          if (flower == 1)
          {
           carte(long=long, lat=lat, obs=obs,  label=label, carte=carte,nocart=nocart, symbol=symbol, W=W, method="Neighbourplot1",buble=buble,cbuble=z,criteria=criteria,nointer=nointer,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
          }
         else
          {
            carte(long=long, lat=lat, obs=obs,  label=label, carte=carte,nocart=nocart, symbol=symbol,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
          }          }

}


mi<-tclVar(round(min(bubble),2))
mea<-tclVar(round(mean(bubble),2))
ma<-tclVar(round(max(bubble),2))
entry.Name <-tkentry(tt3,width="5",textvariable=mi)
entry.Name2 <-tkentry(tt3,width="5",textvariable=mea)
entry.Name3 <-tkentry(tt3,width="5",textvariable=ma)
tkgrid(tklabel(tt3,text="Break Points:"))
tkgrid(tklabel(tt3,text="Small Bubble"),entry.Name)
tkgrid(tklabel(tt3,text="Middle Bubble"),entry.Name2)
tkgrid(tklabel(tt3,text="Large Bubble"),entry.Name3)

autre.but <- tkbutton(tt3, text="     OK     " , command=bubl);
tkgrid(autre.but,columnspan=2)
tkgrid(tklabel(tt3,text="    "))
tkfocus(tt3)

}

OnOK2 <- function()
{ 
rbValue="math"
tkdestroy(tt2)
if(rbValue=="math")
{
z<<-sqrt(bubble/max(bubble))*3
}
else
{
z<<-3*(bubble/max(bubble))^0.57
}	
 if (quad)
        {
            carte(long=long, lat=lat, obs=obs,  label=label, symbol=symbol,  carte=carte,nocart=nocart, obsq=obsq, method="Quadrant",buble=buble,cbuble=z,criteria=criteria,nointer=nointer,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
        }
        else
        {
          if (flower == 1)
          {
           carte(long=long, lat=lat, obs=obs,  label=label, carte=carte,nocart=nocart, symbol=symbol, W=W, method="Neighbourplot1",buble=buble,cbuble=z,criteria=criteria,nointer=nointer,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
          }
         else
          {
            carte(long=long, lat=lat, obs=obs,  label=label, carte=carte,nocart=nocart, symbol=symbol,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
          }          }
}


labelText12 <- tclVar("Do you want a legend for bubbles")
label12 <- tklabel(tt2,justify = "center", wraplength = "3i", text=tclvalue(labelText12))
tkconfigure(label12, textvariable=labelText12)
tkgrid(label12,columnspan=2)

point.but <- tkbutton(tt2, text="  Yes  ", command=OnOK);
poly.but <- tkbutton(tt2, text=" No ", command=OnOK2);
tkgrid(point.but, poly.but)
tkgrid(tklabel(tt2,text="    "))
tkfocus(tt2)
}



}





####################################################
# Permutation
####################################################
permutation<-function()
{


tt1<-tktoplevel()
Name <- tclVar("n")
entry.Name <-tkentry(tt1,width="5",textvariable=Name)
tkgrid(tklabel(tt1,text="Number of simulations"),entry.Name)




OnOK <- function()
{ 
	value1 <- tclvalue(Name)
	tkdestroy(tt1)
       if(is.na(as.integer(value1)))
    {
        tkmessageBox(message="Sorry, but you have to choose decimal values",icon="warning",type="ok");
    }
      else
    {
    n=as.integer(value1)
    perm<-NULL
     for (i in 1:n)
     {
       sam<-sample(var,length(var))
       sam <- sam - mean(sam);

       epe <- sam %*% sam
       mi <- (sam %*% W %*% sam)/epe
       morani <- round(mi,4);
       perm <- c(perm,morani);
     }

     msg <- paste("The p-value of the permutation test is :",1-length(which(perm<rep(MORAN,n)))/n)

     tkmessageBox(message=msg,icon="info",type="ok") 

    }   
}



OK.but <-tkbutton(tt1,text="   OK   ",command=OnOK)
#tkbind(entry.Name, "<Return>",OnOK)
tkgrid(OK.but)
tkfocus(tt1)



}

carte(long=long, lat=lat, obs=obs,  label=label, symbol=symbol,axis=axis,lablong=lablong, lablat=lablat)
graphique(var1=var, var2=wvar, obs=obs, num=3, graph="Moran", labvar=labvar, symbol=symbol, b=beta, locmoran=locmoran, obsq=obsq,opt1=opt1);


####################################################
# création de la boite de dialogue
####################################################
if(interactive())
{
tt <- tktoplevel()

labelText1 <- tclVar("Work on the map")
label1 <- tklabel(tt,justify = "center", wraplength = "3i", text=tclvalue(labelText1))
tkconfigure(label1, textvariable=labelText1)
tkgrid(label1,columnspan=2)

point.but <- tkbutton(tt, text="  Points  ", command=pt1func);
poly.but <- tkbutton(tt, text=" Polygon ", command=poly1func);
tkgrid(point.but, poly.but)
tkgrid(tklabel(tt,text="    "))


labelText1 <- tclVar("Work on the moranplot")
label1 <- tklabel(tt,justify = "center", wraplength = "3i", text=tclvalue(labelText1))
tkconfigure(label1, textvariable=labelText1)
tkgrid(label1,columnspan=2)

point1.but <- tkbutton(tt, text="  Points  ", command=pt2func);
poly1.but <- tkbutton(tt, text=" Polygon ", command=poly2func);
tkgrid(point1.but, poly1.but)
tkgrid(tklabel(tt,text="    "))


labelText1 <- tclVar("Choose a quadrant")
label1 <- tklabel(tt,justify = "center", wraplength = "3i", text=tclvalue(labelText1))
tkconfigure(label1, textvariable=labelText1)
tkgrid(label1,columnspan=2)

point2.but <- tkbutton(tt, text="4th Quadrant", command=quad2func);
poly2.but <- tkbutton(tt, text="1st Quadrant", command=quad1func);
tkgrid(point2.but, poly2.but)
point3.but <- tkbutton(tt, text="3rd Quadrant", command=quad3func);
poly3.but <- tkbutton(tt, text="2nd Quadrant", command=quad4func);
tkgrid(point3.but, poly3.but)

tkgrid(tklabel(tt,text="    "))



label1 <- tclVar("To stop selection, leave the cursur on the active graph, click on the right button of the mouse and stop")
label11 <- tklabel(tt,justify = "center", wraplength = "3i", text=tclvalue(label1))
tkconfigure(label11, textvariable=label1)
tkgrid(label11,columnspan=2)
tkgrid(tklabel(tt,text="    "))


msg <- paste("Moran index: ", MORAN, " - ","p-value (Gaussian Test) : ", if(round(prob,4)<0.0001){"<0.0001"}else{round(prob,4)}) 
tkgrid(tklabel(tt,text=msg),columnspan=2)
tkgrid(tklabel(tt,text="    "))


labelText10 <- tclVar("Permutation Test")
label10 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText10))
tkconfigure(label10, textvariable=labelText10)
tkgrid(label10,columnspan=2)

noint10.but <- tkbutton(tt, text="     OK     ", command=permutation);
tkgrid(noint10.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))


labelText7 <- tclVar("Non interactive selection  /  Draw Spatial contours ")
label7 <- tklabel(tt,justify = "center", wraplength = "3i", text=tclvalue(labelText7))
tkconfigure(label7, textvariable=labelText7)
tkgrid(label7,columnspan=2)

noint1.but <- tkbutton(tt, text="  On/Off  ", command=fnointer);
nocou1.but <- tkbutton(tt, text="  On/Off  ", command=cartfunc);
tkgrid(noint1.but , nocou1.but)
tkgrid(tklabel(tt,text="    "))



labelText9 <- tclVar("Bubbles")
label9 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText9))
tkconfigure(label9, textvariable=labelText9)
tkgrid(label9,columnspan=2)

bubble.but <- tkbutton(tt, text="  On/Off  ", command=fbubble);
tkgrid(bubble.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))

labelText3 <- tclVar("Restore graph")
label3 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText3))
tkconfigure(label3, textvariable=labelText3)
tkgrid(label3,columnspan=2)

nettoy.but <- tkbutton(tt, text="     OK     " , command=SGfunc);
tkgrid(nettoy.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))


labelText4 <- tclVar("Additional graph")
label4 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText4))
tkconfigure(label4, textvariable=labelText4)
tkgrid(label4,columnspan=2)

autre.but <- tkbutton(tt, text="     OK     " , command=graphfunc);
tkgrid(autre.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))


labelText5 <- tclVar(" Exit ")
label5 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText5))
tkconfigure(label5, textvariable=labelText5)
tkgrid(label5,columnspan=2)

quit.but <- tkbutton(tt, text="     OK     ", command=quitfunc);
tkgrid(quit.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))

tkwait.variable(fin)
}
####################################################


return(list(obs=obs,MORAN=MORAN))
  }


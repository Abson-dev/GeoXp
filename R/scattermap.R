`scattermap` <-
function(long,lat,var1,var2,listvar=NULL, listnomvar=NULL,opt=1,quantiles=0.5,criteria=NULL,carte=NULL,label="",symbol=0, labvar=c("",""), color=1,axis=FALSE, lablong="", lablat="")
{
# initialisation

nointer<-FALSE
nocart<-FALSE
buble<-FALSE
maptt<-FALSE
z<-NULL
legmap<-NULL
legends<-list(FALSE,FALSE,"","")

# transformation data.frame en matrix

if(length(listvar)>0)
{
if(dim(as.matrix(listvar))[2]==1)
{
listvar<-as.matrix(listvar)
}
}

obs<-vector(mode = "logical", length = length(long))
graphics.off();


if((labvar[1]==c(""))&&(labvar[2]==c("")))
{labvar<-c(names(data.frame(var1,var2))[1],names(data.frame(var1,var2))[2])}

var1=as.matrix(var1)
var2=as.matrix(var2)
lat=as.matrix(lat)
long=as.matrix(long)

graphChoice <- "";
varChoice1 <- "";
varChoice2 <- "";
choix<-"";
listgraph <- c("Histogram","Barplot","Scatterplot")

get(getOption("device"))()
get(getOption("device"))()
    fin <- tclVar(FALSE)
    
    
if ((opt == 2) || (opt == 3))
{
   # v1 <- sort(var1);
   # meandist <- 0;
   # for (i in 1:(length(long)-1))
   # {
   #     meandist <- meandist + v1[i+1] - v1[i];
   # }
   # h <- (1 / (length(var1) - 1)) * meandist;

        u1 <- sort(var1)
        u1 <- as.vector(u1)
        z <- seq(min(u1), max(u1), by = ((max(u1)-min(u1))/1000))
        z <- round(z)
        z1 <- z[2:length(z)] - z[1:(length(z) - 1)]
        h <- mean(z1)

    p <- 1 / (1 + (h^3 / 6));
    p1 <- 1 / (1 + (h^3 / 1000)); #60
    p2 <- 1 / (1 + (h^3 / 4)); #0.6
    alpha <- (1 - p) / p;
    borne1 <- (1 - p1) / p1;
    borne2 <- (1 - p2) / p2;


  }


####################################################
# sélection d'un point
####################################################

pointfunc<-function() 
{
    quit <- FALSE;

    while(!quit)
    {
        if(maptt) 
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
            obs<<-selectmap(var1=var1,var2=var2,obs=obs,Xpoly=loc[1], Ypoly=loc[2], method="point");
        }
        
        #graphiques
      graphique(var1=var1, var2=var2, obs=obs, num=3, graph="Scatterplot", labvar=labvar, symbol=symbol, opt1=opt, quantiles=quantiles, alpha1=alpha);
# Remarque : s'il y a tous ces If, c'est pour prévoir de rajoutter un barplot avec options de couleurs
  # sur la carte 

         if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
           {if(graphChoice=="Barplot")
             {
               labmod<-levels(as.factor(listvar[,which(listnomvar == varChoice1)]))
               graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=4, graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2),labmod=labmod,col=color);
               carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart,method="Cluster",classe=listvar[,which(listnomvar == varChoice1)],col=color,legmap=legmap,legends=legends,labmod=labmod,axis=axis)
             }
           else
            {
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=4, graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2));
            carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axis)
            } 
           }
       else
         {carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axis)
  
         }  


    }
  }

####################################################
# sélection d'un point sur la carte
####################################################

pt1func <- function()
{   maptt <<- TRUE;
    pointfunc();
  }

####################################################
# sélection d'un point sur le graphique
####################################################

pt2func <- function()
{
    maptt <<- FALSE;
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

    while(!quit)
    {
        if(maptt)
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

    if (maptt)
    {
        obs <<- selectmap(var1=long, var2=lat, obs=obs, Xpoly=polyX, Ypoly=polyY, method="poly");
    }
    else
    {
        obs <<- selectmap(var1=var1, var2=var2, obs=obs, Xpoly=polyX, Ypoly=polyY, method="poly");
    }

    #graphiques
    graphique(var1=var1, var2=var2, obs=obs, num=3, graph="Scatterplot", labvar=labvar, symbol=symbol, opt1=opt, quantiles=quantiles, alpha1=alpha);
    # Remarque : s'il y a tous ces If, c'est pour prévoir de rajoutter un barplot avec options de couleurs
  # sur la carte 

         if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
           {if(graphChoice=="Barplot")
             {
               labmod<-levels(as.factor(listvar[,which(listnomvar == varChoice1)]))
               graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=4, graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2),labmod=labmod,col=color);
               carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart,method="Cluster",classe=listvar[,which(listnomvar == varChoice1)],col=color,legmap=legmap,legends=legends,labmod=labmod,axis=axis)
             }
           else
            {
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=4, graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2));
            carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axis)
            } 
           }
       else
         {carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axis)
  
         }  
}
  }

####################################################
# sélection d'un polygone sur la carte
####################################################

poly1func <- function()
{
    SGfunc();
    maptt <<- TRUE;
   polyfunc();
  }

####################################################
# sélection d'un polygone sur le graphique
####################################################

poly2func <- function()
{
    SGfunc();
    maptt <<- FALSE;
    polyfunc();
  }


####################################################
# contour des unités spatiales
####################################################
cartfunc <- function()
{
  if(!nocart)
   { if (length(carte) != 0)
       {nocart<<-TRUE
      # graphiques

         if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
           {if(graphChoice=="Barplot")
             {
               labmod<-levels(as.factor(listvar[,which(listnomvar == varChoice1)]))
               carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart,method="Cluster",classe=listvar[,which(listnomvar == varChoice1)],col=color,legmap=legmap,legends=legends,labmod=labmod,axis=axis)
             }
           else
            {
          carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axis)
            } 
           }
       else
         {carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axis)
  
         }  


    }
     else
       {
        tkmessageBox(message="Spatial Contours have not been given",icon="warning",type="ok")
         if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
           {if(graphChoice=="Barplot")
             {
               labmod<-levels(as.factor(listvar[,which(listnomvar == varChoice1)]))
               carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart,method="Cluster",classe=listvar[,which(listnomvar == varChoice1)],col=color,legmap=legmap,legends=legends,labmod=labmod,axis=axis)
             }
           else
            {
          carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axis)
            } 
           }
       else
         {carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axis)
  
         }        }
  }

else


{
   nocart<<-FALSE;
         if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
           {if(graphChoice=="Barplot")
             {
               labmod<-levels(as.factor(listvar[,which(listnomvar == varChoice1)]))
               carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart,method="Cluster",classe=listvar[,which(listnomvar == varChoice1)],col=color,legmap=legmap,legends=legends,labmod=labmod,axis=axis)
             }
           else
            {
          carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axis)
            } 
           }
       else
         {carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axis)
  
         }  
}

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

      if (((graphChoice == "Histogram")&&(!is.numeric(listvar[,which(listnomvar == varChoice1)])))||((graphChoice == "Scatterplot")&&((!is.numeric(listvar[,which(listnomvar == varChoice1)]))||(!is.numeric(listvar[,which(listnomvar == varChoice2)]))))) 
        {
        tkmessageBox(message="Variables choosed are not in a good format",icon="warning",type="ok");
        }
       else
           { 
            x11(width=5, height=5);
            if(graphChoice=="Barplot")
             {
                  tt1<-tktoplevel()

OnOK <- function()
{ 

tkdestroy(tt1) 
msg <- paste("Click on the map to indicate the location of the upper left corner of the legend box")
tkmessageBox(message=msg)

dev.set(2);
loc <- locator(1)
legends<<-list(legends[[1]],TRUE,legends[[3]],loc)


labmod<-levels(as.factor(listvar[,which(listnomvar == varChoice1)]))
graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=4, graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2),labmod=labmod,col=color);
carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,  label=label, symbol=symbol,carte=carte,nocart=nocart,method="Cluster",classe=listvar[,which(listnomvar == varChoice1)],col=color,legmap=legmap,legends=legends,labmod=labmod,axis=axis,lablong=lablong,lablat=lablat)
   
}


OnOK2 <- function()
{ 

legends<<-list(legends[[1]],FALSE,legends[[3]],"")
tkdestroy(tt1) 

labmod<-levels(as.factor(listvar[,which(listnomvar == varChoice1)]))
graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=4, graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2),labmod=labmod,col=color);
carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,  label=label, symbol=symbol,carte=carte,nocart=nocart,method="Cluster",classe=listvar[,which(listnomvar == varChoice1)],col=color,legmap=legmap,legends=legends,labmod=labmod,axis=axis,lablong=lablong,lablat=lablat)
  
}



labelText12 <- tclVar("Do you want a legend for factors")
label12 <- tklabel(tt1,justify = "center", wraplength = "3i", text=tclvalue(labelText12))
tkconfigure(label12, textvariable=labelText12)
tkgrid(label12,columnspan=2)

point.but <- tkbutton(tt1, text="  Yes  ", command=OnOK);
poly.but <- tkbutton(tt1, text=" No ", command=OnOK2);
tkgrid(point.but, poly.but)
tkgrid(tklabel(tt,text="    "))


tkfocus(tt1)    }
           else
            {
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=4, graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2));
            carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axis)
            } }
       }
    }
    else
    {
        tkmessageBox(message="List of Variables and list of variable names must have been given",icon="warning",type="ok");
    }
  }


####################################################
# rafraichissement des graphiques
####################################################

SGfunc<-function() 
{
    obs<<-vector(mode = "logical", length = length(long));

# graphiques

    #graphiques
    graphique(var1=var1, var2=var2, obs=obs, num=3, graph="Scatterplot", labvar=labvar, symbol=symbol, opt1=opt, quantiles=quantiles, alpha1=alpha);

         if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
           {if(graphChoice=="Barplot")
             {
               labmod<-levels(as.factor(listvar[,which(listnomvar == varChoice1)]))
               graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=4, graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2),labmod=labmod,col=color);
               carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart,method="Cluster",classe=listvar[,which(listnomvar == varChoice1)],col=color,legmap=legmap,legends=legends,labmod=labmod,axis=axis)
             }
           else
            {
            graphique(var1=listvar[,which(listnomvar == varChoice1)], var2=listvar[,which(listnomvar == varChoice2)], obs=obs, num=4, graph=graphChoice, symbol=1, labvar=c(varChoice1,varChoice2));
            carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axis)
            } 
           }
       else
         {carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axis)
  
         }  

}

####################################################
# quitter l'application
####################################################

quitfunc<-function() 
{
    tclvalue(fin)<<-TRUE
#    graphics.off();
    tkdestroy(tt);
  }


####################################################
# modification du alpha de l'estimateur
####################################################
  
refresh.code<-function(...)
{
    alpha<<-slider1(no=1); 
    graphique(var1=var1, var2=var2, obs=obs, num=3, graph="Scatterplot", labvar=labvar, symbol=symbol, opt1=opt, quantiles=quantiles, alpha1=alpha);
  }


####################################################
# Open a no interactive selection
####################################################

fnointer<-function() 
{
if(!nointer)
  { if (length(criteria) != 0)
     {nointer<<-TRUE
         if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
           {if(graphChoice=="Barplot")
             {
               labmod<-levels(as.factor(listvar[,which(listnomvar == varChoice1)]))
               carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart,method="Cluster",classe=listvar[,which(listnomvar == varChoice1)],col=color,legmap=legmap,legends=legends,labmod=labmod,axis=axis)
             }
           else
            {
          carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axis)
            } 
           }
       else
         {carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axis)
  
         }    }
   else
      {
       tkmessageBox(message="Criteria has not been given",icon="warning",type="ok")
      }

  }

else 

{
   nointer<<-FALSE;

         if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
           {if(graphChoice=="Barplot")
             {
                labmod<-levels(as.factor(listvar[,which(listnomvar == varChoice1)]))
               carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart,method="Cluster",classe=listvar[,which(listnomvar == varChoice1)],col=color,legmap=legmap,legends=legends,labmod=labmod,axis=axis)
             }
           else
            {
          carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axis)
            } 
           }
       else
         {carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axis)
  
         }  
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
     varChoix <- choixvarfunc("Choice of variables","Choose a variable",listnomvar)
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
        tkmessageBox(message="To use Bubbles, the lists wich contain the variables and their names must have been given",icon="warning",type="ok");
    }

}

else
{buble<<-FALSE
 legends<<-list(FALSE,legends[[2]],"",legends[[4]])
     if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
           {if(graphChoice=="Barplot")
             {

                labmod<-levels(as.factor(listvar[,which(listnomvar == varChoice1)]))
               carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs, label=label, symbol=symbol,carte=carte,nocart=nocart,method="Cluster",classe=listvar[,which(listnomvar == varChoice1)],col=color,legmap=legmap,legends=legends,labmod=labmod, lablong="", lablat="",axis=axis)
             }
           else
            {
          carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends, lablong="", lablat="",axis=axis)
            } 
           }
       else
         {carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends, lablong="", lablat="",axis=axis)
  
         }  
}


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
         if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
           {if(graphChoice=="Barplot")
             {
                labmod<-levels(as.factor(listvar[,which(listnomvar == varChoice1)]))
               carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,  label=label, symbol=symbol,carte=carte,nocart=nocart,method="Cluster",classe=listvar[,which(listnomvar == varChoice1)],col=color,legmap=legmap,legends=legends,labmod=labmod,axis=axis,lablong=lablong, lablat=lablat)
             }
           else
            {
          carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,  label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
            } 
           }
       else
         {carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,  label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends,axis=axis,lablong=lablong, lablat=lablat)
  
         }   

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
         if ((graphChoice != "") && (varChoice1 != "") && (length(dev.list()) > 2))
           {if(graphChoice=="Barplot")
             {

                labmod<-levels(as.factor(listvar[,which(listnomvar == varChoice1)]))
               carte(long=long, lat=lat,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,obs=obs,label=label, symbol=symbol,carte=carte,nocart=nocart,method="Cluster",classe=listvar[,which(listnomvar == varChoice1)],col=color,legmap=legmap,legends=legends,labmod=labmod, lablong="", lablat="",axis=axis)
             }
           else
            {
          carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer,  label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends, lablong="", lablat="",axis=axis)
            } 
           }
       else
         {carte(long=long, lat=lat,obs=obs,buble=buble,cbuble=z,criteria=criteria,nointer=nointer, label=label, symbol=symbol,carte=carte,nocart=nocart,legmap=legmap,legends=legends, lablong="", lablat="",axis=axis)
  
         } 
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


carte(long=long, lat=lat,criteria=criteria,nointer=nointer,buble=buble,cbuble=z,obs=obs, lablong=lablong, lablat=lablat, label=label, symbol=symbol,carte=carte,legmap=legmap,legends=legends,axis=axis)
graphique(var1=var1, var2=var2, obs=obs, num=3, graph="Scatterplot", labvar=labvar, symbol=symbol, opt1=opt, quantiles=quantiles,alpha1=alpha);


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

point.but <- tkbutton(tt, text="  Point  ", command=pt1func);
poly.but <- tkbutton(tt, text=" Polygon ", command=poly1func);
tkgrid(point.but, poly.but)
tkgrid(tklabel(tt,text="    "))

labelText2 <- tclVar("Work on the scattermap")
label2 <- tklabel(tt,justify = "center", wraplength = "3i", text=tclvalue(labelText2))
tkconfigure(label2, textvariable=labelText2)
tkgrid(label2,columnspan=2)

p2.but <- tkbutton(tt, text="  Point  ", command=pt2func);
pol2.but <- tkbutton(tt, text=" Polygon ", command=poly2func);
tkgrid(p2.but, pol2.but)
tkgrid(tklabel(tt,text="    "))


label1 <- tclVar("To stop selection, leave the cursur on the active graph, click on the right button of the mouse and stop")
label11 <- tklabel(tt,justify = "center", wraplength = "3i", text=tclvalue(label1))
tkconfigure(label11, textvariable=label1)
tkgrid(label11,columnspan=2)
tkgrid(tklabel(tt,text="    "))

if (opt == 3)
{
    slider1(tt,refresh.code,c("Reg. Smooth. Spline Parameter"),c(borne1),c(borne2),c((borne2-borne1)/300),c(alpha))
  }



labelText7 <- tclVar("Non interactive selection")
label7 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText7))
tkconfigure(label7, textvariable=labelText7)
tkgrid(label7,columnspan=2)

noint1.but <- tkbutton(tt, text="  On/Off  ", command=fnointer);
tkgrid(noint1.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))


labelText6 <- tclVar("Draw spatial contours")
label6 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText6))
tkconfigure(label6, textvariable=labelText6)
tkgrid(label6,columnspan=2)

nocou1.but <- tkbutton(tt, text="  On/Off  ", command=cartfunc);
tkgrid(nocou1.but,columnspan=2)
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


labelText5 <- tclVar("Exit")
label5 <- tklabel(tt,justify = "center", wraplength = "3i",text=tclvalue(labelText5))
tkconfigure(label5, textvariable=labelText5)
tkgrid(label5,columnspan=2)

quit.but <- tkbutton(tt, text="     OK     ", command=quitfunc);
tkgrid(quit.but,columnspan=2)
tkgrid(tklabel(tt,text="    "))

tkwait.variable(fin)
}


####################################################

return(obs)
  }

